-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax, ViewPatterns, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module ExRender (ExCabalEnv(..), ExRenderPackage, exDisp, exDispQ, exRenderPkg) where

import Data.Maybe
import Data.List
import Data.Function
import Data.Default

import Distribution.Text
import Distribution.Package
import Distribution.Version
import Distribution.Compiler
import Distribution.System
import Distribution.PackageDescription
import Documentation.Haddock.Parser

import CabalLenses

import ExRender.Base
import ExRender.Haddock ()
import ExRender.License ()
import ExRender.Dependency ()

data ExCabalEnv = ExCabalEnv
    { exGHCVersion ∷ Version
    , exCopyright ∷ Doc
    , exBugsTo ∷ String
    }
    deriving (Show)

instance Default ExCabalEnv where
    def = ExCabalEnv
        { exGHCVersion = case buildCompilerId of
            (CompilerId GHC ver) → ver
            x → error $ "Unsupported compiler " ++ show x
        , exCopyright = vcat
            [ "# Copyright 2015 Mykola Orliuk <virkony@gmail.com>"
            , "# Distributed under the terms of the GNU General Public License v2"
            ]
        , exBugsTo = "virkony@gmail.com"
        }

class ExRenderPackage a where
    type ExPackageEnv a
    exDispPkg ∷ ExPackageEnv a → a → Doc

instance ExRender GenericPackageDescription where
    exDisp = exDispPkg def

instance ExRenderPackage GenericPackageDescription where
    type ExPackageEnv GenericPackageDescription = ExCabalEnv
    exDispPkg env descr = exheres where
        nameSelf = pkgName . package $ packageDescription descr
        ignoredPkgIds = map (fromJust . simpleParse) ["base", "ghc", "ghc-prim"]

        ignoredDep (Dependency n _) | n `elem` ignoredPkgIds = True
        ignoredDep _ = False
        ignoredTestDep (Dependency n _) | n == nameSelf = True
        ignoredTestDep d = ignoredDep d
        ignoredBinDep (Dependency n _) | n == nameSelf = True
        ignoredBinDep d = ignoredDep d

        exDepFn name deps = vcat [
                text ("$(" ++ name) <> " \"",
                nest 4 . vcat . map exDisp . mergeSortedDeps $ sortDeps deps,
                "\")"]

        exLibDeps | null libDeps = empty
                  | otherwise = exDepFn "haskell_lib_dependencies" libDeps
            where
                libDeps = filter (not . ignoredDep) (collectLibDeps env descr)

        exBinDeps | null binDeps = empty
                  | otherwise = exDepFn "haskell_bin_dependencies" binDeps
            where
                binDeps = filter (not . ignoredBinDep) (collectBinDeps env descr)

        exTestDeps = case condTestSuites descr of
            [] → empty
            _ → exDepFn "haskell_test_dependencies" testDeps where
                testDeps = filter (not . ignoredTestDep) (collectTestDeps env descr)

        exDependencies = vcat [
            "DEPENDENCIES=\"",
            nest 4 exLibDeps,
            nest 4 exTestDeps,
            nest 4 exBinDeps,
            "\""]

        pkgDescr = packageDescription descr

        hasLib = isJust $ condLibrary descr
        hasBin = not . null $ condExecutables descr
        hasMods = maybe False (not . null . exposedModules . condTreeData) . condLibrary $ descr
        exRequire = "require hackage" <+> nbrackets exParams
            where
                exHasLib = if hasLib then empty else "has_lib=false"
                exHasBin = if hasBin then "has_bin=true" else empty
                exHasOptions | hasMods || not hasLib = empty
                             | otherwise = hsep [
                                 "has_haddock=false",
                                 "has_hscolour=false",
                                 "has_profile=false"
                                 ]
                exParams = spaces $ exHasLib <+> exHasBin <+> exHasOptions

        exSlot = if not hasBin && hasLib then empty else exField "SLOT" "0"
        exPlatforms = case arch $ fromDefaults descr of
            I386 → "~x86"
            X86_64 → "~amd64"
            x → error $ "Unsupported architecture " ++ show x
        exheres = vcat [
            exCopyright env,
            "# Generated for " <> disp (package pkgDescr),
            "",
            exRequire,
            "",
            exField "SUMMARY" (synopsis pkgDescr),
            exFieldDoc "DESCRIPTION" (exDispQ . toRegular . parseString $ description pkgDescr),
            exField "HOMEPAGE" (homepage pkgDescr),
            "",
            exField "LICENCES" (render . exDisp $ license pkgDescr),
            exSlot,
            exField "PLATFORMS" exPlatforms,
            "",
            exDependencies,
            "",
            exField "BUGS_TO" (exBugsTo env),
            ""
            ]

-- |Collect dependencies from all 'CondTree' nodes of
-- 'GenericPackageDescription' using provided view
collectDeps ∷ ExCabalEnv → (GenericPackageDescription → [CondTree ConfVar [Dependency] a])
              → GenericPackageDescription → [Dependency]
collectDeps env view descr = concatMap build (view descr) where
    condVars = (fromDefaults descr) {
        compilerFlavor = GHC,
        compilerVersion = Just $ exGHCVersion env
        }

    build t = condTreeConstraints t ++ concatMap buildOptional (condTreeComponents t)

    buildOptional (eval condVars → True, t, _) = build t
    buildOptional (_, _, Just t) = build t
    buildOptional (_, _, Nothing) = []

collectLibDeps, collectBinDeps, collectTestDeps ∷ ExCabalEnv → GenericPackageDescription → [Dependency]
collectLibDeps env = collectDeps env (maybeToList . condLibrary)
collectBinDeps env = collectDeps env (map snd . condExecutables)
collectTestDeps env = collectDeps env (map snd . condTestSuites)

-- | Render 'a' to a final Exheres
exRenderPkg ∷ ExRenderPackage a ⇒ ExPackageEnv a → a → String
exRenderPkg env = (++ "\n") . render . exDispPkg env

-- |Sort dependencies according to Exherbo order
sortDeps ∷ [Dependency] → [Dependency]
sortDeps = sortBy (compare `on` display)

-- |Merge sorted deps to through applying the most tighten ones
mergeSortedDeps ∷ [Dependency] → [Dependency]
mergeSortedDeps [] = []
mergeSortedDeps [x] = [x]
mergeSortedDeps (x:y:z) = case (x, y) of
    (Dependency n v, Dependency n' v') | n == n' →
        mergeSortedDeps (Dependency n (intersectVersionRanges v v') : z)
    _ → x : mergeSortedDeps (y:z)

-- TODO: drop test deps that already in build
-- TODO: use renderStyle instead of manual wrapping
