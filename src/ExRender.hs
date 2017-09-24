-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module ExRender (ExCabalEnv(..), ExRenderPackage, exDisp, exDispQ, exRenderPkg, exDispCopyright) where

import Data.Maybe
import Data.List
import Data.Function
import Data.Default
import qualified Data.Map as M

import Control.Arrow ((&&&))

import Distribution.Text
import Distribution.Types.CondTree
import Distribution.Package
import Distribution.Version
import Distribution.Compiler
import Distribution.System
import Distribution.PackageDescription
import Documentation.Haddock.Parser

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
        , exCopyright = exDispCopyright "2015" "" (exBugsTo def)
        , exBugsTo = "nobody@localhost"
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

        ignoredPkgIds :: [PackageName]
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
            exField "PLATFORMS" "~amd64",
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
    flags = M.fromList . map (flagName &&& id) $ genPackageFlags descr
    eval (Var (Flag k)) = flagDefault . fromJust $ M.lookup k flags
    eval (Var (OS Linux)) = True -- TODO: solve this hard-coded OS assumption
    eval (Var (OS _)) = False
    eval (Var (Arch X86_64)) = True -- TODO: support other platforms besides amd64
    eval (Var (Arch _)) = False
    eval (Var (Impl GHC vr)) = exGHCVersion env `withinRange` vr
    eval (Var (Impl _ _)) = False -- XXX: no support for non-GHC compilers
    eval (Lit f) = f
    eval (CNot e) = not (eval e)
    eval (COr a b) = eval a || eval b
    eval (CAnd a b) = eval a && eval b
    -- eval e = error $ "Unsupported expr " ++ show e

    build t = condTreeConstraints t ++ concatMap buildOptional (condTreeComponents t)

    buildOptional (CondBranch (eval → True) t _) = build t
    buildOptional (CondBranch _ _ (Just t)) = build t
    buildOptional (CondBranch _ _ Nothing) = []

collectLibDeps, collectBinDeps, collectTestDeps ∷ ExCabalEnv → GenericPackageDescription → [Dependency]
collectLibDeps env = collectDeps env (maybeToList . condLibrary)
collectBinDeps env = collectDeps env (map snd . condExecutables)
collectTestDeps env = collectDeps env (map snd . condTestSuites)

-- | Build copyright header
exDispCopyright ∷ String → String → String → Doc
exDispCopyright year user email = vcat
    [ "# Copyright" <+> text year <+> exDisp user <+> "<" <> text email <> ">"
    , "# Distributed under the terms of the GNU General Public License v2"
    ]

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
