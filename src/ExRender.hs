-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module ExRender (exDisp, exDispQ, exRender) where

import Data.Maybe
import Data.List
import qualified Data.Map as M
import Text.PrettyPrint

import Control.Arrow ((&&&))

import Distribution.Text
import Distribution.Package
import Distribution.Version
import Distribution.License
import Distribution.Compiler
import Distribution.System
import Distribution.PackageDescription
import Documentation.Haddock.Parser
import Documentation.Haddock.Types

exWrapWidth = 80 :: Int
exGHCVersion = Version [7, 8, 2] [] -- TODO: solve this hard-coded GHC version assumption

dquoted [] = []
dquoted ('\\':xs) = "\\\\" ++ dquoted xs
dquoted ('"':xs) = "\\\"" ++ dquoted xs
dquoted (x:xs) = x : dquoted xs

softWidth width = build 0 [] where
    build _ ys [] = [reverse ys]
    build 0 [] (w:ws) = build (length w) [w] ws
    build n ys (w:ws) | n' > width = reverse ys : build 0 [] (w:ws)
                      | otherwise = build n' (w : ys) ws
        where
            n' = length w + n

reflow width = vcat . map (text . unwords) . softWidth width . words

-- wrap doc with spaces around
spaces doc | isEmpty doc = empty
           | otherwise = space <> doc <> space

-- wrap with brackets non-empty doc
nbrackets doc | isEmpty doc = empty
              | otherwise = brackets doc

class ExRender a where
    -- |Renders 'a' into a 'Doc' representing some part of exheres
    exDisp :: a → Doc

class ExRenderQ a where
    -- |Renders 'a' into a 'Doc' to be placed within double-quotes inside of exheres
    exDispQ :: a → Doc

instance ExRenderQ String where
    -- TODO: exDispQ = sep . map text . words . dquoted
    exDispQ = text . dquoted

instance ExRender Identifier where exDisp (_, s, _) = text s
instance ExRenderQ Identifier where exDispQ = exDisp

instance ExRenderQ id => ExRenderQ (DocH mod id) where
    exDispQ x = case x of
        DocEmpty → empty
        DocAppend a b → exDispQ a <> exDispQ b
        DocString s → exDispQ s
        DocParagraph a → exDispQ a
        DocIdentifier s → exDispQ s
        DocModule s → exDispQ s
        DocWarning a → exDispQ a
        DocEmphasis a → exDispQ a
        DocMonospaced a → exDispQ a
        DocBold a → exDispQ a
        DocHyperlink (Hyperlink _ (Just s)) → exDispQ s
        DocHyperlink (Hyperlink s Nothing) → exDispQ s
        DocAName s → exDispQ s
        DocProperty s → exDispQ s

instance ExRender LowerBound where
    exDisp (LowerBound v InclusiveBound) = ">=" <> disp v
    exDisp (LowerBound v ExclusiveBound) = ">" <> disp v

instance ExRender UpperBound where
    exDisp (UpperBound v InclusiveBound) = "<=" <> disp v
    exDisp (UpperBound v ExclusiveBound) = "<" <> disp v
    exDisp x = error $ "Unsupported UpperBound: " ++ show x

instance ExRender VersionInterval where
    exDisp (LowerBound v  InclusiveBound,
            UpperBound v' InclusiveBound) | v == v' = brackets ("=" <> disp v)
    exDisp (LowerBound (Version [0] []) InclusiveBound, NoUpperBound) = empty
    exDisp (LowerBound (Version [0] []) InclusiveBound, ub) = brackets (exDisp ub)
    exDisp (lb, NoUpperBound) = brackets (exDisp lb)
    exDisp (lb, ub) = brackets (exDisp lb <> char '&' <> exDisp ub)

instance ExRender VersionRange where
    exDisp vr = case asVersionIntervals vr of
        [vi] → exDisp vi
        _ → error $ "Unsupported version range: " ++ display vr

instance ExRender Dependency where
    exDisp (Dependency n vr) = "dev-haskell/" <> disp n <> exDisp vr

instance ExRender License where
    exDisp (GPL Nothing) = "Unspecified-GPL"
    exDisp (GPL (Just v)) = "GPL-" <> disp v
    exDisp (AGPL (Just v)) = "AGPL-" <> disp v
    exDisp (LGPL Nothing) = "Unspecified-LGPL"
    exDisp (LGPL (Just v)) = "LGPL-" <> disp v
    exDisp (Apache Nothing) = "Unspecified-Apache"
    exDisp (Apache (Just v)) = "Apache-" <> disp v
    exDisp BSD3 = "BSD-3"
    exDisp BSD4 = "BSD-4"
    exDisp MIT = "MIT"
    exDisp (UnknownLicense "BSD2") = "BSD-2"
    exDisp (UnknownLicense "MPL-2") = "MPL-2.0"
    exDisp x = error $ "Unsupported license: " ++ display x

instance ExRender GenericPackageDescription where
    exDisp descr = exheres where
        name = pkgName . package $ packageDescription descr
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

        exLibDeps | libDeps == [] = empty
                  | otherwise = exDepFn "haskell_lib_dependencies" libDeps
            where
                libDeps = filter (not . ignoredDep) (collectLibDeps descr)

        exBinDeps | binDeps == [] = empty
                  | otherwise = exDepFn "haskell_bin_dependencies" binDeps
            where
                binDeps = filter (not . ignoredBinDep) (collectBinDeps descr)

        exTestDeps = case condTestSuites descr of
            [] → empty
            xs → exDepFn "haskell_test_dependencies" testDeps where
                testDeps = filter (not . ignoredTestDep) (collectTestDeps descr)

        exDependencies = vcat [
            "DEPENDENCIES=\"",
            nest 4 exLibDeps,
            nest 4 exTestDeps,
            nest 4 exBinDeps,
            "\""]

        pkgDescr = packageDescription descr

        hasLib = condLibrary descr /= Nothing
        hasBin = condExecutables descr /= []
        hasMods = maybe False (([] /=) . exposedModules . condTreeData) . condLibrary $ descr
        exRequire = "require hackage" <+> nbrackets exParams
            where
                exHasLib = if hasLib then empty else "has_lib=false"
                exHasBin = if hasBin then "has_bin=true" else empty
                exHasOptions | hasMods = empty
                             | otherwise = hsep [
                                 "has_haddock=false",
                                 "has_hscolour=false",
                                 "has_profile=false"
                                 ]
                exParams = spaces $ exHasLib <+> exHasBin <+> exHasOptions

        exheres = vcat [
            "# Copyright 2015 Mykola Orliuk <virkony@gmail.com>",
            "# Distributed under the terms of the GNU General Public License v2",
            "# Generated from " <> disp (package pkgDescr) <> ".cabal",
            "",
            exRequire,
            "",
            exField "SUMMARY" (synopsis pkgDescr),
            exFieldDoc "DESCRIPTION" (exDispQ . toRegular . parseString $ description pkgDescr),
            exField "HOMEPAGE" (homepage pkgDescr),
            "",
            exField "LICENCES" (exRender $ license pkgDescr),
            exField "PLATFORMS" "~amd64",
            "",
            exDependencies,
            "",
            exField "BUGS_TO" "virkony@gmail.com",
            ""
            ]

-- |Collect dependencies from all 'CondTree' nodes of
-- 'GenericPackageDescription' using provided view
collectDeps ∷ (GenericPackageDescription → [CondTree ConfVar [Dependency] a])
              → GenericPackageDescription → [Dependency]
collectDeps view descr = concatMap build (view descr) where
    flags = M.fromList . map (flagName &&& id) $ genPackageFlags descr
    eval (Var (Flag k)) = flagDefault . fromJust $ M.lookup k flags
    eval (Var (OS Linux)) = True -- TODO: solve this hard-coded OS assumption
    eval (Var (OS _)) = False
    eval (Var (Arch X86_64)) = True -- TODO: support other platforms besides amd64
    eval (Var (Arch _)) = False
    eval (Var (Impl GHC vr)) = exGHCVersion `withinRange` vr
    eval (Var (Impl _ _)) = False -- XXX: no support for non-GHC compilers
    eval (Lit f) = f
    eval (CNot e) = not (eval e)
    eval (COr a b) = eval a || eval b
    eval (CAnd a b) = eval a || eval b
    -- eval e = error $ "Unsupported expr " ++ show e

    build t = condTreeConstraints t ++ concatMap buildOptional (condTreeComponents t)

    buildOptional (eval → True, t, _) = build t
    buildOptional (eval → False, _, Just t) = build t
    buildOptional (eval → False, _, Nothing) = []

collectLibDeps = collectDeps (maybeToList . condLibrary)
collectBinDeps = collectDeps (map snd . condExecutables)
collectTestDeps = collectDeps (map snd . condTestSuites)

-- |Render 'a' to a final part of Exheres
exRender ∷ ExRender a ⇒ a → String
exRender = render . exDisp

-- |Render a multi-line meta-field of Exheres if non-empty value
exFieldDoc ∷ String → Doc → Doc
exFieldDoc name value | isEmpty value = empty
                      | otherwise = vcat [text name <> "=\"", value, char '"']

-- |Render a single-line with potential wrap meta-field of Exheres if non-empty value
exField _ "" = empty
exField name x | length singleLine < exWrapWidth = text singleLine
               | otherwise = exFieldDoc name (reflow exWrapWidth (dquoted x))
    where
        singleLine = name ++ "=\"" ++ dquoted x ++ "\""

-- |Sort dependencies according to Exherbo order
sortDeps ∷ [Dependency] → [Dependency]
sortDeps = sortBy (\a b → display a `compare` display b)

-- |Merge sorted deps to through applying the most tighten ones
mergeSortedDeps ∷ [Dependency] → [Dependency]
mergeSortedDeps [] = []
mergeSortedDeps [x] = [x]
mergeSortedDeps (x:y:z) = case (x, y) of
    (Dependency n v, Dependency n' v') | n == n' →
        mergeSortedDeps ((Dependency n (intersectVersionRanges v v')):z)
    _ → x : mergeSortedDeps (y:z)

-- TODO: drop test deps that already in build
-- TODO: use renderStyle instead of manual wrapping
