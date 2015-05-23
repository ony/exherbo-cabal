-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}
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
import Distribution.PackageDescription
import Documentation.Haddock.Parser
import Documentation.Haddock.Types

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
    exDisp (LowerBound v InclusiveBound) = text ">=" <> disp v
    exDisp (LowerBound v ExclusiveBound) = text ">" <> disp v

instance ExRender UpperBound where
    exDisp (UpperBound v InclusiveBound) = text "<=" <> disp v
    exDisp (UpperBound v ExclusiveBound) = text "<" <> disp v
    exDisp x = error $ "Unsupported UpperBound: " ++ show x

instance ExRender VersionInterval where
    exDisp (LowerBound v  InclusiveBound,
            UpperBound v' InclusiveBound) | v == v' = brackets (text "=" <> disp v)
    exDisp (LowerBound (Version [0] []) InclusiveBound, NoUpperBound) = empty
    exDisp (LowerBound (Version [0] []) InclusiveBound, ub) = brackets (exDisp ub)
    exDisp (lb, NoUpperBound) = brackets (exDisp lb)
    exDisp (lb, ub) = brackets (exDisp lb <> char '&' <> exDisp ub)

instance ExRender VersionRange where
    exDisp vr = case asVersionIntervals vr of
        [vi] → exDisp vi
        _ → error $ "Unsupported version range: " ++ display vr

instance ExRender Dependency where
    exDisp (Dependency n vr) = text "dev-haskell/" <> disp n <> exDisp vr

instance ExRender License where
    exDisp (GPL Nothing) = text "GPL-2"
    exDisp (GPL (Just v)) = text "GPL-" <> disp v
    exDisp (AGPL (Just v)) = text "AGPL-" <> disp v
    exDisp (LGPL Nothing) = text "LGPL-2.1"
    exDisp (LGPL (Just v)) = text "LGPL-" <> disp v
    exDisp (Apache (Just v)) = text "Apache-" <> disp v
    exDisp BSD3 = text "BSD-3"
    exDisp MIT = text "MIT"
    exDisp x = error $ "Unsupported license: " ++ display x

instance ExRender GenericPackageDescription where
    exDisp descr = exheres where
        [] = condExecutables descr -- no support for apps

        name = pkgName . package $ packageDescription descr
        nameSelf = pkgName . package $ packageDescription descr
        ignoredPkgIds = map (fromJust . simpleParse) ["base", "ghc", "ghc-prim"]

        ignoredDep (Dependency n _) | n `elem` ignoredPkgIds = True
        ignoredDep _ = False
        ignoredTestDep (Dependency n _) | n == nameSelf = True
        ignoredTestDep d = ignoredDep d
        ignoredBinDep (Dependency n _) | n == nameSelf = True
        ignoredBinDep d = ignoredDep d

        a `depOrd` b = display a `compare` display b

        mergeDeps [] = []
        mergeDeps [x] = [x]
        mergeDeps (x:y:z) = case (x, y) of
            (Dependency n v, Dependency n' v') | n == n' →
                mergeDeps ((Dependency n (intersectVersionRanges v v')):z)
            _ → x : mergeDeps (y:z)

        exDepFn name deps = vcat [
                text ("$(" ++ name) <> text " \"",
                nest 4 . vcat . map exDisp $ mergeDeps sortedDeps,
                text "\")"]
            where
                sortedDeps = sortBy depOrd deps

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
            text "DEPENDENCIES=\"",
            nest 4 exLibDeps,
            nest 4 exTestDeps,
            nest 4 exBinDeps,
            text "\""]

        pkgDescr = packageDescription descr

        wrapWidth = 80

        exFieldDoc name d | isEmpty d = empty
        exFieldDoc name d = vcat [text name <> text "=\"", d, char '"']

        exField _ "" = empty
        exField name x | length singleLine < wrapWidth = text singleLine
                       | otherwise = multiLineDoc
            where
                singleLine = name ++ "=\"" ++ dquoted x ++ "\""
                multiLineDoc = vcat [
                    text name <> text "=\"",
                    reflow wrapWidth (dquoted x),
                    char '"'
                    ]
        hasLib = condLibrary descr /= Nothing
        hasBin = condExecutables descr /= []
        hasMods = maybe False (([] /=) . exposedModules . condTreeData) . condLibrary $ descr
        exRequire = text "require hackage" <+> nbrackets exParams
            where
                exHasLib = if hasLib then empty else text "has_lib=false"
                exHasBin = if hasBin then text "has_bin=true" else empty
                exHasOptions | hasMods = empty
                             | otherwise = hsep [
                                 text "has_haddock=false",
                                 text "has_hscolour=false",
                                 text "has_profile=false"
                                 ]
                exParams = spaces $ exHasLib <+> exHasBin <+> exHasOptions

        exheres = vcat [
            text "# Copyright 2015 Mykola Orliuk <virkony@gmail.com>",
            text "# Distributed under the terms of the GNU General Public License v2",
            text "# Generated from " <> disp (package pkgDescr) <> text ".cabal",
            text "",
            exRequire,
            text "",
            exField "SUMMARY" (synopsis pkgDescr),
            exFieldDoc "DESCRIPTION" (exDispQ . toRegular . parseString $ description pkgDescr),
            exField "HOMEPAGE" (homepage pkgDescr),
            text "",
            exField "LICENCES" (exRender $ license pkgDescr),
            exField "PLATFORMS" "~amd64",
            text "",
            exDependencies,
            text "",
            exField "BUGS_TO" "virkony@gmail.com",
            text ""
            ]

collectDeps ∷ (GenericPackageDescription → [CondTree ConfVar [Dependency] a])
              → GenericPackageDescription → [Dependency]
collectDeps view descr = concatMap build (view descr) where
    flags = M.fromList . map (flagName &&& id) $ genPackageFlags descr
    eval (Var (Flag k)) = flagDefault . fromJust $ M.lookup k flags
    eval (CNot e) = not (eval e)
    eval (Var (Impl GHC vr)) = Version [7, 8, 2] [] `withinRange` vr -- TODO: solve this hard-coded assumption
    eval e = error $ "Unsupported expr " ++ show e

    build t = condTreeConstraints t ++ concatMap buildOptional (condTreeComponents t)

    buildOptional (eval → True, t, _) = build t
    buildOptional (eval → False, _, Just t) = build t
    buildOptional (eval → False, _, Nothing) = []

collectLibDeps = collectDeps (maybeToList . condLibrary)
collectBinDeps = collectDeps (map snd . condExecutables)
collectTestDeps = collectDeps (map snd . condTestSuites)

-- TODO: drop test deps that already in build
-- TODO: use renderStyle instead of manual wrapping

-- |Render 'a' to a final part of Exheres
exRender ∷ ExRender a ⇒ a → String
exRender = render . exDisp
