-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# LANGUAGE FlexibleInstances #-}

module ExRender where

import Data.Maybe
import Data.List
import Text.PrettyPrint

import Distribution.Text
import Distribution.Package
import Distribution.Version
import Distribution.License
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

instance Text (Ex String) where
    -- TODO: disp (Ex x) = sep . map text . words $ dquoted x
    disp (Ex x) = text (dquoted x)

instance Text (Ex (DocH mod String)) where
    disp (Ex x) = case x of
        DocEmpty -> empty
        DocAppend a b -> disp (Ex a) <> disp (Ex b)
        DocString s -> disp (Ex s)
        DocParagraph a -> disp (Ex a)
        DocIdentifier s -> disp (Ex s)
        DocModule s -> disp (Ex s)
        DocWarning a -> disp (Ex a)
        DocEmphasis a -> disp (Ex a)
        DocMonospaced a -> disp (Ex a)
        DocBold a -> disp (Ex a)
        DocHyperlink (Hyperlink _ (Just s)) -> disp (Ex s)
        DocHyperlink (Hyperlink s Nothing) -> disp (Ex s)
        DocAName s -> disp (Ex s)
        DocProperty s -> disp (Ex s)

newtype Ex a = Ex a

instance Text (Ex Dependency) where
    disp (Ex (Dependency n vr)) = text "dev-haskell/" <> disp n <> disp (Ex vr)

instance Text (Ex VersionRange) where
    disp (Ex vr) = case asVersionIntervals vr of
        [vi] -> disp (Ex vi)
        _ -> error $ "Unsupported version range: " ++ display vr

instance Text (Ex LowerBound) where
    disp (Ex (LowerBound v InclusiveBound)) = text ">=" <> disp v
    disp (Ex (LowerBound v ExclusiveBound)) = text ">" <> disp v

instance Text (Ex UpperBound) where
    disp (Ex (UpperBound v InclusiveBound)) = text "<=" <> disp v
    disp (Ex (UpperBound v ExclusiveBound)) = text "<" <> disp v
    disp (Ex x) = error $ "Unknown " ++ show x

instance Text (Ex VersionInterval) where
    disp (Ex (LowerBound v  InclusiveBound,
              UpperBound v' InclusiveBound)) | v == v' = brackets (text "==" <> disp v)
    disp (Ex (LowerBound (Version [0] []) InclusiveBound, NoUpperBound)) = empty
    disp (Ex (LowerBound (Version [0] []) InclusiveBound, ub)) = brackets (disp (Ex ub))
    disp (Ex (lb, NoUpperBound)) = brackets (disp (Ex lb))
    disp (Ex (lb, ub)) = brackets (disp (Ex lb) <> text "&" <> disp (Ex ub))

instance Text (Ex License) where
    disp (Ex (GPL Nothing)) = text "GPL-2"
    disp (Ex (GPL (Just v))) = text "GPL-" <> disp v
    disp (Ex (AGPL (Just v))) = text "AGPL-" <> disp v
    disp (Ex (LGPL (Just v))) = text "LGPL-" <> disp v
    disp (Ex (Apache (Just v))) = text "Apache-" <> disp v
    disp (Ex BSD3) = text "BSD-3"
    disp (Ex MIT) = text "MIT"
    disp (Ex x) = error $ "Unsupported license: " ++ display x

instance Text (Ex GenericPackageDescription) where
    disp (Ex descr) = exheres where
        [] = condExecutables descr -- no support for apps

        name = pkgName . package $ packageDescription descr
        nameSelf = pkgName . package $ packageDescription descr
        nameBase = fromJust $ simpleParse "base"

        ignoredDep (Dependency n _) | n == nameBase = True
        ignoredDep _ = False
        ignoredTestDep (Dependency n _) | n == nameSelf = True
        ignoredTestDep d = ignoredDep d
        ignoredBinDep (Dependency n _) | n == nameSelf = True
        ignoredBinDep d = ignoredDep d

        a `depOrd` b = display a `compare` display b

        mergeDeps [] = []
        mergeDeps [x] = [x]
        mergeDeps (x:y:z) = case (x, y) of
            (Dependency n v, Dependency n' v') | n == n' ->
                mergeDeps ((Dependency n (intersectVersionRanges v v')):z)
            _ -> x : mergeDeps (y:z)

        exDepFn name deps = vcat [
                text ("$(" ++ name) <> text " \"",
                nest 4 . vcat . map (disp . Ex) $ mergeDeps sortedDeps,
                text "\")"]
            where
                sortedDeps = sortBy depOrd deps

        exLibDeps | libDeps == [] = empty
                  | otherwise = exDepFn "haskell_lib_dependencies" libDeps
            where
                allLibDeps = (maybe [] condTreeConstraints . condLibrary) descr
                libDeps = filter (not . ignoredDep) allLibDeps

        exBinDeps | binDeps == [] = empty
                  | otherwise = exDepFn "haskell_bin_dependencies" binDeps
            where
                allBinDeps = concatMap (condTreeConstraints . snd) $ condExecutables descr
                binDeps = filter (not . ignoredBinDep) allBinDeps

        exTestDeps = case condTestSuites descr of
            [] -> empty
            xs -> exDepFn "haskell_test_dependencies" testDeps where
                allTestDeps = concatMap (condTreeConstraints . snd) xs
                testDeps = filter (not . ignoredTestDep) allTestDeps

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
            exFieldDoc "DESCRIPTION" (disp . Ex . toRegular . parseString $ description pkgDescr),
            exField "HOMEPAGE" (homepage pkgDescr),
            text "",
            exField "LICENCES" (display . Ex $ license pkgDescr),
            exField "PLATFORMS" "~amd64",
            text "",
            exDependencies,
            text "",
            exField "BUGS_TO" "virkony@gmail.com",
            text ""
            ]

-- TODO: drop test deps that already in build
-- TODO: use renderStyle instead of manual wrapping
