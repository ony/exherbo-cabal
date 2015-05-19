-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.List
import Data.ByteString.Lazy.Char8 (unpack)
import Text.PrettyPrint

import System.Environment
import Distribution.Text
import Distribution.Package
import Distribution.Version
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Network.HTTP.Client

simpleFetch :: String -> IO String
simpleFetch url = do
    let settings = defaultManagerSettings
    req <- parseUrl url
    withManager settings $ \man -> do
        liftM (unpack . responseBody) $ httpLbs req man

fetchPackageDescription :: PackageIdentifier -> IO GenericPackageDescription
fetchPackageDescription pkgid = do
    -- http://hackage.haskell.org/package/happstack-server-7.4.4/happstack-server.cabal
    let baseUri = "http://hackage.haskell.org/package/"
        cabalFile = display (packageName pkgid) ++ ".cabal"
        url = baseUri ++ display pkgid ++ "/" ++ cabalFile
    ParseOk _ descr <- liftM parsePackageDescription $ simpleFetch url
    return descr

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
    disp (Ex x) = error $ "Unknown " ++ show x

exDeps descr = render dependencies where
    [] = condExecutables descr -- no support for apps

    name = pkgName . package $ packageDescription descr
    nameSelf = pkgName . package $ packageDescription descr
    nameBase = fromJust $ simpleParse "base"

    ignoredDep (Dependency n _) | n == nameBase = True
    ignoredDep _ = False
    ignoredTestDep (Dependency n _) | n == nameSelf = True
    ignoredTestDep d = ignoredDep d

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
            
    exLibDeps = exDepFn "haskell_lib_dependencies" libDeps where
        allLibDeps = (condTreeConstraints . fromJust . condLibrary) descr
        libDeps = filter (not . ignoredDep) allLibDeps

    exTestDeps = case condTestSuites descr of
        [] -> empty
        xs -> exDepFn "haskell_test_dependencies" testDeps where
            allTestDeps = concatMap (condTreeConstraints . snd) xs
            testDeps = filter (not . ignoredTestDep) allTestDeps

    dependencies = vcat [
        text "DEPENDENCIES=\"",
        nest 4 exLibDeps,
        nest 4 exTestDeps,
        text "\""]

-- TODO: drop test deps that already in build
-- TODO: handle executables

test = do
    -- let pkgid = fromJust $ simpleParse "yesod-core-1.4.9.1"
    let pkgid = fromJust $ simpleParse "happstack-lite-7.3.6"
    print pkgid
    print (display pkgid)
    descr <- fetchPackageDescription pkgid
    putStrLn (exDeps descr)

main :: IO ()
main = do
    args <- getArgs
    sources <- case args of
        [] -> liftM lines getContents
        _ -> return args
    forM_ sources $ \source -> do
        let pkgid = fromJust $ simpleParse source
        descr <- fetchPackageDescription pkgid
        putStrLn (exDeps descr)
