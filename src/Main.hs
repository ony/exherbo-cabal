-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.ByteString.Lazy.Char8 (unpack)

import System.Environment
import Distribution.Text
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Network.HTTP.Client

import ExRender

simpleFetch ∷ String → IO String
simpleFetch url = do
    let settings = defaultManagerSettings
    req ← parseUrl url
    withManager settings $ \man → do
        liftM (unpack . responseBody) $ httpLbs req man

fetchPackageDescription ∷ PackageIdentifier → IO GenericPackageDescription
fetchPackageDescription pkgid = do
    -- http://hackage.haskell.org/package/happstack-server-7.4.4/happstack-server.cabal
    let baseUri = "http://hackage.haskell.org/package/"
        cabalFile = display (packageName pkgid) ++ ".cabal"
        url = baseUri ++ display pkgid ++ "/" ++ cabalFile
    ParseOk _ descr ← liftM parsePackageDescription $ simpleFetch url
    return descr

selftest = do
    descr <- readPackageDescription verbose "exherbo-cabal.cabal"
    putStrLn (exRender descr)

main ∷ IO ()
main = do
    args ← getArgs
    sources ← case args of
        [] → liftM lines getContents
        _ → return args
    forM_ sources $ \source → do
        descr ← case source of
            ('.':_) → readPackageDescription verbose source
            ('/':_) → readPackageDescription verbose source
            _ → fetchPackageDescription (fromJust $ simpleParse source)
        putStrLn (exRender descr)
