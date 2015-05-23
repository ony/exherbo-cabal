-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}

module Main where

import Control.Monad
import Control.Exception
import Data.Maybe
import Data.ByteString.Lazy.Char8 (unpack)

import System.Environment
import System.IO

import Distribution.Text
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Network.HTTP.Client

import ExRender

-- |Fetch content by provided URI
simpleFetch ∷ String → IO String
simpleFetch url = do
    let settings = defaultManagerSettings
    req ← parseUrl url
    withManager settings $ \man → do
        liftM (unpack . responseBody) $ httpLbs req man

-- |Fetch and parse cabal file for specific 'PackageIdentifier' using URL build
-- in form of http://hackage.haskell.org/package/<pkgid>/<pkgname>.cabal
fetchPackageDescription ∷ PackageIdentifier → IO GenericPackageDescription
fetchPackageDescription pkgid = do
    let baseUri = "http://hackage.haskell.org/package/"
        cabalFile = display (packageName pkgid) ++ ".cabal"
        url = baseUri ++ display pkgid ++ "/" ++ cabalFile
    ParseOk _ descr ← liftM parsePackageDescription $ simpleFetch url
    return descr

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
            (simpleParse → Just pkgId) -> fetchPackageDescription pkgId
            _ -> error $ "Specified source " ++ show source
                ++ " neither starts with '.' or '/' (local file)"
                ++ " nor a valid packageIdentifier (to fetch from hackage)"
        let handler ∷ SomeException → IO ()
            handler e = hPutStrLn stderr $ "# Failed fetch/generate for " ++ show source ++ ": " ++ show e
        catch (evaluate (exRender descr) >>= putStrLn) handler
