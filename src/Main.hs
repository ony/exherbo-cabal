-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Maybe
import Data.ByteString.Lazy.Char8 (unpack)

import System.Environment
import System.IO

import Distribution.Text
import Distribution.Package
import Distribution.License
import Distribution.Version
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Network.HTTP.Client
import Network.HTTP.Types

import qualified Text.Regex.PCRE.Light.Char8 as R

import ExRender

-- |Fetch content by provided URI
simpleFetch ∷ String → IO String
simpleFetch url = do
    let settings = defaultManagerSettings { managerRetryableException = isTemporary
                                          , managerResponseTimeout = Just 120000000
                                          , managerWrapIOException = (threadDelay 1 >>)
                                          }
        isTemporary (fromException → Just (StatusCodeException e _ _)) = statusCode e `elem` [503]
        isTemporary _ = False
    req ← parseUrl url
    withManager settings $ liftM (unpack . responseBody) . httpLbs req

licenseHints ∷ [(R.Regex, License)]
licenseHints = [
    (R.compile "http://creativecommons\\.org/publicdomain/zero/1\\.0/" [R.multiline], UnknownLicense "CC0"),
    (R.compile "http://www\\.apache\\.org/licenses/LICENSE-2\\.0" [R.multiline], Apache (Just $ Version [2, 0] [])),
    (R.compile "GNU AFFERO GENERAL PUBLIC LICENSE\\s*Version 3," [R.multiline], UnknownLicense "AGPL-3"),
    (R.compile "Mozilla Public License Version 2\\.0$" [R.multiline], UnknownLicense "MPL-2")
    ]

guessLicense ∷ String → IO License
guessLicense text | isJust foundLicense = return $ fromJust foundLicense where
    foundLicense = listToMaybe $ mapMaybe hint licenseHints
    hint (re, answer) = const answer <$> R.match re text []

guessLicense _ = return OtherLicense

hackageBaseUri ∷ String
hackageBaseUri = "http://hackage.haskell.org/package/"

fixLicense ∷ GenericPackageDescription → IO GenericPackageDescription
fixLicense descr | isNothing maybeLicensePath = return descr
                 | license pkgDescr == OtherLicense = catch adjustLicense handler
                 | otherwise = return descr
    where
        pkgDescr = packageDescription descr
        packageUri = hackageBaseUri ++ display (package pkgDescr)

        handler ∷ SomeException → IO GenericPackageDescription
        handler e = do
            hPutStrLn stderr $ "# No license fix for " ++ display (package pkgDescr) ++ ": " ++ show e
            return descr

        -- TODO: proper handling of multiple licenses
        maybeLicensePath = case licenseFiles pkgDescr of
                            [x] → Just x
                            _ → Nothing
        adjustLicense = do
            let licensePath = fromJust $ maybeLicensePath
            licenseContent ← simpleFetch (packageUri ++ "/src/" ++ licensePath)
            license' ← guessLicense licenseContent
            case license' of
                OtherLicense → do
                    hPutStrLn stderr licenseContent
                    return descr
                _ → do
                    let pkgDescr' = pkgDescr { license = license' }
                        descr' = descr { packageDescription = pkgDescr' }
                    return descr'

-- |Fetch and parse cabal file for specific 'PackageIdentifier' using URL build
-- in form of http://hackage.haskell.org/package/<pkgid>/<pkgname>.cabal
fetchPackageDescription ∷ PackageIdentifier → IO GenericPackageDescription
fetchPackageDescription pkgid = do
    let cabalFile = display (packageName pkgid) ++ ".cabal"
        packageUri = hackageBaseUri ++ display pkgid
        url = packageUri ++ "/" ++ cabalFile
    ParseOk _ descr ← liftM parsePackageDescription $ simpleFetch url
    fixLicense descr

main ∷ IO ()
main = do
    args ← getArgs
    case args of
        "-h":_ → putStr helpString
        "--help":_ → putStr helpString
        _ → do
          sources ← case args of
              [] → liftM lines getContents
              _ → return args
          forM_ sources $ \source → do
              hPutStrLn stderr $ "# Processing " ++ show source
              descr ← case source of
                  ('.':_) → readPackageDescription verbose source >>= fixLicense
                  ('/':_) → readPackageDescription verbose source >>= fixLicense
                  (simpleParse → Just pkgId) -> fetchPackageDescription pkgId
                  _ -> error $ "Specified source " ++ show source
                      ++ " neither starts with '.' or '/' (local file)"
                      ++ " nor a valid packageIdentifier (to fetch from hackage)"
              let handler ∷ SomeException → IO ()
                  handler e = hPutStrLn stderr $ "# Failed fetch/generate for " ++ show source ++ ": " ++ show e
              catch (evaluate (exRender descr) >>= putStrLn) handler


helpString :: String
helpString =
  "Generate package description from .cabal files in format of exheres-0 for\n" ++
  "Exherbo Linux.\n" ++
  "\n" ++
  "See https://github.com/ony/exherbo-cabal\n" ++
  "\n" ++
  "Usage: exherbo-cabal [ -h | --help | <ref-to-package> ... ]\n"++
  "  -h | --help        Print this help and exit\n" ++
  "  <ref-to-package> either a package name (mtl) at Hackage with optional\n" ++
  "  version (mtl-2.2.1) or path to local cabal file (./exherbo-cabal.cabal)\n" ++
  "  If no <ref-to-package> provided in args read them from standart input\n" ++
  "\n" ++
  "Examples:\n" ++
  "  > exherbo-cabal mtl-2.2.1\n" ++
  "  > exherbo-cabal mtl transformers\n" ++
  "  > echo yesod-core | exherbo-cabal\n" ++
  "  > exherbo-cabal ./exherbo-cabal.cabal\n" ++
  "  > find /tmp/index -name \\*.cabal | exherbo-cabal\n"
