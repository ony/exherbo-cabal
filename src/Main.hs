-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# LANGUAGE ViewPatterns, LambdaCase, UnicodeSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.DeepSeq
import Data.Maybe
import Data.Monoid
import Data.Default
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Time (getCurrentTime, utctDay, formatTime, defaultTimeLocale)

import Options.Applicative hiding (maybeReader)

import System.IO
import System.FilePath
import System.Directory

import Distribution.Text
import Distribution.Package
import Distribution.License
import Distribution.Version
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Network.HTTP.Client
import Network.HTTP.Types

import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), (</>), empty, text)
import qualified Text.Regex.PCRE.Light.Char8 as R

import ExRender

maybeReader ∷ (String → Maybe a) → ReadM a
maybeReader f = eitherReader $ \case
    (f → Just x) → return x
    arg → Left $ "cannot parse value `" ++ arg ++ "'"

textAuto ∷ Text a ⇒ ReadM a
textAuto = maybeReader simpleParse

-- |Fetch content by provided URI
simpleFetch ∷ String → IO String
simpleFetch url = do
    let settings = defaultManagerSettings { managerRetryableException = isTemporary
                                          , managerResponseTimeout = responseTimeoutMicro 120000000
                                          , managerWrapException = const (threadDelay 1 >>)
                                          }
        isTemporary (fromException → Just (HttpExceptionRequest _ (StatusCodeException e _))) = statusCode (responseStatus e) `elem` [503]
        isTemporary _ = False
    req ← parseUrlThrow url
    manager ← newManager settings
    (unpack . responseBody) <$> httpLbs req manager

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
            let licensePath = fromJust maybeLicensePath
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
    ParseOk _ descr ← parsePackageDescription <$> simpleFetch url
    fixLicense descr

data TargetCabal = TargetCabalFile FilePath
                 | TargetPackage PackageIdentifier
                 | TargetInvalid String
    deriving (Show)

targetParse ∷ String → Either String TargetCabal
targetParse arg = case arg of
    ('.':_) → return $ TargetCabalFile arg
    ('/':_) → return $ TargetCabalFile arg
    (simpleParse → Just pkgId) → return $ TargetPackage pkgId
    _ → Left $ "Specified target " ++ show arg
        ++ " neither starts with '.' or '/' (local cabal file)"
        ++ " nor a valid packageIdentifier (to fetch from hackage)"

targetArguments ∷ Parser [TargetCabal]
targetArguments = some (argument targetReader (metavar "TARGETS..."))
    where targetReader = eitherReader targetParse

data ExCabal =
    ExCabal
        { ghcVersion ∷ Version
        , userName ∷ String
        , userEmail ∷ String
        , destFolder ∷ Maybe FilePath
        , targets ∷ Maybe [TargetCabal]
        }
    deriving (Show)

exCabalParser ∷ Parser ExCabal
exCabalParser = ExCabal
    <$> option textAuto
        ( long "ghc" <> short 'V'
        <> metavar "VERSION"
        <> help "Target VERSION of GHC"
        <> showDefaultWith display
        <> value (exGHCVersion def)
        )
    <*> strOption
        ( long "user-name" <> metavar "NAME"
        <> help "User name for copyright header"
        <> showDefault
        <> value "Mr. Nobody"
        )
    <*> strOption
        (long "user-email" <> metavar "EMAIL"
        <> help "User email for copyright and bug-reports"
        <> showDefault
        <> value (exBugsTo def)
        )
    <*> optional (strOption
        ( long "dest-dir" <> short 'd'
        <> metavar "DIRECTORY"
        <> help ("Generate output into exheres-0 files under DIRECTORY " ++
                 "(usually repo/packages) instead of standart output.")
        ))
    <*> optional targetArguments

main ∷ IO ()
main = do
    let opts = info (helper <*> exCabalParser)
            ( fullDesc
            <> headerDoc (Just helpHeader)
            <> progDescDoc (Just helpDesc)
            <> footerDoc (Just helpFooter)
            )
    params ← execParser opts
    copyrightYear ← formatTime defaultTimeLocale "%Y" . utctDay <$> getCurrentTime
    let env = def
            { exGHCVersion = ghcVersion params
            , exCopyright = exDispCopyright copyrightYear (userName params) (userEmail params)
            , exBugsTo = userEmail params
            }
    targets' ← case targets params of
        Just xs → return xs
        Nothing →
            fmap (map (either TargetInvalid id . targetParse) . lines) getContents

    let output = case destFolder params of
            Nothing → const putStr -- output to stdout
            Just dirpath → \descr exheres → do
                let pkgId = package $ packageDescription descr
                    pnv = display pkgId
                    pn = display (pkgName pkgId)
                    folder = dirpath </> "dev-haskell" </> pn
                    filepath = folder </> (pnv ++ ".exheres-0")
                createDirectoryIfMissing True folder
                writeFile filepath exheres

    let generate source getDescr = do
            let handler ∷ SomeException → IO ()
                handler e = hPutStrLn stderr $ "# Failed fetch/generate for " ++ show source ++ ": " ++ show e
                body = do
                    descr ← getDescr
                    let exheres = exRenderPkg env descr
                    exheres `deepseq` output descr exheres
            hPutStrLn stderr $ "# Processing " ++ show source
            catch body handler

    forM_ targets' $ \case
        TargetInvalid err → hPutStrLn stderr $ "# Invalid target: " ++ err
        TargetCabalFile filepath → generate filepath (readPackageDescription verbose filepath >>= fixLicense)
        TargetPackage pkgId → generate (display pkgId) (fetchPackageDescription pkgId)


helpHeader ∷ Doc
helpHeader = fillSep [
  "Generate package description", "from .cabal files",
  "in format of exheres-0", "for Exherbo Linux"]

helpDesc ∷ Doc
helpDesc = fillSep [
  "Each of TARGETS specify either", "a package name (mtl) at Hackage",
  "with optional", "version (mtl-2.2.1)",
  "or path to local cabal file (./exherbo-cabal.cabal).",
  "If no TARGETS provided in args read them from standart input."]

helpFooter ∷ Doc
helpFooter = vcat ["Examples:", indent 2 $ vcat [
  "> exherbo-cabal mtl-2.2.1",
  "> exherbo-cabal mtl transformers",
  "> echo yesod-core | exherbo-cabal",
  "> exherbo-cabal ./exherbo-cabal.cabal",
  "> find /tmp/index -name \\*.cabal | exherbo-cabal",
  "> find /tmp/index -name \\*.cabal | exherbo-cabal --dest-dir hackage-repo/packages"
  ], mempty, "See https://github.com/ony/exherbo-cabal"]
