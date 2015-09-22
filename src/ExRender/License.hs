-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module ExRender.License () where

import Distribution.Text
import Distribution.License

import ExRender.Base

exKnownLicenses ∷ [String]
exKnownLicenses = ["CC0", "AGPL-3"]

instance ExRender License where
    exDisp (GPL Nothing) = "Unspecified-GPL"
    exDisp (GPL (Just v)) = "GPL-" <> disp v
    exDisp (AGPL (Just v)) = "AGPL-" <> disp v
    exDisp (LGPL Nothing) = "Unspecified-LGPL"
    exDisp (LGPL (Just v)) = "LGPL-" <> disp v
    exDisp (Apache Nothing) = "Unspecified-Apache"
    exDisp (Apache (Just v)) = "Apache-" <> disp v
    exDisp (MPL v) = "MPL-" <> disp v
    exDisp BSD2 = "BSD-2"
    exDisp BSD3 = "BSD-3"
    exDisp BSD4 = "BSD-4"
    exDisp ISC = "ISC"
    exDisp MIT = "MIT"
    exDisp PublicDomain = "public-domain"
    exDisp (UnknownLicense "BSD2") = "BSD-2"
    exDisp (UnknownLicense "MPL-2") = "MPL-2.0"
    exDisp (UnknownLicense x) | x `elem` exKnownLicenses = text x
    exDisp x = error $ "Unsupported license: " ++ display x
