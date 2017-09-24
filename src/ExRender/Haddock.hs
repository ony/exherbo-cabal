-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}

module ExRender.Haddock () where

import ExRender.Base

import Documentation.Haddock.Parser
import Documentation.Haddock.Types

instance ExRender Identifier where exDisp (_, s, _) = text s
instance ExRenderQ Identifier where exDispQ = exDisp

instance ExRenderQ id => ExRenderQ (DocH mod id) where
    exDispQ x = case x of
        DocEmpty -> empty
        DocAppend a b -> exDispQ a <> exDispQ b
        DocString s -> exDispQ s
        DocParagraph a -> exDispQ a
        DocIdentifier s -> exDispQ s
        DocModule s -> exDispQ s
        DocWarning a -> exDispQ a
        DocEmphasis a -> exDispQ a
        DocMonospaced a -> exDispQ a
        DocBold a -> exDispQ a
        DocHyperlink (Hyperlink _ (Just s)) -> exDispQ s
        DocHyperlink (Hyperlink s Nothing) -> exDispQ s
        DocPic _ -> empty -- XXX: no images in description
        DocAName s -> exDispQ s
        DocProperty s -> exDispQ s
        DocExamples _ -> empty -- XXX: examples are filtered out
        _ -> error $ "Unsupported haddock node"
