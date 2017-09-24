-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# LANGUAGE FlexibleInstances #-}

module ExRender.Base
    ( module ExRender.Base
    , module Text.PrettyPrint
    ) where

import Text.PrettyPrint

-- | Wrap width soft limit
exWrapWidth :: Int
exWrapWidth = 100

class ExRender a where
    -- | Renders 'a' into a 'Doc' representing some part of exheres
    exDisp :: a -> Doc

class ExRenderQ a where
    -- | Renders 'a' into a 'Doc' to be placed within double-quotes inside of exheres
    exDispQ :: a -> Doc

-- | Double-quoted string for bash
dquoted :: String -> String
dquoted [] = []
dquoted ('\\':xs) = "\\\\" ++ dquoted xs
dquoted ('"':xs) = "\\\"" ++ dquoted xs
dquoted ('`':xs) = "\\`" ++ dquoted xs
dquoted ('$':xs) = "\\$" ++ dquoted xs
dquoted (x:xs) = x : dquoted xs

-- | Split String in lines according to soft limit
softWidth :: Int -> [String] -> [[String]]
softWidth width = build 0 [] where
    build _ ys [] = [reverse ys]
    build 0 [] (w:ws) = build (length w) [w] ws
    build n ys (w:ws) | n' > width = reverse ys : build 0 [] (w:ws)
                      | otherwise = build n' (w : ys) ws
        where
            n' = length w + n

-- | Build a Doc out of String respecting soft width limit
reflow :: Int -> String -> Doc
reflow width = vcat . map (text . unwords) . softWidth width . words

-- | Wrap doc with spaces around if non-empty
spaces :: Doc -> Doc
spaces doc | isEmpty doc = empty
           | otherwise = space <> doc <> space

-- | Wrap with brackets non-empty doc
nbrackets :: Doc -> Doc
nbrackets doc | isEmpty doc = empty
              | otherwise = brackets doc

-- | Render a multi-line meta-field of Exheres if non-empty value
exFieldDoc :: String -> Doc -> Doc
exFieldDoc name value | isEmpty value = empty
                      | otherwise = vcat [text name <> text "=\"", value, char '"']

-- |Render a single-line with potential wrap meta-field of Exheres if non-empty value
exField :: String -> String -> Doc
exField _ "" = empty
exField name x | length singleLine <= exWrapWidth = text singleLine
               | otherwise = exFieldDoc name (reflow exWrapWidth (dquoted x))
    where
        singleLine = name ++ "=\"" ++ dquoted x ++ "\""

instance ExRender String where
    exDisp "" = empty
    exDisp s = text s

instance ExRenderQ String where
    -- TODO: exDispQ = sep . map text . words . dquoted
    exDispQ = text . dquoted
