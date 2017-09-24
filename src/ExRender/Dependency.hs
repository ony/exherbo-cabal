-- Copyright © 2015 Mykola Orliuk <virkony@gmail.com>
-- Distributed under the terms of the GNU General Public License v2

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ViewPatterns, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module ExRender.Dependency () where

import Distribution.Text
import Distribution.Package
import Distribution.Version

import ExRender.Base

instance ExRender LowerBound where
    exDisp (LowerBound v InclusiveBound) = ">=" <> disp v
    exDisp (LowerBound v ExclusiveBound) = ">" <> disp v

instance ExRender UpperBound where
    exDisp (UpperBound v InclusiveBound) = "<=" <> disp v
    exDisp (UpperBound v ExclusiveBound) = "<" <> disp v
    exDisp x = error $ "Unsupported UpperBound: " ++ show x

-- | Render some of VersionInterval's that can be represented with a single condition and thus suitable for using in disjunction list.
--
-- >>> map maybeExVersion $ asVersionIntervals (fromJust $ simpleParse ">=1.0 || ==0.1.*" :: VersionRange)
-- [Just =0.1*,Just >=1.0]
maybeExVersion :: VersionInterval -> Maybe Doc
maybeExVersion = \case
    -- >=x && <=x
    (LowerBound a InclusiveBound, UpperBound b InclusiveBound)
        | a == b -> Just $ char '=' <> disp a

    -- <x, <=x
    (LowerBound (Version [0] []) InclusiveBound, ub) -> Just $ exDisp ub

    -- >=x, >x
    (lb, NoUpperBound) -> Just $ exDisp lb

    (LowerBound (Version [] _) _, _) -> Nothing
    (_, UpperBound (Version [] _) _) -> Nothing
    -- >=x.y && <x.y'
    (LowerBound v@(Version a []) InclusiveBound, UpperBound (Version b []) ExclusiveBound)
        | init a == init b && succ (last a) == last b ->
            Just $ char '=' <> disp v <> char '*'

    (LowerBound (Version [_] _) _, _) -> Nothing
    -- >=x.y.z && <x.y'
    (LowerBound v@(Version a []) InclusiveBound, UpperBound (Version b []) ExclusiveBound)
        | init a' == init b && succ (last a') == last b ->
                Just $ text "~>" <> disp v
            where a' = init a

    _ -> Nothing

-- | Transform VersionInterval in a sequence of disjunctions
--
-- >>> map exVersions $ asVersionIntervals (fromJust $ simpleParse ">=1.0" :: VersionRange)
-- [[>=1.0]]
-- >>> map exVersions $ asVersionIntervals (fromJust $ simpleParse ">=1.0 && <1.3" :: VersionRange)
-- [[=1.0*,=1.1*,=1.2*]]
-- >>> map exVersions $ asVersionIntervals (fromJust $ simpleParse ">=1.0 && <=1.3" :: VersionRange)
-- [[=1.0*,=1.1*,=1.2*,=1.3]]
-- >>> map exVersions $ asVersionIntervals (fromJust $ simpleParse ">=1 && <=1.3" :: VersionRange)
-- [[=1,=1.0*,=1.1*,=1.2*,=1.3]]
-- >>> map exVersions $ asVersionIntervals (fromJust $ simpleParse ">=1 && <=1.0.3" :: VersionRange)
-- [[=1,=1.0,=1.0.0*,=1.0.1*,=1.0.2*,=1.0.3]]
exVersions :: VersionInterval -> [Doc]
exVersions = \case
    (maybeExVersion -> Just x) -> [x]

    -- ... && <=x.b
    (lb, UpperBound v InclusiveBound) ->
        exVersions (lb, UpperBound v ExclusiveBound) ++ [char '=' <> disp v]

    -- >=x.a && <x.b
    (LowerBound va@(Version a _) InclusiveBound, ub@(UpperBound (Version b _) ExclusiveBound))
        | init a == init b -> do
            c <- [init a ++ [i] | i <- [last a .. last b - 1]]
            return $ char '=' <> disp (Version c []) <> char '*'
        | length a < length b ->
            char '=' <> disp va : exVersions (LowerBound (Version (a ++ [0]) []) InclusiveBound, ub)
    _ -> []

instance ExRender VersionInterval where
    exDisp (LowerBound (Version [0] []) InclusiveBound, NoUpperBound) = empty
    exDisp (maybeExVersion -> Just exVi) = exVi
    exDisp (lb, ub) = exDisp lb <> char '&' <> exDisp ub

instance ExRender VersionRange where
    exDisp vr = case asVersionIntervals vr of
        [vi] -> nbrackets $ exDisp vi
        (concatMap exVersions -> exVis) | not $ null exVis -> nbrackets . hcat $ punctuate (char '|') exVis
        _ -> error $ "Unsupported version range: " ++ display vr

instance ExRender Dependency where
    exDisp (Dependency n vr) = "dev-haskell/" <> disp n <> exDisp vr

-- $setup
--
-- >>> import Data.Maybe
--
-- doctest examples:
--
-- >>> exDisp (fromJust $ simpleParse ">=1.0 && <1.3" :: VersionRange)
-- [>=1.0&<1.3]
-- >>> exDisp (fromJust $ simpleParse ">=1.1 && <2" :: VersionRange)
-- [~>1.1]
-- >>> exDisp (fromJust $ simpleParse "==1.* || ==3.*" :: VersionRange)
-- [=1*|=3*]
-- >>> exDisp (fromJust $ simpleParse "==1.1.* || ==1.0.* || ==0.11.*" :: VersionRange)
-- [=0.11*|=1.0*|=1.1*]
