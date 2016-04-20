{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.IfMin
-- Copyright   :  (c) Kevin Velghe
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Kevin Velghe <kevin@paretje.be>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Configure layouts based on the number of windows.
--
-- Based on XMonad.Layout.PerWorkspace by Brent Yorgey, inspired by
-- XMonad.Layout.IfMax.
--
-----------------------------------------------------------------------------

module XMonad.Layout.IfMin
    ( -- * Usage
      -- $usage
      IfMin (..)
    , ifMin
    ) where

import Data.Maybe

import XMonad
import qualified XMonad.StackSet as W

data IfMin l1 l2 a = IfMin Int Bool (l1 a) (l2 a)
    deriving (Show, Read)

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (IfMin l1 l2) a where
    runLayout (W.Workspace i p@(IfMin n _ lt lf) ms) r
        | ws >= n   = do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                         return (wrs, Just $ mkNewIfMinT p mlt')
        | otherwise = do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                         return (wrs, Just $ mkNewIfMinF p mlt')
        where ws = length (W.integrate' ms)

    handleMessage (IfMin n bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ IfMin n bool nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (\nf -> return . Just $ IfMin n bool lt nf)

    description (IfMin _ True  l1 _) = description l1
    description (IfMin _ _     _ l2) = description l2

mkNewIfMinT :: IfMin l1 l2 a -> Maybe (l1 a) -> IfMin l1 l2 a
mkNewIfMinT (IfMin n _ lt lf) mlt' =
    (\lt' -> IfMin n True lt' lf) $ fromMaybe lt mlt'

mkNewIfMinF :: IfMin l1 l2 a -> Maybe (l2 a) -> IfMin l1 l2 a
mkNewIfMinF (IfMin n _ lt lf) mlf' =
    (\lf' -> IfMin n False lt lf') $ fromMaybe lf mlf'

ifMin :: (LayoutClass l1 a, LayoutClass l2 a)
      => Int -> (l1 a) -> (l2 a) -> IfMin l1 l2 a
ifMin n = IfMin n False
