{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE Safe #-}

module Data.Functor.All.Syntax(
  fail
, fromInteger
, ifThenElse
) where

import qualified Control.Monad as M(Monad(fail))
import qualified Data.Bool as B(Bool(True, False))
import qualified Data.String as S(String)
import qualified Prelude as P(Num, Integer, fromInteger)

fail ::
  M.Monad m =>
  S.String
  -> m a
fail =
  M.fail

fromInteger ::
  P.Num a =>
  P.Integer
  -> a
fromInteger =
  P.fromInteger

ifThenElse ::
  B.Bool
  -> a
  -> a
  -> a
ifThenElse B.True t _ =
  t
ifThenElse B.False _ f =
  f
