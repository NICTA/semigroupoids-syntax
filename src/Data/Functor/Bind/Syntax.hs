{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE Safe #-}

module Data.Functor.Bind.Syntax(
  (>>=)
) where

import Data.Functor.Bind(Bind((>>-)))

(>>=) ::
  Bind f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) =
  (>>-)

infixl 1 >>=
