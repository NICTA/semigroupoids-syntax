{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE Safe #-}

module Data.Functor.Apply.Syntax(
  (>>)
) where

import Data.Functor.Apply(Apply((.>)))

(>>) ::
  Apply f =>
  f a
  -> f b
  -> f b
(>>) =
  (.>)

infixl 1 >>
