{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE Safe #-}

module Data.Functor.Point.Syntax(
  return
) where

import Data.Functor.Point(Point(point))

return ::
  Point f =>
  a
  -> f a
return =
  point
