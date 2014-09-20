{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE Safe #-}

module Data.Functor.Point(
  Point(point) 
) where

import Control.Applicative(Applicative(pure), ZipList, Const, WrappedMonad, WrappedArrow)
import Control.Arrow(Arrow)
import Control.Comonad(Cokleisli)
import Control.Comonad.Trans.Env(EnvT(EnvT))
import Control.Comonad.Trans.Identity(IdentityT)
import Control.Comonad.Trans.Store(StoreT)
import Control.Comonad.Trans.Traced(TracedT)
import Control.Monad(Monad)
import Control.Monad.Trans.Cont(ContT)
import Control.Monad.Trans.List(ListT)
import Control.Monad.Trans.Maybe(MaybeT)
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Trans.RWS.Lazy as RWSL(RWST)
import Control.Monad.Trans.RWS.Strict as RWSS(RWST)
import Control.Monad.Trans.State.Lazy as LS(StateT)
import Control.Monad.Trans.State.Strict as SS(StateT)
import Control.Monad.Trans.Writer.Lazy as LW(WriterT)
import Control.Monad.Trans.Writer.Strict as SW(WriterT)
import Data.Either(Either)
import Data.Functor.Apply(Apply, MaybeApply, WrappedApplicative)
import Data.Functor.Bind(Bind)
import Data.Functor.Compose(Compose)
import Data.Functor.Identity(Identity)
import Data.Functor.Product(Product)
import Data.List.NonEmpty(NonEmpty)
import Data.Maybe(Maybe)
import Data.Monoid(Monoid(mempty))
import Data.Semigroup(Semigroup, Option)
import Data.Semigroupoid.Static(Static)
import Data.Sequence(Seq)
import Data.Tree(Tree)
import System.IO(IO)

class Apply f => Point f where
  point ::
    a
    -> f a

instance Point [] where
  point =
    pure

instance Point IO where
  point =
    pure

instance Point ZipList where
  point =
    pure

instance Point Maybe where
  point =
    pure

instance Point Identity where
  point =
    pure

instance Point Tree where
  point =
    pure

instance Point Seq where
  point =
    pure

instance Point Option where
  point =
    pure

instance Point NonEmpty where
  point =
    pure

instance Point ((->) m) where
  point =
    pure

instance Point (Either a) where
  point =
    pure

instance (Semigroup m, Monoid m) => Point ((,) m) where
  point =
    pure

instance (Semigroup m, Monoid m) => Point (Const m) where
  point =
    pure

instance Monad f => Point (WrappedMonad f) where
  point =
    pure

instance (Apply f, Applicative f) => Point (IdentityT f) where
  point =
    pure

instance (Bind f, Monad f) => Point (MaybeT f) where
  point =
    pure

instance (Apply f, Applicative f) => Point (ListT f) where
  point =
    pure

instance Point f => Point (MaybeApply f) where
  point =
    pure

instance (Apply f, Applicative f) => Point (WrappedApplicative f) where
  point =
    pure

instance Arrow a => Point (WrappedArrow a b) where
  point =
    pure

instance (Apply f, Applicative f) => Point (TracedT t f) where
  point =
    pure

instance (Apply f, Applicative f, Semigroup m, Monoid m) => Point (StoreT m f) where
  point =
    pure

instance (Apply f, Applicative f, Semigroup m, Monoid m) => Point (EnvT m f) where
  point a =
    EnvT mempty (pure a) 

instance Point (Cokleisli f a) where
  point =
    pure

instance (Applicative f, Apply f, Applicative g, Apply g) => Point (Product f g) where
  point =
    pure

instance (Applicative f, Apply f, Applicative g, Apply g) => Point (Compose f g) where
  point =
    pure

instance (Applicative f, Apply f, Semigroup w, Monoid w) => Point (SW.WriterT w f) where
  point =
    pure

instance (Applicative f, Apply f, Semigroup w, Monoid w) => Point (LW.WriterT w f) where
  point =
    pure

instance (Bind f, Monad f) => Point (SS.StateT e f) where
  point =
    pure

instance (Bind f, Monad f) => Point (LS.StateT e f) where
  point =
    pure

instance (Applicative f, Apply f) => Point (ReaderT r f) where
  point =
    pure

instance Point (ContT r f) where
  point =
    pure

instance (Apply f, Applicative f) => Point (Static f a) where
  point =
    pure

instance (Bind f, Monad f, Semigroup w, Monoid w) => Point (RWSS.RWST r w s f) where
  point =
    pure

instance (Bind f, Monad f, Semigroup w, Monoid w) => Point (RWSL.RWST r w s f) where
  point =
    pure
