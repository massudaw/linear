{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Linear.V1
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 2-D Vectors
----------------------------------------------------------------------------
module Linear.V1
  ( V1(..)
  , R1(..)
  ) where

import Control.Applicative
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Functor.Bind
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Arr (Ix(..))
import Linear.Core
import Linear.Metric
import Linear.Epsilon
import Linear.Vector
import Prelude hiding (sum)

-- $setup
-- >>> import Control.Lens

-- | A 2-dimensional vector
--
-- >>> pure 1 :: V1 Int
-- V1 1 1
--
-- >>> V1 1 2 + V1 3 4
-- V1 4 6
--
-- >>> V1 1 2 * V1 3 4
-- V1 3 8
--
-- >>> sum (V1 1 2)
-- 3

data V1 a = V1 !a deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Functor V1 where
  fmap f (V1 a) = V1 (f a)
  {-# INLINE fmap #-}
  a <$ _ = V1 a
  {-# INLINE (<$) #-}

instance Foldable V1 where
  foldMap f (V1 a ) = f a 
  {-# INLINE foldMap #-}

instance Traversable V1 where
  traverse f (V1 a ) = V1 <$> f a 
  {-# INLINE traverse #-}

instance Foldable1 V1 where
  foldMap1 f (V1 a) = f a 
  {-# INLINE foldMap1 #-}

instance Traversable1 V1 where
  traverse1 f (V1 a ) = V1 <$> f a 
  {-# INLINE traverse1 #-}

instance Apply V1 where
  V1 a  <.> V1 d  = V1 (a d) 
  {-@ INLINE (<.>) #-}

instance Applicative V1 where
  pure a = V1  a
  {-# INLINE pure #-}
  V1 a  <*> V1 d  = V1 (a d) 
  {-@ INLINE (<*>) #-}

instance Additive V1 where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Bind V1 where
  V1 a >>- f = V1 a'  where
    V1 a' = f a
  {-# INLINE (>>-) #-}

instance Monad V1 where
  return a = V1 a 
  {-# INLINE return #-}
  V1 a  >>= f = V1 a'  where
    V1 a'  = f a
  {-# INLINE (>>=) #-}

instance Num a => Num (V1 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V1 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Metric V1 where
  dot (V1 a ) (V1 c ) = a * c 
  {-# INLINE dot #-}

-- | A space that distinguishes 2 orthogonal basis vectors '_x' and '_y', but may have more.
class R1 t where
  -- |
  -- >>> V1 1 2 ^._x
  -- 1
  --
  -- >>> V1 1 2 & _x .~ 3
  -- V1 3 2
  --
  -- @
  -- '_x' :: Lens' (t a) a
  -- @
  _x :: Functor f => (a -> f a) -> t a -> f (t a)


instance R1 V1 where
  _x f (V1 a ) = V1 <$> f a
  {-# INLINE _x #-}

instance Core V1 where
  core f = V1 (f _x) 
  {-# INLINE core #-}

instance Distributive V1 where
  distribute f = V1 (fmap (\(V1 x ) -> x) f) 
  {-# INLINE distribute #-}


instance Epsilon a => Epsilon (V1 a) where
  nearZero = nearZero . quadrance
  {-# INLINE nearZero #-}

instance Storable a => Storable (V1 a) where
  sizeOf _ = 1 * sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr (V1 x) = poke ptr' x
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = V1 <$> peek ptr' 
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Ix a => Ix (V1 a) where
  {-# SPECIALISE instance Ix (V1 Int) #-}

  range (V1 l1 ,V1 u1 ) =
    [ V1 i1 | i1 <- range (l1,u1)]
  {-# INLINE range #-}

  unsafeIndex (V1 l1 ,V1 u1 ) (V1 i1 ) =
    unsafeIndex (l1,u1) i1 
  {-# INLINE unsafeIndex #-}

  inRange (V1 l1 ,V1 u1 ) (V1 i1 ) =
    inRange (l1,u1) i1 
  {-# INLINE inRange #-}
