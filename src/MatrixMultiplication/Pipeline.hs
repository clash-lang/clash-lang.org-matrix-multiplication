-- The Apply instance for DelayedFoldTypeGen nests type families:
{-# LANGUAGE UndecidableInstances #-}

{- | Pipelining the dot product with delayed signals ('DSignal') and dependently
typed folds ('dfold'). Besides the hand-rolled pipeline of the blog post,
this module provides two general purpose pipelined folds: 'foldrp' and
'foldrpp'.
-}
module MatrixMultiplication.Pipeline where

import Clash.Prelude

import Data.Singletons (Apply, Proxy (..), TyFun)

import qualified Clash.Signal.Delayed.Bundle as DBundle

-- * Non-pipelined dot product on signals

-- | Multiply-add on signals: @c + a * b@
multiplyAdd ::
  (Num a) =>
  Signal System (a, a) ->
  Signal System a ->
  Signal System a
multiplyAdd ab c = c + a * b
 where
  (a, b) = unbundle ab

{- | Dot product on signals. Because 'multiplyAdd' is of the form @a -> b -> b@,
this is a simple 'foldr'.
-}
dot ::
  forall n a.
  (KnownNat n, Num a) =>
  Signal System (Vec n a) ->
  Signal System (Vec n a) ->
  Signal System a
dot a b = foldr multiplyAdd (pure 0) ab
 where
  -- <$> is another notation for fmap. fmapping zip over a signal leaves
  -- a signal of 1-argument functions. To apply an argument within the
  -- signal use we use <*>. This is a common pattern in Clash.
  ab :: Vec n (Signal System (a, a))
  ab = unbundle (zip <$> a <*> b)

-- * Pipelined dot product, by hand

{- | Multiply-add followed by a register. 'delayed' inserts enough registers to
match its type context: in this case a single one, as @(n + 1) - n ~ 1@.
-}
dMultiplyAdd ::
  (HiddenClockResetEnable System, Num a, NFDataX a) =>
  DSignal System n (a, a) ->
  DSignal System n a ->
  DSignal System (n + 1) a
dMultiplyAdd ab c = delayed (repeat 0) result
 where
  result = c + a * b
  (a, b) = DBundle.unbundle ab

{- | Type level function for 'dfold': the type of a multiply-add pipeline that
started with a delay @n@ and has been folded @l@ times. We don't care about a
runtime representation, so there is no actual implementation.
-}
data MultAddFoldedTypeGen (n :: Nat) (a :: Type) (f :: TyFun Nat Type) :: Type

{- | As 'dMultiplyAdd' incurs a delay of 1 and the pipeline starts with a delay
@n@, the delay after folding @l@ times is @n + l@.
-}
type instance Apply (MultAddFoldedTypeGen n a) l = DSignal System (n + l) a

-- | Pipelined version of 'dot', built with 'dfold'
dotfDfold ::
  forall d n a.
  (HiddenClockResetEnable System, KnownNat n, Num a, NFDataX a) =>
  DSignal System d (Vec n a) ->
  DSignal System d (Vec n a) ->
  DSignal System (d + n) a
dotfDfold a b =
  dfold
    -- Type level function to supply type of /l/th folding
    (Proxy @(MultAddFoldedTypeGen d a))
    -- Chain function:
    go
    -- Start value of pipeline:
    (pure 0)
    -- Every processing element gets an external input:
    (DBundle.unbundle (zip <$> a <*> b))
 where
  go ::
    -- \| Number of times folded already
    SNat l ->
    -- \| Tuple to be multiplied
    DSignal System d (a, a) ->
    -- \| Input from previous element in pipeline
    DSignal System (d + l) a ->
    DSignal System (d + l + 1) a
  go SNat ab c = dMultiplyAdd (delayed (repeat (0, 0)) ab) c

-- * Pipelined folds

{- | Type level function for 'dfold': a signal of type @b@ that started with a
delay of @startDelay@ and gained @procDelay@ delay for every of the @l@ folds.
-}
data
  DelayedFoldTypeGen
    (dom :: Domain)
    (b :: Type)
    (startDelay :: Nat)
    (procDelay :: Nat)
    (f :: TyFun Nat Type) ::
    Type

type instance
  Apply (DelayedFoldTypeGen dom b startDelay procDelay) l =
    DSignal dom (startDelay + (l * procDelay)) b

{- | Pipelined foldr. Function itself is not pipelined, but a single register
will be added after it.
-}
foldrp ::
  forall dom a b startDelay n.
  (HiddenClockResetEnable dom, KnownNat n, NFDataX a, NFDataX b) =>
  -- | f
  (a -> b -> b) ->
  -- | Default output
  b ->
  -- | Default input
  a ->
  -- | Vector to fold over
  DSignal dom startDelay (Vec n a) ->
  -- | Start value
  DSignal dom startDelay b ->
  DSignal dom (startDelay + n) b
foldrp f bDefault aDefault = foldrpp go aDefault
 where
  go :: DSignal dom l a -> DSignal dom l b -> DSignal dom (l + 1) b
  go a b = delayed (repeat bDefault) (f <$> a <*> b)

-- | Pipelined foldr. Function itself might be pipelined.
foldrpp ::
  forall dom a b procDelay startDelay n.
  (HiddenClockResetEnable dom, KnownNat n, KnownNat procDelay, NFDataX a) =>
  -- | Possibly pipelined function
  (forall l. DSignal dom l a -> DSignal dom l b -> DSignal dom (l + procDelay) b) ->
  -- | Default input
  a ->
  -- | Vector to fold over
  DSignal dom startDelay (Vec n a) ->
  -- | Start value
  DSignal dom startDelay b ->
  DSignal dom (startDelay + (n * procDelay)) b
foldrpp f aDefault as start =
  dfold
    (Proxy @(DelayedFoldTypeGen dom b startDelay procDelay))
    go
    start
    (DBundle.unbundle as)
 where
  go ::
    SNat l ->
    DSignal dom startDelay a ->
    DSignal dom (startDelay + (l * procDelay)) b ->
    DSignal dom (startDelay + ((l + 1) * procDelay)) b
  go SNat a b = f (delayed (repeat aDefault) a) b

-- * Pipelined dot product, using pipelined folds

-- | Pipelined version of 'dot'
dotf ::
  (HiddenClockResetEnable System, KnownNat n, Num a, NFDataX a) =>
  DSignal System d (Vec n a) ->
  DSignal System d (Vec n a) ->
  DSignal System (d + n) a
dotf a b =
  foldrp
    -- Function:
    (\(x, y) acc -> x * y + acc)
    -- Defaults for output/input of function:
    0
    (0, 0)
    -- Vector to fold over:
    (zip <$> a <*> b)
    -- Start value:
    (pure 0)

{- | Pipelined version of 'dot' that processes (and produces) 'Maybe' values.
Note that we use 'undefined' as an equivalent of @xxxx@ in VHDL/Verilog: in
Clash, 'undefined' is an 'XException' that is only raised if the value is
actually used.
-}
dotfm ::
  (HiddenClockResetEnable System, KnownNat n, Num a, NFDataX a) =>
  DSignal System d (Maybe (Vec n a, Vec n a)) ->
  DSignal System (d + n) (Maybe a)
dotfm ab =
  foldrp
    -- Function:
    multAdd
    -- Defaults for output/input of function:
    Nothing
    undefined
    -- Vector to fold over:
    (uncurry zip . fromJustX <$> ab)
    -- Start value; Nothing on no input, Just 0 on input:
    ((const 0 <$>) <$> ab)
 where
  multAdd _ Nothing = Nothing
  multAdd (a, b) (Just c) = Just (a * b + c)
