{-# Language  InstanceSigs #-}
{-# Language  TypeOperators #-}
{-# Language  MultiParamTypeClasses #-}
{-# Language  FunctionalDependencies #-}
{-# Language  UndecidableInstances #-} -- Added just for the recursive definition

module LoebSheet
( SheetElem(..)
, LoebSheet
, eval
) where


import Control.Monad   (join)
import Control.Compose ((:.)(..))


-- | A data type to abstract over how one has to work with loeb to make things
-- work the way one would want them to.
data SheetElem ix a
    = Item a
    | At ix
    | Fn1 (a -> a) (SheetElem ix a)
    | Fn2 (a -> a -> a) (SheetElem ix a) (SheetElem ix a)
    | Fn3 (a -> a -> a -> a) (SheetElem ix a) (SheetElem ix a) (SheetElem ix a)
    | Fn4 (a -> a -> a -> a -> a) (SheetElem ix a) (SheetElem ix a) (SheetElem ix a) (SheetElem ix a)
    | Fn5 (a -> a -> a -> a -> a -> a) (SheetElem ix a) (SheetElem ix a) (SheetElem ix a) (SheetElem ix a) (SheetElem ix a)
    | Fn6 (a -> a -> a -> a -> a -> a -> a) (SheetElem ix a) (SheetElem ix a) (SheetElem ix a) (SheetElem ix a) (SheetElem ix a) (SheetElem ix a)

-- | Num definition for SheetElem to make composing 2 numbers inside a sheet
-- much easier.
instance (Num a) => Num (SheetElem ix a) where
    (+) = Fn2 (+)
    (-) = Fn2 (-)
    (*) = Fn2 (*)
    negate = Fn1 negate
    abs = Fn1 abs
    signum = Fn1 signum
    fromInteger = Item . fromInteger

-- | Same as Num.
instance (Fractional a) => Fractional (SheetElem ix a) where
    (/) = Fn2 (/)
    recip = Fn1 recip
    fromRational = Item . fromRational


-- | Typeclass that ensures that an applicative f along with the index
-- can be used in creating a LoebSheet
class (Applicative f) => LoebSheet index f | f -> index where
    sheetAt :: f a -> index -> Maybe a


-- | A simple instance of LoebSheet for []
instance LoebSheet Int [] where
    sheetAt []     _ = Nothing
    sheetAt (x:_)  0 = Just x
    sheetAt (_:xs) n = xs `sheetAt` (n-1)
    

-- | Recursive instance of LoebSheet that allows us to compose multiple 
-- LoebSheets together. Also allows us to make nD sheets by composing the
-- same LoebSheet on itself. E.g. pythagorasTringle in Examples
instance (LoebSheet index1 f, LoebSheet index2 g) 
         => LoebSheet (index1, index2) (f :. g) where
    sheetAt :: (f :. g) a -> (index1, index2) -> Maybe a
    (O x) `sheetAt` (n1, n2)
        = join $ fmap (`sheetAt` n2) (x `sheetAt` n1)


-- | Evaluates the elements in LoebSheet into values.
eval :: (LoebSheet index f)
     => f (SheetElem index a) -> f (Maybe a)
eval = loeb . fmap translate


-- | The meat of the whole thing. "Magical" function loeb.
loeb :: Functor f
     => f (f a -> a) -> f a
loeb x = go
  where
    go = fmap ($ go) x


-- | Translates a SheetElem into the form required by loeb.
translate :: (LoebSheet index f)
          => SheetElem index a -> (f (Maybe a) -> Maybe a)
translate (Item x) table
    = Just x
translate (At n) table
    = join $ table `sheetAt` n
translate (Fn1 f x) table
    = f <$> translate x table
translate (Fn2 f x y) table
    = f <$> translate x table <*> translate y table
translate (Fn3 f x y z) table
    = f <$> translate x table <*> translate y table <*> translate z table
translate (Fn4 f x1 x2 x3 x4) table
    = f <$> translate x1 table <*> translate x2 table <*> translate x3 table
    <*> translate x4 table
translate (Fn5 f x1 x2 x3 x4 x5) table
    = f <$> translate x1 table <*> translate x2 table <*> translate x3 table
    <*> translate x4 table <*> translate x5 table
translate (Fn6 f x1 x2 x3 x4 x5 x6) table
    = f <$> translate x1 table <*> translate x2 table <*> translate x3 table
    <*> translate x4 table <*> translate x5 table <*> translate x6 table

