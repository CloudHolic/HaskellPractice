import Data.Monoid hiding (Sum, getSum, Product)
import Prelude hiding (Foldable)

-- rose tree
data Rose a = a :> [Rose a] deriving Show

root :: Rose a -> a
root (x :> xs) = x

children :: Rose a -> [Rose a]
children (x :> xs) = xs

size :: Rose a -> Int
size (x :> xs) = 1 + sum (map size xs)

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> xs) = sum (map leaves xs)

-- sample rose trees
tree = 'x' :> map (flip (:>) []) ['a'..'z']
-- tree = 'x' :> map (\c -> c :> []) ['a'..'z']


-- functor
instance Functor Rose where
    fmap g (x :> xs) = g x :> map (fmap g) xs

tree' = 1 :> map (flip (:>) []) [1..5]


-- monoid
-- newtype Sum a = Sum {getSum :: a} deriving Show
newtype Sum a = Sum a deriving Show
newtype Product a = Product a deriving Show

unSum :: Sum a -> a
unSum (Sum x) = x

unProduct :: Product a -> a
unProduct (Product x) = x

instance Num a => Semigroup (Sum a) where
    Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

instance Num a => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)

instance Num a => Monoid (Product a) where
    mempty = Product 1

-- fmap Sum tree'
-- fmap Product tree'


-- foldable
class Functor f => Foldable f where
    fold :: Monoid m => f m -> m
    foldMap :: Monoid m => (a -> m) -> (f a -> m)
    foldMap g a = fold $ fmap g a

instance Foldable Rose where
    fold (x :> xs) = (h.g) xs `mappend` x
                        where
                            g = map (fold)
                            h = foldr (mappend) mempty
-- fmap Sum tree'
-- fold $ fmap Sum tree' -- Sum 16
-- unSum $ fold $ fmap Sum tree' -- 16