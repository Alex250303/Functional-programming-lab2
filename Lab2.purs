module Lab2 where
 
import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.List (List(..), reverse, length, (:))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex predicate = go 0
  where
  go :: Int -> List a -> Maybe Int
  go n (Cons x xs) | predicate x = Just n
                   | otherwise = go (n + 1) xs
  go _ Nil = Nothing
  
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex predicate xs = go (length(xs) - 1) (reverse(xs))
  where
    go :: Int -> List a -> Maybe Int
    go n (Cons a as) | predicate a = Just n
                     | otherwise = go (n - 1) as
    go _ Nil = Nothing

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip as bs = reverse $ go as bs Nil
  where
  go Nil _ acc = acc
  go _ Nil acc = acc
  go (Cons a as) (Cons b bs) acc = go as bs $ Cons (Tuple a b) acc

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip ab = go ab Nil Nil
  where
  go Nil acca accb = Tuple (reverse acca) (reverse accb)
  go (Cons a as) Nil Nil = go as (Cons (fst a) Nil) (Cons (snd a) Nil)
  go (Cons a as) acca accb = go as (Cons (fst a) acca) (Cons (snd a) accb)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter predicate list = go Nil list
  where 
  go acc Nil = reverse acc
  go acc (x : xs)
    |predicate x = go(x : acc) xs
    |otherwise = go acc xs

tailRecursionFilter :: forall a. (a -> Boolean) -> List a -> List a
tailRecursionFilter _ Nil = Nil
tailRecursionFilter p (Cons x xs) | p x = Cons x $ tailRecursionFilter p xs
                     | otherwise = tailRecursionFilter p xs
    
take :: forall a. Int -> List a -> List a
take _ Nil = Nil
take 0 _ = Nil
take n (Cons x xs) = Cons x $ take (n - 1) xs

tailRecursionTake :: forall a. Int -> List a -> List a
tailRecursionTake = go Nil
  where
  go acc 0 _ = reverse acc
  go acc _ Nil = reverse acc
  go acc n (Cons x xs) = go (Cons x acc) (n - 1) xs

test::Effect Unit
test = do
  logShow $ tailRecursionFilter(_<4) (1 : 3 : 5 : 7 : Nil)
