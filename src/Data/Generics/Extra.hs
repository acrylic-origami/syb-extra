{-# LANGUAGE Rank2Types, TupleSections, ScopedTypeVariables #-}

module Data.Generics.Extra (
  shallowest,
  common,
  constr_ppr,
  everything_ppr,
  everythingWithContext,
  mkQT,
  extQT,
  everywhereWithContextBut,
  everywhereMBut
) where
import Control.Applicative
import Control.Arrow ( (***) )
import Data.Data ( cast )
import Data.Tree
import Data.Maybe
import Data.Foldable
import Data.Generics hiding (empty)
import Data.ByteString (ByteString)

shallowest :: (Alternative f) => GenericQ (f a) -> GenericQ (f [a])
shallowest op z = pure pure <*> op z <|> foldl (liftA2 (++)) (pure []) (gmapQ (shallowest op) z)

everywhereMBut :: Monad m => (forall a. Data a => a -> Bool) -> (forall a. Data a => a -> m a) -> (forall a. Data a => a -> m a)
everywhereMBut pred f z =
  if pred z -- matches the behavior of everywhereBut
    then return z
    else (gmapM (everywhereMBut pred f) z) >>= f

-- everywhereWithContext :: forall s => (s -> s -> s) (forall a. Data a => a -> (a, s)) -> (forall a. Data a => a -> (a, s))
-- everywhereWithContext cat q s a =
--   let (next_a, s') = q a
--   in gmapQ everywhereWithContext cat q -- this is almost trivial but is actually not: need to understand gfoldl to make something that both transforms and returns an accumulating result. It's definitely doable, just have no idea how.

everywhereWithContext :: forall a s. Data a => (s -> s -> s) -> (forall b. Data b => b -> (s, b)) -> s -> a -> (s, a)
everywhereWithContext cat q s0 =
  gfoldl k (s0,) where
    k :: Data c => (s, c -> d) -> c -> (s, d)
    k (s, f) a =
      let (s', a') = everywhereWithContext cat q s a
      in (cat s' *** f) (q a')

mkQT :: forall a b c. (Data b, Data c) => a -> (b -> (a, b)) -> c -> (a, c)
mkQT a0 f c = case cast c of
  Just b -> (fromJust . cast) <$> f b -- cast is only one deep, so it can only assert that `c ~ b_arg` for the arg type of `f`, but not that `c ~ b_ret` despite the type signature contract that `b_arg ~ b_ret`, which is why we have to do that kludge with a second cast
  Nothing -> (a0, c)

extQT :: (Data b, Data c) => 
  (b -> (a, b)) ->
  (c -> (a, c)) ->
  (b -> (a, b))
extQT f g a = case cast a of
  Just c -> (fromJust . cast) <$> g c
  Nothing -> f a

everywhereWithContextBut :: forall a s. Data a => (s -> s -> s) -> (forall b. Data b => b -> Maybe (s, b)) -> s -> a -> (s, a)
everywhereWithContextBut cat q s0 =
  gfoldl k (s0,) where
    k :: Data c => (s, c -> d) -> c -> (s, d)
    k (s, f) a | Just (s', a') <- q a = (cat s *** f) $ everywhereWithContextBut cat q s' a'
               | otherwise = (s, f a)

constr_ppr :: Data d => d -> String
constr_ppr = everything_ppr (show . toConstr)

everything_ppr :: Data d => GenericQ String -> d -> String
everything_ppr = everything_ppr' 0 where
  everything_ppr' :: Data d => Int -> GenericQ String -> d -> String
  everything_ppr' depth f d' = case asum
    [cast d' :: Maybe ByteString] of -- create a list of cases like so of error cases
    Just _ -> ""
    Nothing -> let lpad = replicate depth ' ' in
      '\n' : lpad ++ f d' ++ concat (gmapQ (everything_ppr' (depth + 1) f) d')

common :: Data d => GenericQ (r, Bool) -> d -> Maybe (Tree r)
common f v =
  let res = f v
      childs = catMaybes $ gmapQ (common f) v in
  if snd res
    then Just $ Node (fst res) childs
    else case childs of
      [] -> Nothing
      [a] -> Just a
      _ -> Just (Node (fst res) childs)