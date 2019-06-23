{-# LANGUAGE Rank2Types #-}

module Data.Generics.Extra (
  shallowest,
  common,
  constr_ppr,
  everything_ppr
) where
  import Control.Applicative
  import Data.Tree
  import Data.Maybe
  import Data.Foldable
  import Data.Generics hiding (empty)
  import Data.ByteString (ByteString)
  
  shallowest :: (Alternative f) => GenericQ (f a) -> GenericQ (f [a])
  shallowest op z = pure pure <*> op z <|> foldl (liftA2 (++)) (pure []) (gmapQ (shallowest op) z)
  
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