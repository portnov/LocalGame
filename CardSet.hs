
module CardSet where

import Prelude hiding (null, elem)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Cards

data CardSet = CardSet {
    csJokers :: [CardColor],
    bySuit :: M.Map Suit (S.Set CardValue),
    byValue :: M.Map CardValue [Suit] }
  deriving (Eq)

empty :: CardSet
empty = CardSet [] M.empty M.empty

null :: CardSet -> Bool
null cs = L.null (csJokers cs) && M.null (bySuit cs) && M.null (byValue cs)

toList :: CardSet -> [Card]
toList cs = concatMap cards (M.assocs $ bySuit cs) ++ map Joker (csJokers cs)
  where
    cards (suit, vals) = [Card suit val | val <- S.toList vals]

instance Show CardSet where
  show cs = unwords $ map show $ toList cs

insert :: Card -> CardSet -> CardSet
insert (Joker color) cs = cs { csJokers = color : csJokers cs }
insert (Card suit val) cs = cs {
                                bySuit = M.insertWith S.union suit (S.singleton val) (bySuit cs),
                                byValue = M.insertWith (++) val [suit] (byValue cs)
                               }

delete :: Card -> CardSet -> CardSet
delete (Joker color) cs = cs { csJokers = L.delete color (csJokers cs) }
delete (Card suit val) cs = cs {
                                bySuit = M.update delSet suit (bySuit cs),
                                byValue = M.update delList val (byValue cs)
                               }
  where
    delSet set = let set' = S.delete val set
                 in  if S.null set'
                       then Nothing
                       else Just set'

    delList list = let list' = L.delete suit list
                   in  if L.null list'
                         then Nothing
                         else Just list'

fromList :: [Card] -> CardSet
fromList cards = foldr insert empty cards

sumMap :: Num a => (Card -> a) -> CardSet -> a
sumMap fn cs = sum $ map fn (toList cs)

elem :: Card -> CardSet -> Bool
elem (Joker color) cs = color `L.elem` csJokers cs
elem (Card suit value) cs =
  case M.lookup value (byValue cs) of
    Nothing -> False
    Just list -> suit `L.elem` list

insertAll :: [Card] -> CardSet -> CardSet
insertAll cards cs = foldr insert cs cards

deleteAll :: [Card] -> CardSet -> CardSet
deleteAll cards cs = foldr delete cs cards

size :: CardSet -> Int
size cs = length (csJokers cs) + sum (map S.size $ M.elems $ bySuit cs)

