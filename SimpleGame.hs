
import System.IO
import Control.Monad.State
import Control.Monad.IfElse
import Data.List
import Data.Function
import Data.Char

import Cards

data Player = First | Second
  deriving (Eq, Show)

type FullHistory = [(Player, Move)]
type History = [(Player, PrivateMove)]

data GameState = GS {
  hand1 :: [Card],
  hand2 :: [Card],
  deck :: [Card],
  trash :: [Card],
  openCards :: [Card],
  history :: FullHistory,
  fullLog :: [(Player,Move,Int)]}

instance Show GameState where
  show (GS h1 h2 d t _ _ _) = "I: " ++ showHand h1 ++ " |  II: " ++ showHand h2 ++ "\n" ++
                     "Deck: " ++ unwords (map show d) ++ "\n" ++
                     "Trash: " ++ unwords (map show t)

data PrivateMove = UnknownChange | KnownChange Card | NoChange
  deriving (Eq, Show)

data Move = Change Card | Pass
  deriving (Eq,Show)

data PlayerState = PS {
  me :: Player,
  myHand :: [Card],
  myOpenCards :: [Card],
  myHistory :: History }

type Game a = StateT GameState IO a

thisPackType = PackType 2 False

fullPack = orderedPack thisPackType

packSize = length fullPack
packPoints = handSum fullPack
target = 40
handSize = 5
maxMoves = 3

startState = do
  pack <- shuffle fullPack 
  return $ GS [] [] pack [] [] [] []

showHand :: [Card] -> String
showHand cards = unwords (map show cards) ++ " (" ++ show (handSum cards) ++ ")"

forgetCard :: Move -> PrivateMove
forgetCard (Change _) = UnknownChange 
forgetCard Pass       = NoChange

forget :: Player -> (Player, Move) -> (Player, PrivateMove)
forget player (p,move) =
  case (p == player, move) of
    (True,  Change card) -> (p, KnownChange card)
    (False, Change _)    -> (p, UnknownChange)
    (_,     Pass)        -> (p, NoChange)

forgetCards :: Player -> FullHistory -> History
forgetCards player = map (forget player)

give :: Player -> Card -> [Card] -> GameState -> GameState
give First  c other st = st {hand1 = c: hand1 st, deck = other}
give Second c other st = st {hand2 = c: hand2 st, deck = other}

giveCard :: Player -> Game ()
giveCard player = do
  st <- get
  d <- gets deck
  case d of
    [] ->     return ()
    (c:cs) -> put $ give player c cs st

getHand :: Player -> Game [Card]
getHand First  = gets hand1
getHand Second = gets hand2

setHand :: Player -> [Card] -> Game ()
setHand First  hand = get >>= \s -> put (s {hand1 = hand})
setHand Second hand = get >>= \s -> put (s {hand2 = hand})

exclude card = filter (/= card)

trashCard :: Player -> Card -> Game ()
trashCard First  card = do
    st <- get
    let hand = exclude card (hand1 st)
    put $ st {hand1 = hand, trash = card: trash st}
trashCard Second card = do
    st <- get 
    let hand = exclude card (hand2 st)
    put $ st {hand2 = hand, trash = card: trash st}

emptyHand :: Player -> Game ()
emptyHand First = do
  st <- get
  put $ st {hand1 = [], trash = hand1 st ++ trash st, openCards = hand1 st ++ openCards st}
emptyHand Second = do
  st <- get
  put $ st {hand2 = [], trash = hand2 st ++ trash st, openCards = hand2 st ++ openCards st}

playerMove :: Player -> Game ()
playerMove player = do
  hand <- getHand player
  hist <- gets (forgetCards player . history)
  open <- gets openCards
  move <- runPlayer player (PS player hand open hist)
  runMove player move
  hand' <- getHand player
  writeHistory player move
  writeLog player move (handSum hand')

writeHistory :: Player -> Move -> Game ()
writeHistory player move = do
  st <- get
  put $ st {history = (player,move): history st}

writeLog :: Player -> Move -> Int -> Game ()
writeLog player move p = do
  st <- get
  put $ st {fullLog = (player,move,p): fullLog st}

runMove :: Player -> Move -> Game ()
runMove player Pass = return ()
runMove player (Change card) = do
    trashCard player card
    giveCard player

points :: PlayerState -> Double
points ps = fromIntegral $ handSum $ myHand ps

evalChange :: PlayerState -> Card -> Double
evalChange ps card = points ps - (fromIntegral $ cardPoints card) + avgPoints ps

nTrashed :: PlayerState -> Int
nTrashed ps = length $ filter (isChange . snd) (myHistory ps)
  where
    isChange NoChange = False
    isChange _        = True

handSum :: [Card] -> Int
handSum cards = sum $ map cardPoints cards

trashedSum :: PlayerState -> Double
trashedSum ps = fromIntegral $ sum $ map (val . snd) (myHistory ps)
  where
    val NoChange = 0
    val UnknownChange = 0
    val (KnownChange c) = cardPoints c

openSum :: PlayerState -> Double
openSum ps = fromIntegral $ sum $ map cardPoints (myOpenCards ps)

nOpened :: PlayerState -> Int
nOpened ps = length $ myOpenCards ps

avgPoints :: PlayerState -> Double
avgPoints ps = 
  let n = packSize - 2*handSize - nTrashed ps - nOpened ps
      m = (fromIntegral $ packPoints) - points ps - trashedSum ps - openSum ps
  in  m / (fromIntegral n)

evalMove :: PlayerState -> Move -> Double
evalMove ps Pass = abs (target - points ps)
evalMove ps (Change card) = abs (target - evalChange ps card)

ai :: PlayerState -> Game Move
ai ps = 
  let (move,_) = minimumBy (compare `on` snd) $ map evalMove' possibleMoves
      evalMove' m = (m, evalMove ps m)
      possibleMoves = Pass: [Change card | card <- myHand ps]
  in  do
    lift $ putStrLn $ "AI: " ++ show (forgetCard move)
    return move

runPlayer :: Player -> PlayerState -> Game Move
runPlayer First  = humanPlayer
runPlayer Second = ai

readMove :: String -> Move
readMove str@(v:s) =
      if isDigit v
        then if (v == '1') && (head s == '0')
                then Change $ Card (readSuit (tail s)) (N 10)
                else Change $ Card (readSuit s) (N $ read [v])
        else if v `elem` "JQKA"
                then Change $ Card (readSuit s) (readVal v)
                else if str == "pass"
                       then Pass
                       else error "Invalid move!"
  where
    readSuit "C" = Clubs
    readSuit "D" = Diamonds
    readSuit "H" = Hearts
    readSuit "S" = Spades
    
    readVal 'J' = Jack
    readVal 'Q' = Queen
    readVal 'K' = King
    readVal 'A' = Ace

humanPlayer :: PlayerState -> Game Move
humanPlayer ps =
  lift $ do
      putStrLn $ "Your hand: " ++ showHand (myHand ps)
      putStr "Your move? "
      hFlush stdout
      str <- getLine
      let move = readMove str
      return move

deal :: Game ()
deal = 
  replicateM_ handSize $ do
    giveCard First
    giveCard Second
  
game :: Game (Int, Int)
game = do
  replicateM_ maxMoves $ do
    playerMove First
    playerMove Second
  st <- get
  return (handSum $ hand1 st, handSum $ hand2 st)

enoughCards :: Game Bool
enoughCards = do
  d <- gets deck
  return $ length d > 2*handSize

fullGame :: Game ()
fullGame =
  whileM enoughCards $ do
    lift $ putStr "\nNew deal ("
    emptyHand First
    emptyHand Second
    deal
    n <- gets (length . deck)
    lift $ putStrLn $ show n ++ " cards remained)"
    (a,b) <- game
    st <- get
    let (h1,h2) = (hand1 st, hand2 st)
    lift $ putStrLn $ "I: " ++ showHand h1 ++ "; II: " ++ showHand h2
    lift $ putStrLn $
      case abs (target - fromIntegral a) `compare` abs (target - fromIntegral b) of
        EQ -> "Drawn"
        LT -> "You win"
        GT -> "AI win"

main = do
  st <- startState
  runStateT fullGame st
    
