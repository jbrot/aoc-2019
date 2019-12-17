{-# LANGUAGE ScopedTypeVariables #-}
module Day15 (day15) where

import Prelude hiding (Left, Right)

import Control.Monad.Trans.State
import Data.List.Index (imap)
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.PSQueue (PSQ, Binding (..))
import qualified Data.PSQueue as P
import Data.Vector (Vector)
import qualified Data.Vector as V
import Options.Applicative

import Common
import CLI

type Value = Integer

type Program = Vector Value
type Address = Int
type Pointer = Address
type Memory = Map Address Value
data ProgState = ProgState { memory :: Memory
                           , instPointer  :: Pointer
                           , relBase :: Pointer
                           }

initializeProgram :: Program -> ProgState
initializeProgram v = ProgState (M.fromList . V.toList . V.indexed $ v) 0 0

type ProgT = StateT ProgState

data Opcode = Add | Multiply | Halt | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals | AdjustRelBase
data ParamMode = Position | Immediate | Relative

readMem :: Monad m => Address -> ProgT m Value
readMem a = fmap (M.findWithDefault 0 a . memory) get

readMemRel :: Monad m => Int -> ProgT m Value
readMemRel o = do
    st <- get
    readMem (instPointer st + o)

writeMem :: Monad m => Address -> Value -> ProgT m ()
writeMem a v = modify (\ps -> ps{memory = M.insert a v (memory ps)})

stepPointer :: Monad m => Int -> ProgT m ()
stepPointer o = modify (\st -> st{instPointer = instPointer st + o})

setPointer :: Monad m => Pointer -> ProgT m ()
setPointer p = modify (\st -> st{instPointer = p})

stepRelBase :: Monad m => Int -> ProgT m ()
stepRelBase o = modify (\st -> st{relBase = relBase st + o})

toParamModes :: Value -> [ParamMode]
toParamModes = foldlDigits parse (repeat Position)
  where parse modes 0 = Position:modes
        parse modes 1 = Immediate:modes
        parse modes 2 = Relative:modes
        parse _ _ = undefined

toOpcode :: Value -> Opcode
toOpcode 1 = Add
toOpcode 2 = Multiply
toOpcode 3 = Input
toOpcode 4 = Output
toOpcode 5 = JumpIfTrue
toOpcode 6 = JumpIfFalse
toOpcode 7 = LessThan
toOpcode 8 = Equals
toOpcode 9 = AdjustRelBase
toOpcode 99 = Halt
toOpcode _ = undefined

parseOpcode :: Value -> (Opcode, [ParamMode])
parseOpcode n = (toOpcode (n `mod` 100), toParamModes (n `div` 100))

cvrt :: (Integral a, Num b) => a -> b
cvrt = fromInteger . toInteger

getParameter :: Monad m => Int -> ParamMode -> ProgT m (Value, Address)
getParameter o m = do
    val <- readMemRel (o + 1)
    case m of
      Position -> let v = cvrt val in fmap (flip (,) v) (readMem v)
      Immediate -> pure (val, undefined)
      Relative -> do
          rb <- fmap relBase get
          let v = cvrt val + rb in fmap (flip (,) v) (readMem v)

getOpcode :: Monad m => ProgT m (Opcode, [ProgT m (Value, Address)])
getOpcode = fmap (fmap (imap getParameter) . parseOpcode) (readMemRel 0)

data PResult m = PHalt | PInput (Value -> ProgT m (PResult m)) | POutput Value (ProgT m (PResult m))
-- Step the program; returns True if halt, False if we should still run
stepProgram :: Monad m => ProgT m (PResult m)
stepProgram = do
    (op, params) <- getOpcode
    (p1,p2,p3) <- case params of
                    p1:p2:p3:_ -> pure (p1,p2,p3)
                    _ -> undefined
    case op of
      Add -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          writeMem c (a + b)
          stepPointer 4
          stepProgram
      Multiply -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          writeMem c (a * b)
          stepPointer 4
          stepProgram
      Input -> do
          (_,a) <- p1
          pure . PInput $ \v -> do
              writeMem a v
              stepPointer 2
              stepProgram
      Output -> do
          (a,_) <- p1
          pure . POutput a $ do
              stepPointer 2
              stepProgram
      JumpIfTrue -> do
          (a,_) <- p1
          (b,_) <- p2
          if a /= 0 then setPointer (cvrt b)
                    else stepPointer 3
          stepProgram
      JumpIfFalse -> do
          (a,_) <- p1
          (b,_) <- p2
          if a == 0 then setPointer (cvrt b)
                    else stepPointer 3
          stepProgram
      LessThan -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          if a < b then writeMem c 1
                   else writeMem c 0
          stepPointer 4
          stepProgram
      Equals -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          if a == b then writeMem c 1
                    else writeMem c 0
          stepPointer 4
          stepProgram
      AdjustRelBase -> do
          (a,_) <- p1
          stepRelBase (cvrt a)
          stepPointer 2
          stepProgram
      Halt -> pure PHalt

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

parseInput :: IO Program
parseInput = fmap (V.fromList . fmap read . split ',') getLine

data RunningProgram m = RunningProgram { st  :: ProgState
                                       , res :: PResult m
                                       , output :: [Value] -- newest to oldest
                                       , queuedInput :: [Value] -- consumes first to last
                                       }

advanceRP :: Monad m => RunningProgram m -> m (RunningProgram m)
advanceRP rp = case (res rp) of
                 PHalt -> pure rp
                 PInput cont -> case queuedInput rp of
                                  (i:is) -> step (cont i) rp{queuedInput = is} >>= advanceRP
                                  [] -> pure rp
                 POutput v cont -> step cont rp{output = v:(output rp)} >>= advanceRP
    where step m rp = fmap (\(r, s) -> rp{st = s, res = r}) (runStateT m (st rp))

startProgram :: Monad m => Program -> m (RunningProgram m)
startProgram prog = do
    (r, st) <- runStateT stepProgram (initializeProgram prog)
    pure (RunningProgram st r [] [])

queueInput :: [Value] -> RunningProgram m -> RunningProgram m
queueInput i rp = rp{queuedInput = (queuedInput rp) <>  i}

type Row = Int
type Col = Int
data Movement = North | South | East | West
    deriving (Show)
data Result = Wall | Empty | Oxygen
    deriving (Eq, Show)

movement :: Movement -> Value
movement North = 1
movement South = 2
movement West = 3
movement East = 4

offset :: Movement -> (Row, Col)
offset North = (-1,  0)
offset South = ( 1,  0)
offset East  = ( 0,  1)
offset West  = ( 0, -1)

result :: Value -> Result
result 0 = Wall
result 1 = Empty
result 2 = Oxygen
result _ = undefined

data Entry = Entry { distance :: Int
                   , pstate :: RunningProgram Identity
                   }

-- This is lawful, but can cause unintended effects
-- We should be okay here, though
instance Eq Entry where
    e1 == e2 = (distance e1) == (distance e2)
instance Ord Entry where
    compare e1 e2 = compare (distance e1) (distance e2)

add :: (Row, Col) -> (Row, Col) -> (Row, Col)
add (r,c) (r', c') = (r + r', c + c')

djikstra :: PSQ (Row,Col) Entry -> Entry
djikstra q = maybe (djikstra newQ) (\(_ :-> e, _) -> e) oxygen
    where Just (pos :-> e, q1) = P.minView q
          neighbors = fmap (\d -> (pos `add` offset d) :-> Entry (1 + distance e) (runIdentity . advanceRP . queueInput [movement d] . pstate $ e)) [North, South, East, West]
          goodNeighbors = filter (\(k :-> v) -> maybe True (\v2 -> v < v2 && distance v2 /= maxBound) (P.lookup k q1)) neighbors
          checkedNeighbors = fmap (\(k :-> e) -> (k :-> e, result . head . output . pstate $ e)) goodNeighbors
          validNeighbors = fmap fst . filter (\(_,r) -> r /= Wall) $ checkedNeighbors
          newQ = P.insert pos e{distance = maxBound} $ foldr (\(k :-> v) q -> P.insert k v q) q1 validNeighbors
          oxygen = listToMaybe (filter (\(_,r) -> r == Oxygen) checkedNeighbors)

djikstra2 :: PSQ (Row,Col) Entry -> Entry
djikstra2 q = if maybe True ((== maxBound) . distance . P.prio) (P.findMin newQ) then e else djikstra2 newQ
    where Just (pos :-> e, q1) = P.minView q
          neighbors = fmap (\d -> (pos `add` offset d) :-> Entry (1 + distance e) (runIdentity . advanceRP . queueInput [movement d] . pstate $ e)) [North, South, East, West]
          goodNeighbors = filter (\(k :-> v) -> maybe True (\v2 -> v < v2 && distance v2 /= maxBound) (P.lookup k q1)) neighbors
          checkedNeighbors = fmap (\(k :-> e) -> (k :-> e, result . head . output . pstate $ e)) goodNeighbors
          validNeighbors = fmap fst . filter (\(_,r) -> r /= Wall) $ checkedNeighbors
          newQ = P.insert pos e{distance = maxBound} $ foldr (\(k :-> v) q -> P.insert k v q) q1 validNeighbors

star1 :: IO ()
star1 = parseInput >>= print . distance . djikstra . P.singleton (0,0) . Entry 0 . runIdentity . startProgram

star2 :: IO ()
star2 = do
    inp <- parseInput
    let o2 = djikstra . P.singleton (0,0) . Entry 0 . runIdentity . startProgram $ inp
    let far = djikstra2 . P.singleton (0,0) . Entry 0 . pstate $ o2
    print (distance far)

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day15 = AnyDay (Day parseStar runStar "day15" "Day 15 of AoC 2019")
