{-# LANGUAGE ScopedTypeVariables #-}
module Day9 (day9) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List.Index (imap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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

type Input m = m Value
type Output m = Value -> m ()

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

runProgram :: forall m. Monad m => Input m -> Output m -> Program -> m ProgState
runProgram i o  = execStateT (stepProgram >>= go) . initializeProgram
    where go :: PResult m -> ProgT m ()
          go PHalt            = pure ()
          go (PInput cont)    = lift i     >>= cont >>= go
          go (POutput v cont) = lift (o v) >>  cont >>= go

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

parseInput :: IO Program
parseInput = fmap (V.fromList . fmap read . split ',') getLine

star1 :: IO ()
star1 = parseInput >>= runProgram (pure 1) (print) >>= const (pure ())

star2 :: IO ()
star2 = undefined

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day9 = AnyDay (Day parseStar runStar "day9" "Day 9 of AoC 2019")
