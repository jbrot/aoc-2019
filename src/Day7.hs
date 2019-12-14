{-# LANGUAGE ScopedTypeVariables #-}
module Day7 (day7) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.List (permutations)
import Data.List.Index (imap)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Options.Applicative

import Common
import CLI

type Program = Vector Int
type Address = Int
type Pointer = Address
type ProgT = StateT (Program, Pointer)

data Opcode = Add | Multiply | Halt | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals
data ParamMode = Position | Immediate

type Input m = m Int
type Output m = Int -> m ()

toParamModes :: Int -> [ParamMode]
toParamModes = foldlDigits parse (repeat Position)
  where parse modes 0 = Position:modes
        parse modes 1 = Immediate:modes
        parse _ _ = undefined

toOpcode :: Int -> Opcode
toOpcode 1 = Add
toOpcode 2 = Multiply
toOpcode 3 = Input
toOpcode 4 = Output
toOpcode 5 = JumpIfTrue
toOpcode 6 = JumpIfFalse
toOpcode 7 = LessThan
toOpcode 8 = Equals
toOpcode 99 = Halt
toOpcode _ = undefined

parseOpcode :: Int -> (Opcode, [ParamMode])
parseOpcode n = (toOpcode (n `mod` 100), toParamModes (n `div` 100))

getParameter :: Monad m => Int -> ParamMode -> ProgT m (Int, Address)
getParameter o m = do
    (p, ptr) <- get
    let val = p V.! (ptr + o + 1)
    case m of
      Position -> pure (p V.! val, val)
      Immediate -> pure (val, undefined)

getOpcode :: Monad m => ProgT m (Opcode, [ProgT m (Int, Address)])
getOpcode = fmap (fmap (imap getParameter) . parseOpcode . uncurry (V.!)) get 

data PResult m = PHalt | PInput (Int -> ProgT m (PResult m)) | POutput Int (ProgT m (PResult m))
-- Step the program; returns True if halt, False if we should still run
stepProgram :: Monad m => ProgT m (PResult m)
stepProgram = do
    (p, ptr) <- get
    (op, params) <- getOpcode
    (p1,p2,p3) <- case params of
                    p1:p2:p3:_ -> pure (p1,p2,p3)
                    _ -> undefined
    case op of
      Add -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          put (p V.// [(c, a + b)], ptr + 4)
          stepProgram
      Multiply -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          put (p V.// [(c, a * b)], ptr + 4)
          stepProgram
      Input -> do
          (_,a) <- p1
          pure . PInput $ \v -> do
            put (p V.// [(a, v)], ptr + 2)
            stepProgram
      Output -> do
          (a,_) <- p1
          pure . POutput a $ do
            put (p, ptr + 2)
            stepProgram
      JumpIfTrue -> do
          (a,_) <- p1
          (b,_) <- p2
          if a /= 0 then put (p, b)
                    else put (p, ptr + 3)
          stepProgram
      JumpIfFalse -> do
          (a,_) <- p1
          (b,_) <- p2
          if a == 0 then put (p, b)
                    else put (p, ptr + 3)
          stepProgram
      LessThan -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          if a < b then put (p V.// [(c, 1)], ptr + 4)
                   else put (p V.// [(c, 0)], ptr + 4)
          stepProgram
      Equals -> do
          (a,_) <- p1
          (b,_) <- p2
          (_,c) <- p3
          if a == b then put (p V.// [(c, 1)], ptr + 4)
                    else put (p V.// [(c, 0)], ptr + 4)
          stepProgram
      Halt -> pure PHalt

runProgram :: forall m. Monad m => Input m -> Output m -> Program -> m Program
runProgram i o = fmap fst . execStateT (stepProgram >>= go) . (flip (,) 0)
    where go :: PResult m -> ProgT m ()
          go PHalt            = pure ()
          go (PInput cont)    = lift i     >>= cont >>= go
          go (POutput v cont) = lift (o v) >>  cont >>= go

type Phase = Int
runAmplifier :: Program -> Phase -> Int -> Int
runAmplifier pr ph i = snd . flip execState (0,0) . runProgram inP outP $ pr
    where inP = do
              (ct,o) <- get
              put  (ct + 1,o)
              case ct of
                0 -> pure ph
                _ -> pure i
          outP o = modify (\(s,_) -> (s,o))

runAmps :: Program -> [Phase] -> Int
runAmps p = foldr (runAmplifier p) 0

data RunningProgram m = RunningProgram { st  :: (Program, Pointer)
                                       , res :: PResult m
                                       , output :: [Int] -- newest to oldest
                                       , queuedInput :: [Int] -- consumes first to last
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
    (r, st) <- runStateT stepProgram (prog, 0)
    pure (RunningProgram st r [] [])

queueInput :: [Int] -> RunningProgram m -> RunningProgram m
queueInput i rp = rp{queuedInput = (queuedInput rp) <>  i}

takeOutput :: RunningProgram m -> ([Int], RunningProgram m)
takeOutput rp = (reverse (output rp), rp{output = []})

runLoopedAmplifiers :: Program -> [Phase] -> Int
runLoopedAmplifiers prog phases = go [0] start
    where start = fmap (\phase -> queueInput [phase] . runIdentity .  startProgram $ prog) phases
          iterate inp ps = foldl (\(i,o) -> fmap (\x -> o <> [x]) . takeOutput . runIdentity . advanceRP . queueInput i) (inp, []) ps
          go i progs = let l = last progs in case res l of
                                             PHalt -> last i
                                             _ -> uncurry go $ iterate i progs


split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

parseInput :: IO Program
parseInput = fmap (V.fromList . fmap read . split ',') getLine

star1 :: IO ()
star1 = do
    p <- parseInput
    print . maximum . fmap (runAmps p) . permutations $ [0..4]

star2 :: IO ()
star2 = do
    p <- parseInput
    print . maximum . fmap (runLoopedAmplifiers p) . permutations $ [5..9]

runStar :: Bool -> IO ()
runStar False = star1
runStar True = star2

parseStar :: Parser Bool
parseStar = flag False True $ short '2' <> long "star2" <> help "Run star2" 

day7 = AnyDay (Day parseStar runStar "day7" "Day 7 of AoC 2019")
