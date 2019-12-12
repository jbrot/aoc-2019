{-# LANGUAGE ExistentialQuantification #-}
module CLI where

import Options.Applicative

data Day a = Day { parseCmd :: Parser a
                 , runCmd :: a -> IO ()
                 , name :: String
                 , desc :: String
                 }

data AnyDay = forall a. AnyDay (Day a)

anyDayToParser :: AnyDay -> Mod CommandFields (IO ())
anyDayToParser (AnyDay (Day p f n d)) = command n . info (f <$> p) . progDesc $ d
