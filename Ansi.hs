module Ansi where

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
  deriving (Enum,Show)

data Intensity = Dark | Bright
  deriving (Enum,Show)

ansiSetInkColor :: Intensity -> Color -> IO ()
ansiSetInkColor i c = putStr ("\ESC[" ++ show ((fromEnum c)+30+(fromEnum i)*60) ++ "m")
ansiSetBackColor :: Intensity -> Color -> IO ()
ansiSetBackColor i c = putStr ("\ESC[" ++ show ((fromEnum c)+40+(fromEnum i)*60) ++ "m")
ansiReset = putStr "\ESC[0m"
ansiBold = putStr "\ESC[1m"
ansiItalic = putStr "\ESC[3m"
ansiUnderline = putStr "\ESC[4m"
ansiBlinking = putStr "\ESC[5m"
ansiEraseLine = putStr "\ESC[2K"
ansiClearScreen = putStr "\ESC[2J\ESC[H"
ansiCursorUp :: Int -> IO ()
ansiCursorUp n = putStr ("\ESC["++show(n)++"A")
