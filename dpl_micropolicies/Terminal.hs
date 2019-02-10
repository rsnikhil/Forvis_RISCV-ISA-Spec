module Terminal where

import Text.PrettyPrint (Doc, (<+>), ($$))
import qualified Text.PrettyPrint as P
import System.Console.ANSI

-- TODO: This file and its interface to the world are a mess!

set_terminal_red :: IO ()
set_terminal_red = do
  setSGR [SetConsoleIntensity BoldIntensity]
  setSGR [SetColor Foreground Dull White]
  setSGR [SetColor Background Dull Red]

set_terminal_gray :: IO ()
set_terminal_gray = do
  setSGR [SetConsoleIntensity BoldIntensity]
  setSGR [SetColor Background Dull Yellow]
  setSGR [SetColor Foreground Dull Black]

reset_terminal :: IO ()
reset_terminal = do
  setSGR [ Reset ]

putStrLnRed s = do
  set_terminal_red
  putStr s
  reset_terminal
  putStrLn ""

putStrLnGray s = do
  set_terminal_gray
  putStr s
  reset_terminal
  putStrLn ""

makeGray s = do
  setSGRCode [ SetConsoleIntensity BoldIntensity, 
               SetColor Background Dull Yellow, 
               SetColor Foreground Dull Black ]
  ++ s ++
  setSGRCode [ Reset ]

beginGray :: String
beginGray =
  setSGRCode [ SetConsoleIntensity BoldIntensity, 
               SetColor Background Dull Yellow, 
               SetColor Foreground Dull Black ]

endColor :: String
endColor = 
  setSGRCode [ Reset ]

ppStrong p =
  (P.text $ setSGRCode [ SetConsoleIntensity BoldIntensity, 
                         SetColor Foreground Dull Red ])
  P.<>
  p
  P.<>
  (P.text $ setSGRCode [ Reset ])
  
