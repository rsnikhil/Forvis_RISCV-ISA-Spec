module Terminal where

import Text.PrettyPrint (Doc, (<+>), ($$))
import qualified Text.PrettyPrint as P
import System.Console.ANSI

-- TODO: This file and its interface to the world are a mess!

set_terminal_urgent :: IO ()
set_terminal_urgent = do
  setSGR [SetConsoleIntensity BoldIntensity]
  setSGR [SetColor Foreground Dull White]
  setSGR [SetColor Background Dull Red]

set_terminal_highlight :: IO ()
set_terminal_highlight = do
  setSGR [SetConsoleIntensity BoldIntensity]
  setSGR [SetColor Background Dull Yellow]
  setSGR [SetColor Foreground Dull Black]

reset_terminal :: IO ()
reset_terminal = do
  setSGR [ Reset ]

putStrLnUrgent s = do
  set_terminal_urgent
  putStr s
  reset_terminal
  putStrLn ""

putStrLnHighlight s = do
  set_terminal_highlight
  putStr s
  reset_terminal
  putStrLn ""

makeHighlight s = do
  setSGRCode [ SetConsoleIntensity BoldIntensity, 
               SetColor Background Dull Yellow, 
               SetColor Foreground Dull Black ]
  ++ s ++
  setSGRCode [ Reset ]

beginHighlight :: String
beginHighlight =
  setSGRCode [ SetConsoleIntensity BoldIntensity, 
               SetColor Background Dull Yellow, 
               SetColor Foreground Dull Black ]

endColor :: String
endColor = 
  setSGRCode [ Reset ]

ppStrong p =
  (P.text $ setSGRCode [ SetColor Foreground Dull Blue ])
  P.<>
  p
  P.<>
  (P.text $ setSGRCode [ Reset ])
  
