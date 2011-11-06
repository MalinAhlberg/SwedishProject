module Color where
-- Terminal color output, stolen from Compiler Construction testsuite 
type Color = Int

color :: Color -> String -> String
color c s = fgcol c ++ s ++ normal

normal = "\ESC[0m"

bold :: String -> String
bold = ("\ESC[1m" ++)


fgcol :: Int -> String
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"


red, green, blue,pink, yellow,turkos :: Color
red = 1
green = 2
yellow = 3
blue = 4
pink = 5
turkos = 7

