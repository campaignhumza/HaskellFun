module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (rot,_,_) = rot

-- |Returns the base string for the given system.
base :: System -> String
base (_,ax,_)= ax

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (_,_,rul) = rul 


-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar chr ((c,s):rs)
    | chr == c = s
    | otherwise = lookupChar chr rs

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne rules [] = []
expandOne rules (a:as) = (lookupChar a rules) ++ (expandOne rules as) 

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand rules str 0 = str
expand rules str n = (expand rules newstr (n-1))
    where newstr = expandOne rules str
 

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move mov ((x,y),angle) rot
    | mov == 'F' = ((x+cos((pi/180)*angle),y+sin((pi/180)*angle)),angle)
    | mov == 'L' = ((x,y),angle+rot)
    | mov == 'R' = ((x,y),angle-rot)   

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
trace :: String -> Float -> Colour -> [ColouredLine]
trace (a:as) angle col = trac' (a:as) angle col ((0,0),90) []

trace' :: String -> Float -> Colour -> TurtleState -> [ColouredLine]
trace' [] _ _ _ = []
trace' (y:ys) rot col (org,angle)
    | y == 'F' = (org,(u,v),col) : (trace' ys rot col ((u,v),angle'))
    | otherwise = trace' ys rot col ((u,v),angle')  
      where ((u,v),angle') = move y (org,angle) rot   


trac' :: String -> Float -> Colour -> TurtleState -> [TurtleState] -> [ColouredLine]
trac' [] _ _ _ _ = []
trac' (y:ys) rot col (org,angle) stack
    | y == '[' = trac' ys rot col (org,angle) ((org,angle) : stack)
    | y == ']' = trac' ys rot col (head stack) stack 
    | y == 'F' = (org,(u,v),col) : (trac' ys rot col ((u,v),angle') stack)
    | otherwise = trac' ys rot col ((u,v),angle') stack 
      where ((u,v),angle') = move y (org,angle) rot   
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem :: System -> Int -> Colour -> IO ()
drawLSystem system n colour
  = drawLines (trace (lSystem system n) (angle system) colour)
