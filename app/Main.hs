import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random()

-- Constants
windowWidth, windowHeight, cellSize :: Int
windowWidth = 800
windowHeight = 600
cellSize = 20

dietClock :: Float
dietClock = 5

data Direction = Up | Down | Left | Right deriving (Eq)

data GameState = GameState {
    gameTime :: Float,
    snake :: [(Int, Int)],
    food :: (Int, Int),
    direction :: Direction,
    ai :: Snake,
    aiDirection :: Direction,
    gameOver :: Bool,
    score :: Int
}

type Snake = [(Int, Int)] 
type Food = (Int, Int)
initialState :: GameState
initialState = GameState {
    gameTime = 0,
    snake = [(10, 10)],
    food = (20, 10),
    direction = Main.Right,
    ai = [(35,25),(35,26),(35,27)],
    aiDirection = Main.Down,
    gameOver = False,
    score = 0
}

drawGameState :: GameState -> Picture
drawGameState gs = pictures [boundary, snakePic, aiPic, foodPic, gameOverPic, scorePic]
    where
        snakePic = color blue $ pictures $ map drawCell (snake gs)
        aiPic = color black $ pictures $ map drawCell (ai gs)
        foodPic = color red $ drawCell (food gs)
        drawCell (x, y) = translate (fromIntegral $ x * cellSize - fromIntegral windowWidth `div` 2 + fromIntegral cellSize `div` 2) 
                          (fromIntegral $ y * cellSize - fromIntegral windowHeight `div` 2 + fromIntegral cellSize `div` 2) 
                          $ rectangleSolid (fromIntegral cellSize - 1) (fromIntegral cellSize - 1)
        gameOverPic = if gameOver gs then pictures [translate (-200) 0 $ scale 0.5 0.5 $ color (dark red) $ text "Game Over", translate (-150) (-50) $ scale 0.2 0.2 $ color (dark red) $ text "Press R to Restart"] else blank
        boundary = color black $ rectangleWire (fromIntegral $ windowWidth - 4 * cellSize) (fromIntegral $ windowHeight - 4 * cellSize)
        scorePic = translate (-fromIntegral windowWidth / 2 + 20) (fromIntegral windowHeight / 2 - 20) 
                   $ scale 0.2 0.2 
                   $ color black 
                   $ text ("Score: " ++ show (score gs))

-- Event Handlers 
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'r') Graphics.Gloss.Interface.Pure.Game.Down _ _) _ = initialState
handleInput (EventKey (SpecialKey KeyUp) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Down then gs { direction = Main.Up } else gs
handleInput (EventKey (SpecialKey KeyDown) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Up then gs { direction = Main.Down } else gs
handleInput (EventKey (SpecialKey KeyLeft) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Right then gs { direction = Main.Left } else gs
handleInput (EventKey (SpecialKey KeyRight) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Left then gs { direction = Main.Right } else gs
handleInput _ gs = gs

-- Update the main game state. All state changes are applied here. 
updateGameState :: Float -> GameState -> GameState
updateGameState time gs
    | gameOver gs = gs
    | otherwise = if collidedWithWall || collidedWithSnake || starve then gs { gameOver = True } else gs { snake = newDietSnake, gameTime = newDietGameTime, food = newFood, ai=newAI2, aiDirection=newAIDir, score = newScore}
    where
        collidedWithWall = x < 2 || x >= windowWidth `div` cellSize - 2 || y < 2 || y >= windowHeight `div` cellSize - 2
        collidedWithSnake = (x, y) `elem` tail (snake gs)
        didEatFlag = didEat (snake gs) (direction gs) (food gs) || didEat (ai gs) (aiDirection gs) (food gs)

        (movedAI, newAIDir) = updateAI (ai gs) (aiDirection gs) (snake gs) (food gs)
        (newAI, gameSnake) = collide movedAI (snake gs) newAIDir
        movedSnake = moveSnake gameSnake (direction gs) (food gs)
        (newSnake, newAI2) = collide movedSnake newAI (direction gs)
         
        newGameTime = updateGameTime (gameTime gs) time
        newDietSnake = dietSnake newSnake newGameTime
        newDietGameTime = updateDietGameTime newGameTime
        newFood = updateFood (food gs) didEatFlag
        starve = (snake gs == [])
        (x, y) = if length newDietSnake == 0 then (0,0) else head newDietSnake
        newScore = if didEatFlag then score gs + 1 else score gs

-- Move snake's head only
moveSnakehead :: (Int, Int) -> Direction -> (Int, Int) 
moveSnakehead (x,y) dir = 
  case dir of
    Main.Up    -> (x, y + 1)
    Main.Down  -> (x, y - 1) 
    Main.Left  -> (x - 1, y)
    Main.Right -> (x + 1, y)

updateGameTime :: Float -> Float -> Float
updateGameTime ts time = ts + time

-- Remove the last cell of the snake 
removeLastCell :: Snake -> Snake
removeLastCell [] = []
removeLastCell [_] = []
removeLastCell (x:xs) = x : removeLastCell xs

-- Diet the snake 
dietSnake :: Snake -> Float -> Snake
dietSnake s ts
    |(ts >= dietClock) = removeLastCell s
    | otherwise = s

-- Update diet timestamp 
updateDietGameTime :: Float -> Float
updateDietGameTime ts 
    |(ts >= dietClock) = ts - dietClock
    | otherwise = ts 

-- Update food to next position
updateFood :: Food -> Bool -> Food
updateFood (foodx, foody) didEatFlag
    | didEatFlag = (newX, newY)
    | otherwise = (foodx, foody)
    where
        newX = foody
        newY = foodx

-- Move snake into the next positions, accounting for all changes like eating food 
moveSnake :: Snake -> Direction -> Food -> Snake
moveSnake [] _ (_,_) = []
moveSnake (h:body) dir (foodx, foody) = (newX, newY) : newBody
    where 
      (newX, newY) = moveSnakehead h dir
      newBody = if (foodx == newX && foody == newY) then (newX, newY):(removeLastCell $ h:body) else (newX, newY):removeLastCell body

-- Checks whether the next movement of snake is eating 
didEat :: Snake -> Direction -> Food -> Bool 
didEat [] _ (_,_) = False
didEat (h:_) dir (foodx, foody) = (foodx == newX && foody == newY)
    where 
         (newX, newY) = moveSnakehead h dir


{- 
 - AI Implementation 
 - AI snake will ALWAYS take the optmized path to take the food
 - 
 - NOTE: Both snakes can eat each other's body
 - -}
updateAI :: Snake -> Direction -> Snake -> Food -> (Snake, Direction)
updateAI aiSnake aidir player target = 
  if dist2player * 2 > dist2food 
    then (moveSnake aiSnake dir2food target, dir2food)
    else (moveSnake aiSnake dir2player target, dir2player)
      where 
      (dist2player, dir2player) =  optimizeOverPlayer aiSnake aidir player
      (dist2food, dir2food) = optimizeOverFood aiSnake aidir target

optimizeOverPlayer :: Snake -> Direction -> Snake -> (Int, Direction)
optimizeOverPlayer [] aidir _ = (10000, aidir)
optimizeOverPlayer _ aidir [] = (10000, aidir)
optimizeOverPlayer (aiHead:_) aidir (_:player) = (optDist, optDir)
  where 
    (_ ,target) = foldr (\v (accDist, accCoord) -> if cartesianDist aiHead v < accDist then (cartesianDist aiHead v, v) else (accDist, accCoord)) (100000,(1000,1000)) player
    optDist = cartesianDist aiHead target
    optDir = cartesianMainDir aidir aiHead target
  

optimizeOverFood :: Snake -> Direction -> Food -> (Int, Direction)
optimizeOverFood [] aidir _ = (0, aidir)
optimizeOverFood (aiHead:_) aidir target = (optDist, optDir)
  where 
    optDist = cartesianDist aiHead target 
    optDir = cartesianMainDir aidir aiHead target

-- Some Helper Functions for Cartesian geometry 
cartesianDist :: (Int,Int) -> (Int, Int) -> Int
cartesianDist (x1,y1) (x2,y2) = abs x1-x2 + abs  y1-y2

-- Cartesian Main Direction is the "main" direction from source to target, 
-- Given that the "main" direction cannot be direct opposite of the current direction
-- TODO: this does not consider boundary cases (i.e the ai snake can go out of the walls in the turning cases..)
cartesianMainDir :: Direction -> (Int,Int) -> (Int, Int) -> Direction
cartesianMainDir currDir (x1,y1) (x2,y2)  
  | x1 == x2 && y1 == y2 = currDir -- No direction change 
  | x1 == x2 = 
    if y1 < y2 
      then if currDir == Main.Down then Main.Left else Main.Up -- food is up
      else if currDir == Main.Up then Main.Right else Main.Up-- food is down
  | y1 == y2 = 
    if x1 < x2 
      then if currDir == Main.Left then Main.Up else Main.Right -- food is right
      else if currDir == Main.Right then Main.Up else Main.Left -- food is left
  | otherwise = 
    if y1 < y2 
              then if currDir == Main.Down then xdir else Main.Up -- food is up
              else if currDir == Main.Up then xdir else Main.Down -- food is down
    where  
     xdir = if x1 < x2 
              then if currDir == Main.Left then currDir else Main.Right -- food is right
              else if currDir == Main.Right then currDir else Main.Left -- food is left

collide :: Snake -> Snake -> Direction -> (Snake, Snake)
collide ours (theirsHead:theirsTail) oursDir = 
  if count == 0 then (ours, theirs) else (grow (head ours) ours oursDir, theirs)
    where 
    (count, rest) = cutSnake (head ours) theirsTail
    theirs = (theirsHead: rest)
  -- (grow (head ours) ours oursDir, afterCut)
  -- where 
  --   afterCut = cutSnake (head ours) theirs 
  --   
cutSnake :: (Int, Int) -> Snake -> (Int, Snake)
cutSnake _ [] = (0,[])
cutSnake (x,y) ((x1,y1):b) = 
  case x1 == x && y1 == y of 
    True -> (length b, [])
    False -> (c, (x1,y1):l)
      where 
       (c, l) = cutSnake (x,y) b

grow :: (Int,Int) -> Snake -> Direction -> Snake
grow (x,y) [] dir = ((x,y):[b1,b2,b3]) where
  oppositeDir = case dir of
      Main.Up    ->  Main.Down
      Main.Down  ->  Main.Up
      Main.Left  ->  Main.Right
      Main.Right ->  Main.Left 
  b1 = moveSnakehead (x,y) oppositeDir
  b2 = moveSnakehead b1 oppositeDir
  b3 = moveSnakehead b2 oppositeDir
grow _ (h:b) dir = (h:grow h b dir)
  

-- Entry point
main :: IO ()
main = do
    play
        (InWindow "Snake" (windowWidth, windowHeight) (10, 10))
        white
        10
        initialState
        drawGameState
        handleInput
        updateGameState


