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
    snake = [(10, 10), (9, 10), (8, 10)],
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
    | otherwise = if collidedWithWall 
      || starve then gs { gameOver = True, snake = newDietSnake, gameTime = newDietGameTime, food = newFood, ai=newDietAI, aiDirection=newAIDir, score = newScore} else gs { snake = newDietSnake, gameTime = newDietGameTime, food = newFood, ai=newDietAI, aiDirection=newAIDir, score = newScore}
    where
        collidedWithWall = x < 3 || x >= windowWidth `div` cellSize - 3 || y < 3 || y >= windowHeight `div` cellSize - 3
        -- collidedWithSnake = (x, y) `elem` tail (snake gs)
        didEatFlag = didEat (snake gs) (direction gs) (food gs)
        
        --Update both snake
        (newAI1, newAIDir) = updateAI (ai gs) (aiDirection gs) (snake gs) (food gs)
        newSnake1 = moveSnake (snake gs) (direction gs) (food gs)

        --consider did eat with updated dir
        didAiEatFlag = didEat (ai gs) newAIDir (food gs)
       
        --remove parts if collides with the updated position
        (newSnake2, newAI2) = collide newSnake1 newAI1
        (newAI3, newSnake3) = collide newAI2 newSnake2
        
        -- calculate diet timer
        newGameTime = updateGameTime (gameTime gs) time
        newDietSnake = dietSnake newSnake3 newGameTime
        newDietAI = dietSnake newAI3 newGameTime
        newDietGameTime = updateDietGameTime newGameTime
        
        -- render new food and starve settings
        newFood = updateFood (food gs) (didEatFlag || didAiEatFlag)
        starve = (newAI2 == [] || newDietSnake == [])
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
    | didEatFlag = (foody, foodx)
    | otherwise = (foodx, foody)

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
 - AI snake will take the optmized path to either take the food, or
 - try to collide with the user snake
 - NOTE: Both snakes can eat each other's body
 - -}
updateAI :: Snake -> Direction -> Snake -> Food -> (Snake, Direction)
updateAI aiSnake aidir player target = 
  if dist2player * 3 > dist2food 
    then (moveSnake aiSnake dir2food target, dir2food)
    else (moveSnake aiSnake dir2player target, dir2player)
      where 
      (dist2player, dir2player) =  optimizeOverPlayer aiSnake aidir player
      (dist2food, dir2food) = optimizeOverFood aiSnake aidir target

-- AI algorithm: calculate distance and direction to collide with player
optimizeOverPlayer :: Snake -> Direction -> Snake -> (Int, Direction)
optimizeOverPlayer [] aidir _ = (10000, aidir)
optimizeOverPlayer _ aidir [] = (10000, aidir)
optimizeOverPlayer (aiHead:_) aidir (_:player) = (optDist, optDir)
  where 
    (_ ,target) = foldr (\v (accDist, accCoord) -> if cartesianDist aiHead v < accDist then (cartesianDist aiHead v, v) else (accDist, accCoord)) (100000,(1000,1000)) player
    optDist = cartesianDist aiHead target
    optDir = cartesianMainDir aidir aiHead target
  
-- AI algorithm: calculate distance and direction to take the food
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

-- Collision logic for two snakes colliding each other
collide :: Snake -> Snake -> (Snake, Snake)
collide [] []  = ([],[])
collide snake1 []  = (snake1, [])
collide [] snake2  = ([], snake2)
collide snake1 snake2
    | (fst snake1head == fst snake2head) && (snd snake1head == snd snake2head) = (snake1, [])
    | otherwise = (snake1, snake2head:newSnake2)
    where
        snake2head = head snake2
        snake2tail = tail snake2 
        snake1head = head snake1
        newSnake2 = snd (collide snake1 snake2tail)

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


