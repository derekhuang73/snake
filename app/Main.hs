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
    gameOver :: Bool
}

type Snake = [(Int, Int)] 
type Food = (Int, Int)
initialState :: GameState
initialState = GameState {
    gameTime = 0,
    snake = [(0, 0)],
    food = (20, 10),
    direction = Main.Right,
    gameOver = False
}

drawGameState :: GameState -> Picture
drawGameState gs = pictures [boundary, snakePic, foodPic, gameOverPic]
    where
        snakePic = color blue $ pictures $ map drawCell (snake gs)
        foodPic = color red $ drawCell (food gs)
        drawCell (x, y) = translate (fromIntegral $ x * cellSize - fromIntegral windowWidth `div` 2 + fromIntegral cellSize `div` 2) 
                          (fromIntegral $ y * cellSize - fromIntegral windowHeight `div` 2 + fromIntegral cellSize `div` 2) 
                          $ rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)
        gameOverPic = if gameOver gs then translate (-200) 0 $ scale 0.3 0.3 $ color (dark red) $ text "Game Over" else blank
        boundary = color black $ rectangleWire (fromIntegral windowWidth) (fromIntegral windowHeight)

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
    | otherwise = if collidedWithWall || collidedWithSnake || starve then gs { gameOver = True } else gs { snake = newDietSnake, gameTime = newDietGameTime, food = newFood}
    where
        collidedWithWall = x < 0 || x >= windowWidth `div` cellSize || y < 0 || y >= windowHeight `div` cellSize
        collidedWithSnake = (x, y) `elem` tail (snake gs)
        didEatFlag = didEat (snake gs) (direction gs) (food gs)
        newSnake = moveSnake (snake gs) (direction gs) (food gs)
        newGameTime = updateGameTime (gameTime gs) time
        newDietSnake = dietSnake newSnake newGameTime
        newDietGameTime = updateDietGameTime newGameTime
        newFood = updateFood (food gs) didEatFlag
        starve = (snake gs == [])
        (x, y) = head newDietSnake

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


