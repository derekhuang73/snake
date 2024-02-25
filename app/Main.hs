import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomR, StdGen, newStdGen)

-- Constants
windowWidth, windowHeight, cellSize :: Int
windowWidth = 800
windowHeight = 600
cellSize = 20
dietClock = 10
widthCells = 40
heightCells = 30

data Direction = Up | Down | Left | Right deriving (Eq)

data GameState = GameState {
    gameTime :: Float,
    snake :: [(Int, Int)],
    food :: (Int, Int),
    direction :: Direction,
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
    gameOver = False,
    score = 0
}

drawGameState :: GameState -> Picture
drawGameState gs = pictures [boundary, snakePic, foodPic, gameOverPic, scorePic]
    where
        snakePic = color blue $ pictures $ map drawSnake (snake gs)
        foodPic = color red $ drawCell (food gs)
        drawCell (x, y) = translate (fromIntegral $ x * cellSize - fromIntegral windowWidth `div` 2 + fromIntegral cellSize `div` 2) 
                          (fromIntegral $ y * cellSize - fromIntegral windowHeight `div` 2 + fromIntegral cellSize `div` 2) 
                          $ circleSolid (fromIntegral cellSize / 2)
        drawSnake (x, y) = translate (fromIntegral $ x * cellSize - fromIntegral windowWidth `div` 2 + fromIntegral cellSize `div` 2) 
                          (fromIntegral $ y * cellSize - fromIntegral windowHeight `div` 2 + fromIntegral cellSize `div` 2) 
                          $ pictures [color (white) $ rectangleWire (fromIntegral cellSize) (fromIntegral cellSize), 
                          color (blue) $ rectangleSolid (fromIntegral cellSize - 1) (fromIntegral cellSize - 1)]
        gameOverPic = if gameOver gs then pictures [translate (-200) 0 $ scale 0.5 0.5 $ color (dark red) $ text "Game Over", translate (-150) (-50) $ scale 0.2 0.2 $ color (dark red) $ text "Press R to Restart"] else blank
        boundary = color black $ rectangleWire (fromIntegral $ windowWidth - 4 * cellSize) (fromIntegral $ windowHeight - 4 * cellSize)
        scorePic = translate (-fromIntegral windowWidth / 2 + 20) (fromIntegral windowHeight / 2 - 20) 
                   $ scale 0.2 0.2 
                   $ color black 
                   $ text ("Score: " ++ show (score gs))

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'r') Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = initialState
handleInput (EventKey (SpecialKey KeyUp) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Down then gs { direction = Main.Up } else gs
handleInput (EventKey (SpecialKey KeyDown) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Up then gs { direction = Main.Down } else gs
handleInput (EventKey (SpecialKey KeyLeft) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Right then gs { direction = Main.Left } else gs
handleInput (EventKey (SpecialKey KeyRight) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Left then gs { direction = Main.Right } else gs
handleInput _ gs = gs

updateGameState :: Float -> GameState -> GameState
updateGameState time gs
    | gameOver gs = gs
    | otherwise = if collidedWithWall || collidedWithSnake || starve then gs { gameOver = True } else gs { snake = newDietSnake, gameTime = newDietGameTime, food = newFood, score = newScore}
    where
        collidedWithWall = x < 2 || x >= (windowWidth) `div` cellSize - 2|| y < 2 || y >= (windowHeight) `div` cellSize - 2
        collidedWithSnake = (x, y) `elem` tail (snake gs)
        didEatFlag = didEat (snake gs) (direction gs) (food gs)
        newSnake = moveSnake (snake gs) (direction gs) (food gs)
        newGameTime = updateGameTime (gameTime gs) time
        newDietSnake = dietSnake newSnake newGameTime
        newDietGameTime = updateDietGameTime newGameTime
        newFood = updateFood (food gs) didEatFlag
        starve = (snake gs == [])
        (x, y) = head newDietSnake
        newScore = if didEatFlag then score gs + 1 else score gs

moveSnakehead :: (Int, Int) -> Direction -> (Int, Int) 
moveSnakehead (x,y) dir = 
	case dir of
	    Main.Up    -> (x, y + 1)
	    Main.Down  -> (x, y - 1) 
	    Main.Left  -> (x - 1, y)
	    Main.Right -> (x + 1, y)

updateGameTime :: Float -> Float -> Float
updateGameTime gameTime time = gameTime + time

removeLastCell :: Snake -> Snake
removeLastCell [] = []
removeLastCell [_] = []
removeLastCell (x:xs) = x : removeLastCell xs

dietSnake :: Snake -> Float -> Snake
dietSnake snake gameTime 
    |(gameTime >= dietClock) = removeLastCell snake
    | otherwise = snake

updateDietGameTime :: Float -> Float
updateDietGameTime gameTime
    |(gameTime >= dietClock) = gameTime - dietClock
    | otherwise = gameTime

updateFood :: Food -> Bool -> Food
updateFood (foodx, foody) didEatFlag
    | didEatFlag = (newX, newY)
    | otherwise = (foodx, foody)
    where
        --gen = newStdGen
        --newX = randomR (1, widthCells - 1) gen
        --newY = randomR (1, heightCells - 1) gen
        newX = foody
        newY = foodx

moveSnake :: Snake -> Direction -> Food -> Snake
moveSnake (head:body) dir (foodx, foody) = (newX, newY) : newBody
    where 
         (newX, newY) = moveSnakehead head dir
    	 newBody = if (foodx == newX && foody == newY) then (newX, newY):(cutSnake $ head:body) else (newX, newY):cutSnake body

cutSnake :: Snake -> Snake 
cutSnake [] = [] 
cutSnake [last] = [] 
cutSnake (h:t) = h:(cutSnake t)

didEat :: Snake -> Direction -> Food -> Bool 
didEat (head:body) dir (foodx, foody) = (foodx == newX && foody == newY)
    where 
         (newX, newY) = moveSnakehead head dir

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


