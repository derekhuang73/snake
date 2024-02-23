import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random()

-- Constants
windowWidth, windowHeight, cellSize :: Int
windowWidth = 800
windowHeight = 600
cellSize = 20

data Direction = Up | Down | Left | Right deriving (Eq)

data GameState = GameState {
    snake :: [(Int, Int)],
    food :: (Int, Int),
    direction :: Direction,
    gameOver :: Bool
}

initialState :: GameState
initialState = GameState {
    snake = [(0, 0)],
    food = (10, 10),
    direction = Main.Right,
    gameOver = False
}

drawGameState :: GameState -> Picture
drawGameState gs = pictures [snakePic, foodPic, gameOverPic]
    where
        snakePic = color green $ pictures $ map drawCell (snake gs)
        foodPic = color red $ drawCell (food gs)
        drawCell (x, y) = translate (fromIntegral $ x * cellSize) (fromIntegral $ y * cellSize) $ rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)
        gameOverPic = if gameOver gs then translate (-200) 0 $ scale 0.3 0.3 $ color (dark red) $ text "Game Over" else blank

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Down then gs { direction = Main.Up } else gs
handleInput (EventKey (SpecialKey KeyDown) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Up then gs { direction = Main.Down } else gs
handleInput (EventKey (SpecialKey KeyLeft) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Right then gs { direction = Main.Left } else gs
handleInput (EventKey (SpecialKey KeyRight) Graphics.Gloss.Interface.Pure.Game.Down _ _) gs = if direction gs /= Main.Left then gs { direction = Main.Right } else gs
handleInput _ gs = gs

updateGameState :: Float -> GameState -> GameState
updateGameState _ gs
    | gameOver gs = gs
    | otherwise = if collidedWithWall || collidedWithSnake then gs { gameOver = True } else gs { snake = newSnake }
    where
        collidedWithWall = x < 0 || x >= windowWidth || y < 0 || y >= windowHeight
        collidedWithSnake = (x, y) `elem` tail (snake gs)
        newSnake = moveSnake (snake gs) (direction gs)
        (x, y) = head newSnake

moveSnake :: [(Int, Int)] -> Direction -> [(Int, Int)]
moveSnake ((x, y):xs) dir = case dir of
    Main.Up    -> (x, y + 1) : moveSnake' xs
    Main.Down  -> (x, y - 1) : moveSnake' xs
    Main.Left  -> (x - 1, y) : moveSnake' xs
    Main.Right -> (x + 1, y) : moveSnake' xs
    where moveSnake' [] = []
          moveSnake' [_] = [(x, y)]
          moveSnake' (x':xs') = x' : moveSnake' xs'

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