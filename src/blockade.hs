-- @author Hugo Larsson Wilhelmsson and Erik Smit

import UI.NCurses
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

main :: IO ()
-- Draw the screen
main = do
    drawScreen
    putStrLn "Game Over"

-- Wall frames
wallList = 
    [ "#######################################################################################################################"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#                                                                                                                     #"
    , "#######################################################################################################################"
    ]
-- Draws the game board
drawScreen = runCurses $ do
    -- Turns off the function to write in the terminal
    setEcho False
    -- Use the DefaultWindow from ncurses to get the whole terminal window
    gameBoard <- defaultWindow
    -- Update the window with the game board
    updateWindow gameBoard $ do
        -- 'unlines' creates one string of all the strings in the list with \n between them. drawString draws the string on the window
        drawString $ unlines wallList
        -- Move the cursor to the coordinate (x,y)
        moveCursor 29 4
        -- Draws the string where the cursor currently is pointing
        drawString "(press q or esc to quit)"
        -- Start coordinates 1
        moveCursor 15 30
        drawString "A"
        -- Start coordinates 2
        moveCursor 15 89
        drawString "A"
    -- Render gameBoard
    render
    -- update the matrix with start coordinate 1 and store it in initOnePlayerMatrix
    initOnePlayerMatrix <- updateMatrixInCurses wallList (15, 30) '#'
    -- update the matrix with start coordinate 2 and store it into initTwoPlayerMatrix
    initTwoPlayerMatrix <- updateMatrixInCurses initOnePlayerMatrix (15, 89) '#'
    -- Calls gameLoop with the window and a (Event -> Bool)
    gameLoop gameBoard (\event -> event == EventCharacter '\ESC' || event == EventCharacter 'q' || event == EventCharacter 'Q') 30 15 89 15 "up" "up" initTwoPlayerMatrix



-- Updates the game continously
-- Inputs: (Window to work in, a function type which returns true if 1,Q,esc is passed to it, x1, y1, x2, y2, player1 direction, player2 direction, matrix with the walls)
-- The return type is a Curses monadic action
gameLoop :: Window -> (Event -> Bool) -> Integer -> Integer -> Integer -> Integer -> String -> String -> [[Char]] -> Curses ()
gameLoop gameBoard f x1 y1 x2 y2 dir1 dir2 matrix = loop where
    loop = do
        -- Delay
        liftIO $ threadDelay 100000
        -- Takes the player's input from the gameBoard window
        -- Just 0 means that we are not waiting on input
        event <- getEvent gameBoard (Just 0)
        case event of
            -- Call moveCharacter1 if no input (continue in the same direction as before)
            Nothing -> moveCharacter1 gameBoard f x1 y1 x2 y2 dir1 dir2 matrix
            Just event' ->
                -- quit the loop if f event' (event in f) is true, else call moveCharacter1 with the new direction
                if f event'
                    then return ()
                    -- Checks all the possible inputs for movement (for both players) and calls moveCharacter1 with the correct direction
                    else
                        case event' of
                            EventCharacter 'w' -> moveCharacter1 gameBoard f x1 y1 x2 y2 "up" dir2 matrix
                            EventCharacter 'a' -> moveCharacter1 gameBoard f x1 y1 x2 y2 "left" dir2 matrix
                            EventCharacter 's' -> moveCharacter1 gameBoard f x1 y1 x2 y2 "down" dir2 matrix
                            EventCharacter 'd' -> moveCharacter1 gameBoard f x1 y1 x2 y2 "right" dir2 matrix
                            EventSpecialKey KeyUpArrow -> moveCharacter1 gameBoard f x1 y1 x2 y2 dir1 "up" matrix
                            EventSpecialKey KeyLeftArrow -> moveCharacter1 gameBoard f x1 y1 x2 y2 dir1 "left" matrix
                            EventSpecialKey KeyDownArrow -> moveCharacter1 gameBoard f x1 y1 x2 y2 dir1 "down" matrix
                            EventSpecialKey KeyRightArrow -> moveCharacter1 gameBoard f x1 y1 x2 y2 dir1 "right" matrix
                            -- If something else is pressed, let the players continue in their current direction
                            _ -> moveCharacter1 gameBoard f x1 y1 x2 y2 dir1 dir2 matrix


-- Move character 1
-- The same input/output as gameLoop
moveCharacter1 :: Window -> (Event -> Bool) -> Integer -> Integer -> Integer -> Integer -> String -> String -> [[Char]] -> Curses ()
moveCharacter1 gameBoard f x1 y1 x2 y2 dir1 dir2 matrix = do 
        updateWindow gameBoard $ do
            moveCursor y1 x1
            drawString "#"
        render
        if dir1 == "up" then do
            -- Update the coordinates for player 1
            let newX1 = x1
            let newY1 = y1 - 1
            -- Check collition
            collition <- checkCollition gameBoard f newX1 newY1 "A" matrix
            -- Update the matrix that is checked for collition (it contains all the walls)
            newMatrix <- updateMatrixInCurses matrix (fromIntegral newY1, fromIntegral newX1) '#'
            -- Call moveCharacter2 so the two players both move in the same gameLoop
            moveCharacter2 gameBoard f newX1 newY1 x2 y2 dir1 dir2 newMatrix collition
        else if dir1 == "left" then do
            let newX1 = x1 - 1
            let newY1 = y1
            collition <- checkCollition gameBoard f newX1 newY1 "<" matrix
            newMatrix <- updateMatrixInCurses matrix (fromIntegral newY1, fromIntegral newX1) '#'
            moveCharacter2 gameBoard f newX1 newY1 x2 y2 dir1 dir2 newMatrix collition
        else if dir1 == "down" then do
            let newX1 = x1
            let newY1 = y1 + 1
            collition <- checkCollition gameBoard f newX1 newY1 "v" matrix
            newMatrix <- updateMatrixInCurses matrix (fromIntegral newY1, fromIntegral newX1) '#'
            moveCharacter2 gameBoard f newX1 newY1 x2 y2 dir1 dir2 newMatrix collition
        else if dir1 == "right" then do
            let newX1 = x1 + 1
            let newY1 = y1
            collition <- checkCollition gameBoard f newX1 newY1 ">" matrix
            newMatrix <- updateMatrixInCurses matrix (fromIntegral newY1, fromIntegral newX1) '#'
            moveCharacter2 gameBoard f newX1 newY1 x2 y2 dir1 dir2 newMatrix collition
        else error "Wrong input"


-- Move character 2
-- The same input/output as gameLoop
moveCharacter2 :: Window -> (Event -> Bool) -> Integer -> Integer -> Integer -> Integer -> String -> String -> [[Char]] -> Bool -> Curses ()
moveCharacter2 gameBoard f newX1 newY1 x2 y2 dir1 dir2 matrix prevCollition = do 
        updateWindow gameBoard $ do
            -- Move Cursor between the different players
            moveCursor y2 x2
            drawString "#"
        render
        if dir2 == "up" then do
            -- Update the coordinates for player 2
            let newX2 = x2
            let newY2 = y2 - 1
            -- Check collition
            collition <- checkCollition gameBoard f newX2 newY2 "A" matrix
            -- Update the matrix that is checked for collition (it contains all the walls)
            if collition || prevCollition then endGame gameBoard f
                else do
                    newMatrix <- updateMatrixInCurses matrix (fromIntegral newY2, fromIntegral newX2) '#'
                    -- Call gameLoop to continue with the new values
                    gameLoop gameBoard f newX1 newY1 newX2 newY2 dir1 dir2 newMatrix
        else if dir2 == "left" then do
            let newX2 = x2 - 1
            let newY2 = y2
            collition <- checkCollition gameBoard f newX2 newY2 "<" matrix
            if collition || prevCollition then endGame gameBoard f
                else do
                    newMatrix <- updateMatrixInCurses matrix (fromIntegral newY2, fromIntegral newX2) '#'
                    gameLoop gameBoard f newX1 newY1 newX2 newY2 dir1 dir2 newMatrix
        else if dir2 == "down" then do
            let newX2 = x2
            let newY2 = y2 + 1
            collition <- checkCollition gameBoard f newX2 newY2 "v" matrix
            if collition || prevCollition then endGame gameBoard f
                else do
                    newMatrix <- updateMatrixInCurses matrix (fromIntegral newY2, fromIntegral newX2) '#'
                    gameLoop gameBoard f newX1 newY1 newX2 newY2 dir1 dir2 newMatrix
        else if dir2 == "right" then do
            let newX2 = x2 + 1
            let newY2 = y2
            collition <- checkCollition gameBoard f newX2 newY2 ">" matrix
            if collition || prevCollition then endGame gameBoard f
                else do
                    newMatrix <- updateMatrixInCurses matrix (fromIntegral newY2, fromIntegral newX2) '#'
                    gameLoop gameBoard f newX1 newY1 newX2 newY2 dir1 dir2 newMatrix
        else error "Wrong input"


-- Check collition
checkCollition :: Window -> (Event -> Bool) -> Integer -> Integer -> String -> [[Char]] -> Curses Bool
checkCollition gameBoard f newX newY dir matrix = do
    printPos gameBoard newX newY dir
    if (((matrix !! fromIntegral newY) !! fromIntegral newX /= ' '))
        then return True
        else return False


-- Print the head at the new position
printPos :: Window -> Integer -> Integer -> String -> Curses ()
printPos gameBoard newX newY dirSymbol = do
    updateWindow gameBoard $ do
        moveCursor newY newX
        drawString dirSymbol
    render


-- An infinite loop that shows "GAME OVER" until the player press q,Q or esc
endGame :: Window -> (Event -> Bool) -> Curses ()
endGame gameBoard f = do
    -- Write "GAME OVER"
    updateWindow gameBoard $ do
        moveCursor 10 52
        drawString "GAME OVER"
    render
    loop where
    loop = do
        event <- getEvent gameBoard (Just 0)
        case event of
                Nothing -> loop
                Just event' ->
                    if (f event')
                        then return ()
                        else loop


-- Input: a matrix, coordinates and a new value. Returns a matrix with the new value at the given coordinates
updateMatrix :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
updateMatrix matrix (i, j) value =
    -- go through every row and col and give the new value to index (i,j)
  [ [ if (row, col) == (i, j) then value else currentValue
      -- give each row a index, to loop through
      | (col, currentValue) <- zip [0..] contentRow ]
      -- give each col a index, to loop through
      | (row, contentRow) <- zip [0..] matrix ]


-- updateMatrixInCurses must be called from the monad, which then calls updateMatrix and returns the updated matrix
updateMatrixInCurses :: [[Char]] -> (Int, Int) -> Char -> Curses [[Char]]
updateMatrixInCurses matrix (i, j) value = do
  let updatedMatrix = updateMatrix matrix (i, j) value
  -- liftIO is used to lift the updated matrix into the Curses monade
  liftIO $ return updatedMatrix