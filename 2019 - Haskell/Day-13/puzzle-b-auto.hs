import System.Console.ANSI
import System.IO
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Char

{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Foreign.C.Types
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

parse :: String -> [Int]
parse input = map read (words [if c == ',' then ' ' else c | c <- input])

directAddr :: [Int] -> Int -> Int
directAddr memory addr = memory !! addr

indirectAddr :: [Int] -> Int -> Int -> Int
indirectAddr memory addr base = memory !! ((directAddr memory addr) + base)

getParam :: [Int] -> Int -> Int -> Int -> Int
getParam memory addr mode base
    | mode == 0 = indirectAddr memory addr 0
    | mode == 1 = directAddr memory addr
    | mode == 2 = indirectAddr memory addr base

paddWithZeros :: String -> String
paddWithZeros instruction = replicate (5 - length instruction) '0' ++ instruction

decodeInstruction :: [Int] -> Int -> (Int, Int, Int, Int)
decodeInstruction memory ip = (
        read (drop 3 strCode),
        read [strCode !! 2],
        read [strCode !! 1],
        read [strCode !! 0]
    )
    where strCode = paddWithZeros . show $ memory !! ip

cmpOp :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
cmpOp op = \left right -> fromEnum $ left `op` right

computeBinaryOp :: [Int] -> Int -> Int -> (Int -> Int -> Int) -> (Int, Int, Int, Int) -> [Int]
computeBinaryOp memory base ip op (_, mod1, mod2, mod3) = take saveTo memory ++ (leftHand  `op` rightHand) : drop (saveTo + 1) memory
    where (leftHand, rightHand, saveTo) = (getParam memory (ip + 1) mod1 base, getParam memory (ip + 2) mod2 base, getParam memory (ip + 3) 1 base + (if mod3 == 2 then base else 0))

input :: [Int] -> Int -> Int-> Int -> Int -> [Int]
input memory base ip mod value = take saveTo memory ++ (value : drop (saveTo + 1) memory)
    where saveTo = (getParam memory (ip + 1) 1 0) + (if mod == 2 then base else 0)

output :: [Int] -> Int -> Int -> (Int, Int, Int, Int) -> Int
output memory base ip (_, mod1, _, _) = getParam memory (ip + 1) mod1 base

jump :: Bool -> [Int] -> Int -> Int -> (Int, Int, Int, Int) -> Int
jump ifTrue memory base ip (_, mod1, mod2, _)
    | ifTrue = if (getParam memory (ip + 1) mod1 base) /= 0 then getParam memory (ip + 2) mod2 base else ip + 3
    | otherwise = if (getParam memory (ip + 1) mod1 base) == 0 then getParam memory (ip + 2) mod2 base else ip + 3

computeNextOutput :: [Int] -> Int -> Int -> [Int] -> Maybe (Int, [Int], Int, Int, [Int])
computeNextOutput memory base ip inputs
    | opCode == 1 = computeNextOutput (computeBinaryOp memory base ip (+) instruction) base (ip + 4) inputs  -- add
    | opCode == 2 = computeNextOutput (computeBinaryOp memory base ip (*) instruction) base (ip + 4) inputs  -- mul
    | opCode == 3 = computeNextOutput (input memory base ip mod1 (head inputs)) base (ip + 2) (drop 1 inputs)     -- input
    | opCode == 4 = Just (output memory base ip instruction, memory, base, ip + 2, inputs)              -- output
    | opCode == 5 = computeNextOutput memory base (jump True memory base ip instruction) inputs            -- jmp if true
    | opCode == 6 = computeNextOutput memory base (jump False memory base ip instruction) inputs           -- jmp if false
    | opCode == 7 = computeNextOutput (computeBinaryOp memory base ip (cmpOp (<)) instruction) base (ip + 4) inputs  -- less than
    | opCode == 8 = computeNextOutput (computeBinaryOp memory base ip (cmpOp (==)) instruction) base (ip + 4) inputs  -- equals
    | opCode == 9 = computeNextOutput memory (base + getParam memory (ip + 1) mod1 base) (ip + 2) inputs  -- change base addr
    | otherwise = Nothing
        where instruction@(opCode, mod1, _, _) = decodeInstruction memory ip

computeNOutputs :: Int -> [Int] -> Int -> Int -> [Int] -> [Int] -> Maybe ([Int], [Int], Int, Int, [Int])
computeNOutputs 0 memory base ip inputs outputs = Just (reverse outputs, memory, base, ip, inputs)
computeNOutputs n memory base ip inputs outputs = case (computeNextOutput memory base ip inputs) of
    Nothing -> Just (reverse outputs, memory, base, ip, inputs)
    Just (output', memory', base', ip', inputs') -> computeNOutputs (n-1) memory' base' ip' inputs' (output':outputs)

computeProgram :: [Int] -> Int -> Int -> [Int] -> [Int] -> [Int]
computeProgram memory base ip inputs outputs = case (computeNextOutput memory base ip inputs) of
    Nothing -> reverse outputs
    Just (output', memory', base', ip', inputs') -> computeProgram memory' base' ip' inputs' (output':outputs)

computeNextOutputInteractive :: [Int] -> Int -> Int -> IO Int -> IO (Maybe (Int, [Int], Int, Int))
computeNextOutputInteractive memory base ip inputIO
    | opCode == 1 = computeNextOutputInteractive (computeBinaryOp memory base ip (+) instruction) base (ip + 4) inputIO  -- add
    | opCode == 2 = computeNextOutputInteractive (computeBinaryOp memory base ip (*) instruction) base (ip + 4) inputIO  -- mul
    | opCode == 3 = do
        value <- inputIO
        computeNextOutputInteractive (input memory base ip mod1 value) base (ip + 2) inputIO    -- input
    | opCode == 4 = return (Just (output memory base ip instruction, memory, base, ip + 2))              -- output
    | opCode == 5 = computeNextOutputInteractive memory base (jump True memory base ip instruction) inputIO            -- jmp if true
    | opCode == 6 = computeNextOutputInteractive memory base (jump False memory base ip instruction) inputIO           -- jmp if false
    | opCode == 7 = computeNextOutputInteractive (computeBinaryOp memory base ip (cmpOp (<)) instruction) base (ip + 4) inputIO  -- less than
    | opCode == 8 = computeNextOutputInteractive (computeBinaryOp memory base ip (cmpOp (==)) instruction) base (ip + 4) inputIO  -- equals
    | opCode == 9 = computeNextOutputInteractive memory (base + getParam memory (ip + 1) mod1 base) (ip + 2) inputIO  -- change base addr
    | otherwise = return Nothing
        where instruction@(opCode, mod1, _, _) = decodeInstruction memory ip

computeNOutputsInteractive :: Int -> [Int] -> Int -> Int -> IO Int -> [Int] -> IO (Maybe ([Int], [Int], Int, Int))
computeNOutputsInteractive 0 memory base ip inputIO outputs = do return(Just (reverse outputs, memory, base, ip))
computeNOutputsInteractive n memory base ip inputIO outputs = do
    results <- computeNextOutputInteractive memory base ip inputIO
    case results of
        Nothing -> return (Just (reverse outputs, memory, base, ip))
        Just (output', memory', base', ip') -> computeNOutputsInteractive (n-1) memory' base' ip' inputIO (output':outputs)

playArcade ::  [Int] -> IO ()
playArcade input = do
    ball <- newIORef (-1, -1)
    paddle <- newIORef (-1, -1)
    gameState <- newIORef (input, 0 ::Int, 0 ::Int)
    gameStep ball paddle 0 gameState

    where gameStep :: IORef (Int, Int) -> IORef (Int, Int) -> Int -> IORef ([Int], Int, Int) -> IO ()
          gameStep ball paddle dir state = do
              (memory, base, ip) <- readIORef state
              result <- computeNOutputsInteractive 3 memory base ip (do threadDelay 75000; return dir) []
              case result of
                Nothing -> return ()
                Just (outputs, memory', base', ip') -> do
                    if null outputs
                    then return ()
                    else do
                        mapToOutput outputs
                        dir <- trackBall ball paddle outputs
                        setCursorPosition 23 4
                        print $ (show dir) ++ "  "
                        writeIORef state (memory', base', ip')
                        gameStep ball paddle dir state

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
          where
            getKey' chars = do
              char <- getHiddenChar
              more <- hReady stdin
              (if more then getKey' else return) (char:chars)

mapToInput :: IO Int
mapToInput = do
    key <- getKey
    case key of
        "a" -> return (-1)
        "d" -> return (1)
        _   -> return (0)

mapToOutput :: [Int] -> IO ()
mapToOutput (x:y:val:_) =
    if x == -1
    then do
        setCursorPosition 0 0
        print $ "[ " ++ (show val) ++ " ]"
    else do
        setCursorPosition y x
        case val of
            0 -> putChar ' '
            1 -> putChar ':'
            2 -> putChar '#'
            3 -> do putChar '='
            4 -> do putChar 'O'

trackBall :: IORef (Int, Int) -> IORef (Int, Int) -> [Int] -> IO Int
trackBall ball paddle (x:y:val:_) = do

    if x /= -1
    then case val of
        3 -> do
            (px, pdx) <- readIORef paddle
            writeIORef paddle (x, delta x px)
            setCursorPosition 22 4
            print [x, delta x px]
        4 -> do
            (bx, bdx) <- readIORef ball
            writeIORef ball (x, delta x bx)
            setCursorPosition 22 22
            print [x, delta x bx]
        _ -> return ()
    else return ()
    (_, bdx) <- readIORef ball
    (_, pdx) <- readIORef paddle
    return $ case (pdx, bdx) of
        (0, m) -> -m
        (_, 0) -> 0
        (n, m) -> m

        where delta a' a = if a == -1 then 0 else a' - a

--main :: IO ()
main = do
    clearScreen
    hideCursor
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    handle <- openFile "input.txt" ReadMode
    input <- hGetContents handle  
    -- input <- getContents
    playArcade (parse input ++ replicate 128000 0)
    setCursorPosition 23 4
    print "Game over!"
    getKey
    clearScreen
    showCursor
