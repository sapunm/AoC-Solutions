parse :: String -> [Integer]
parse input = map read (lines input)

calcFuel :: Integer -> Integer
calcFuel mass
    | fuelMass < 0 = 0
    | otherwise = fuelMass + calcFuel(fuelMass)
        where fuelMass = mass `div` 3 - 2

main :: IO ()
main = do
    input <- getContents
    print $ sum (map calcFuel (parse input))
