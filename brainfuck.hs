
import Data.Char
import Data.Word

executeString :: String -> String -> Maybe String
executeString source input = process (0,0, repeat (0 :: Word8), [], input)
    where
        -- ip - instruction pointer
        -- dp - data pointer
        -- reg - data registry
        -- out - output
        process :: (Int,Int,[Word8],[Word8],String) -> Maybe String
        process (ip,dp,reg,out,input)
            | ip >= length source = Just (toChars out)
            | otherwise           = case op of
                '>' -> process (ip', dp+1, reg, out, input)
                '<' -> process (ip', dp-1, reg, out, input)
                '+' -> process (ip', dp, inc dp reg, out, input)
                '-' -> process (ip', dp, dec dp reg, out, input)
                '.' -> process (ip', dp, reg, out ++ [get dp reg], input)
                '[' -> startLoop
                ']' -> endLoop
                ',' -> if null input then Nothing else process (ip', dp, set dp (fromIntegral (ord c) :: Word8) reg, out, input')
                where
                    op         = source !! ip
                    ip'        = ip+1
                    (c,input') = (head input, tail input)            
                    
                    startLoop | get dp reg == 0 = process (ip'', dp, reg, out, input)
                              | otherwise       = process (ip', dp, reg, out, input)
                              where
                                ip'' = loopRange (drop ip source) + ip'

                    endLoop | get dp reg == 0 = process (ip', dp, reg, out, input)
                            | otherwise       = process (ip'', dp, reg, out, input)
                            where                            
                                ip'' = ip' - loopRange (reverse $ take ip' source)

        toChar :: Word8 -> Char
        toChar = chr . fromIntegral

        toChars :: [Word8] -> String
        toChars = map toChar

        update :: Int -> [Word8] -> (Word8 -> Word8) -> [Word8]
        update i xs f = ys ++ [v] ++ zs
            where
                ys = take i xs
                zs = tail $ drop i xs
                v  = f (head $ drop i xs)

        inc :: Int -> [Word8] -> [Word8]
        inc i xs = update i xs (+1)

        dec :: Int -> [Word8] -> [Word8]
        dec i xs = update i xs (subtract 1)

        set :: Int -> Word8 -> [Word8] -> [Word8]
        set i v xs = update i xs (const v)

        get :: Int -> [Word8] -> Word8
        get i xs = xs !! i

        loopRange :: String -> Int
        loopRange str = find str 0 0
            where
                find [] n i
                    | i == 0    = n-1
                    | otherwise = error "Incorrect loop"
                find (x:xs) n i
                    | x == '['  = find xs (n+1) (i+1)
                    | x == ']'  = find xs (n+1) (i-1)
                    | i == 0    = n-1
                    | otherwise = find xs (n+1) i

-- Example
src = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+."
ex1 = executeString src "" == Just "Hello World!"
