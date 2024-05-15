{
main :: IO ()
main = do
    input <- TIO.getContents
    let tokens = alexScanTokens input
    print tokens
}

tokenText :: AlexInput -> String
tokenText (c, s) = [c] ++ takeWhile (not . isSpace) s