-- Domain Specific Languages -- Assignment 3
-- Higher-Order Functions
-- Due: Tuesday, 30 Sep 2014, 23:00

-- @author Zhenfei Nie <zhenfei.gmail.com>


-- Parser is a function, that takes a character and a string. If the character
-- parameter matches the head of the string, then the function returns the tail
-- of the string wrapped in a Maybe type. Otherwise, it returns a failure
-- with Nothing.
type Parser = String -> Maybe String

-- Returns a function that succeeds without parsing a character
emptyParser :: Parser
emptyParser = (\s -> Just s)

-- Returns a function that parses a character
makeCharParser :: Char -> Parser
makeCharParser c =
    (\c s -> case s of [] -> Nothing
                       (x:xs) -> if x == c then Just xs else Nothing) c
--makeCharParser :: Char -> ( String -> Maybe String )
--       add     ::  a    ->   ( a      -> a )
--        let addSix = add 6
--          addSix 8


-- Apply the first parser, then the second parser
makeSeqParser :: Parser -> Parser -> Parser
makeSeqParser p1 p2 =
    \s -> case (p1 s) of Nothing -> Nothing
                         Just s' ->  (p2 s')

-- Parses an alternative, not a sequence, so it will advance for a|b
makeAltParser :: Parser -> Parser -> Parser
makeAltParser p1 p2 = 
    \s -> case (p1 s) of Nothing -> (p2 s)
                         Just s' -> Just s'

-- Parses 0 or more occurrences of a pattern. You should implement this in
-- terms of the above parsers.
--makeKleeneParser :: Parser -> Parser
--makeKleeneParser p = 
--    \s -> case (p s) of Nothing -> Just s
--                        Just s' -> if s == s' then Just s else makeKleeneParser p s'

makeKleeneParser :: Parser -> Parser
makeKleeneParser p = 
    \s -> case (p s) of Nothing -> Nothing
                        Just s' -> if s == s' then Just s else if (p s') == Nothing then Just s' else makeKleeneParser p s'




-- Testing

-- TestCase of the parser, the string, expected result
type TestCase = (Parser, String, Maybe String)


msg :: TestCase -> Bool -> String
msg (_, s, r) False = 
  "Parsing \"" ++ s ++ "\"\nFailed" 
msg (_, s, r) True = 
  "Parsing \"" ++ s ++ "\"\nPassed"

-- Test cases
gh  = emptyParser
gha = makeCharParser 'a'
ghb = makeCharParser 'b'
ghc = makeCharParser 'c'
ghab = makeSeqParser gha ghb
ghgh = makeAltParser gha ghb

aSeqAb = makeSeqParser ghgh ghc

testCases :: [TestCase]
testCases = [(makeSeqParser (makeCharParser 'a') (makeCharParser 'b'), "abcc", Just "cc"),
            (makeSeqParser (makeSeqParser (makeCharParser 'd') (makeCharParser 'c')) (makeCharParser 'b'), "dcba", Just "a"),
            (makeSeqParser (makeSeqParser (makeCharParser 'd') (makeCharParser 'c')) (makeAltParser (makeCharParser 'a') (makeCharParser 'b')), "dcbdcaac", Just "dcaac"),
            (makeSeqParser (makeSeqParser (makeCharParser 'd') (makeCharParser 'c')) (makeAltParser (makeCharParser 'a') (makeCharParser 'b')), "dcadcaac", Just "dcaac"),
            (makeAltParser (makeCharParser 'a') (makeCharParser 'b'), "babcc", Just "abcc"),
            (makeAltParser (makeCharParser 'a') (makeCharParser 'b'), "aabcc", Just "abcc"),
            (makeAltParser (makeCharParser 'a') (makeCharParser 'b'), "daabcc", Nothing),
            (makeAltParser (makeCharParser 'a') (makeSeqParser (makeCharParser 'd') (makeCharParser 'c')), "ab", Just "b"),
            (makeAltParser (makeCharParser 'a') (makeSeqParser (makeCharParser 'd') (makeCharParser 'c')), "db", Nothing),
            (makeAltParser (makeCharParser 'a') (makeSeqParser (makeCharParser 'd') (makeCharParser 'c')), "dcb", Just "b"),
            (makeKleeneParser (makeCharParser 'a'), "aaabcc", Just "bcc"),
            (makeKleeneParser (makeCharParser 'a'), "baaabcc", Nothing),
            (makeKleeneParser ( makeAltParser (makeCharParser 'a') (makeCharParser 'b') ), "abaabcc", Just "cc"),
            (makeKleeneParser ( makeAltParser (makeCharParser 'a') (makeCharParser 'b') ), "cabaabcc", Nothing),
            (makeKleeneParser emptyParser, "cabaabcc", Just "cabaabcc"),
            (makeKleeneParser ( makeSeqParser (makeCharParser 'a') (makeCharParser 'b') ), "ababaabcc", Just "aabcc"),
            (makeKleeneParser (makeSeqParser (makeSeqParser (makeCharParser 'a') (makeCharParser 'b')) (makeCharParser 'c') ), "abcabcabcc", Just "c")]


test :: String
test = foldl (\z -> \y -> z ++ "\n" ++ y) "" res
  where res = map (\t -> let (p, s, r) = t in msg t (p s == r)) testCases 


main :: IO ()
main = putStrLn test



