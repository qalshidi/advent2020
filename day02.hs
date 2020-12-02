{-
--- Day 2: Password Philosophy ---

Your flight departs in a few days from the coastal airport; the easiest way
down to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad
day. "Something's wrong with our computers; we can't log in!" You ask if you
can take a look.

Their password database seems to be a little corrupted: some of the
passwords wouldn't have been allowed by the Official Toboggan Corporate
Policy that was in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle input)
of passwords (according to the corrupted database) and the corporate policy
when that password was set.

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc

Each line gives the password policy and then the password. The password
policy indicates the lowest and highest number of times a given letter
must appear for the password to be valid. For example, 1-3 a means that
the password must contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is
not; it contains no instances of b, but needs at least 1. The first and
third passwords are valid: they contain one a or nine c, both within the
limits of their respective policies.

How many passwords are valid according to their policies?

--- Part Two ---

While it appears you validated the passwords correctly, they don't seem to 
be what the Official Toboggan Corporate Authentication System is expecting.

The shopkeeper suddenly realizes that he just accidentally explained the 
password policy rules from his old job at the sled rental place down the 
street! The Official Toboggan Corporate Policy actually works a little 
differently.

Each policy actually describes two positions in the password, where 1 means 
the first character, 2 means the second character, and so on. (Be careful; 
Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of 
these positions must contain the given letter. Other occurrences of the 
letter are irrelevant for the purposes of policy enforcement.

Given the same example list from above:

    1-3 a: abcde is valid: position 1 contains a and position 3 does not.
    1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
    2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.

How many passwords are valid according to the new interpretation of the policies?

-}

import qualified Data.Text as T
import           System.IO

testCase = [ "1-3 a: abcde"
           , "1-3 b: cdefg"
           , "2-9 c: ccccccccc"
           ]

-- Record data type for each line in the database
data Record = Record Int Int Char T.Text
  deriving Show

-- Min-Max rule
validateV1 :: Record -> Bool
validateV1 (Record minCount maxCount charPolicy password)
  | minCount <= count && count <= maxCount = True
  | otherwise = False
    where
        count = T.length $ T.filter (==charPolicy) password

-- Exactly 1 in the position rule
validateV2 :: Record -> Bool
validateV2 (Record pos1 pos2 charPolicy password)
  = (pass !! (pos1-1) == charPolicy) `xor` (pass !! (pos2-1) == charPolicy)
  where
    pass = T.unpack password
    xor True  False = True
    xor False True  = True
    xor _ _         = False

-- Turns a string into a Record type
parse :: String -> Record
parse t = Record minCount maxCount charPolicy password
  where
    line       = T.pack t
    parts      = T.words line
    counts     = T.splitOn (T.pack "-") (head parts)
    minCount   = read . T.unpack . head $ counts :: Int
    maxCount   = read . T.unpack . last $ counts :: Int
    charPolicy = head . T.unpack $ parts !! 1 :: Char
    password   = last parts

main :: IO ()
main = do
  raw <- readFile "day02.in"
  let contents = lines raw
  print "Validation Version 1:"
  let validated1DB = map (validateV1 . parse) contents
  print $ length (filter id validated1DB)
  print "Validation Version 2:"
  let validated2DB = map (validateV2 . parse) contents
  print $ length (filter id validated2DB)
