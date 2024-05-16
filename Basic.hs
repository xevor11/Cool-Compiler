-- Basic classes for Cool 2022
-- John Boyland
-- January 2022

-- This file includes classes which are treated specially by the compiler.
-- Native features are implemented in the Cool runtime system,
-- and are only permitted for features defined in this file.

-- Classes with native attributes 
-- (Unit, Int, Boolean, String, Symbol and ArrayAny)
-- may not be inherited from.

-- The Any class is the root of the inheritance hierarchy.
data Any = Any deriving (Show, Eq)

-- Returns a string representation for the object
toString :: Any -> String
toString = error "native"

-- Return true if this object is equal (in some sense) to the argument
equals :: Any -> Any -> Bool
equals = error "native"

-- The IO class provides simple input and output operations
data IO = IO deriving (Show, Eq)

-- Terminates program with given message.
abort :: String -> a
abort = error "native"

-- Print the argument (without quotes) to stdout and return itself
out :: String -> IO
out = error "native"

isNull :: Any -> Bool
isNull arg = case arg of
  null -> True
  _    -> False

-- Convert to a string and print
outAny :: Any -> IO
outAny arg = out (if isNull arg then "null" else toString arg)

-- Read and return characters from stdin to the next newline character. 
-- Return null on end of file.
inStr :: IO String
inStr = error "native"

-- Get the symbol for this string, creating a new one if needed.
symbol :: String -> Symbol
symbol = error "native"

-- Return the string associated with this symbol.
symbolName :: Symbol -> String
symbolName = error "native"

-- Return the number of arguments from the command line
getArgC :: IO Int
getArgC = error "native"

-- Return the argument from the command line. 0 = first argument
getArg :: Int -> IO String
getArg = error "native"

-- A class with no subclasses and which has only one instance.
-- It cannot be instantiated of inherited.
-- The null pointer is not legal for Unit.
data Unit = Unit deriving (Show, Eq)

-- The class of integers in the range -2^31 .. (2^31)-1
-- null is not a legal value for integers, and Int can have no subclasses.
data Int = Int { value :: Integer } deriving (Show, Eq)

-- Convert to a string representation
intToString :: Int -> String
intToString = error "native"

-- Return true if the argument is an int with the same value
intEquals :: Int -> Any -> Bool
intEquals = error "native"

-- The class of booleans with two legal values: true and false.
-- null is not a legal boolean.
-- It is illegal to inherit from Boolean.
data Boolean = Boolean { value :: Bool } deriving (Show, Eq)

-- Convert to a string representation
boolToString :: Boolean -> String
boolToString b = if value b then "true" else "false"

-- Return true if the argument is a boolean with the same value
boolEquals :: Boolean -> Any -> Bool
boolEquals = error "native"

-- The class of strings: fixed sequences of characters.
-- Unlike previous version of Cool, strings may be null.
-- It is illegal to inherit from String.
data CoolString = CoolString
  { lengthStr :: Int
  , strField  :: String
  } deriving (Show, Eq)

stringToString :: CoolString -> String
stringToString = strField

-- Return true if the argument is a string with the same characters.
stringEquals :: CoolString -> Any -> Bool
stringEquals = error "native"

-- Return length of string.
strLength :: CoolString -> Int
strLength = lengthStr

-- Return (new) string formed by concatenating self with the argument
concatStr :: CoolString -> CoolString -> CoolString
concatStr = error "native"

-- Returns the substring of self beginning at position start to position end (exclusive)
-- A runtime error is generated if the specified substring is out of range.
substring :: CoolString -> Int -> Int -> CoolString
substring = error "native"

-- Return the character at the given index of the string as an integer.
charAt :: CoolString -> Int -> Int
charAt = error "native"

-- Return the first index of given substring in this string, or -1 if no such substring.
indexOf :: CoolString -> CoolString -> Int
indexOf sub str = let
  n = strLength sub
  diff = lengthStr str - n
  findIndex i
    | i > diff = -1
    | substring str i (i+n) == sub = i
    | otherwise = findIndex (i+1)
  in findIndex 0

-- A symbol is an interned string---two symbols with the same string are always identically the same object.
-- Creating symbols is restricted to ensure the uniqueness properties.
data Symbol = Symbol
  { next :: Symbol
  , name :: String
  , hash :: Int
  } deriving (Show, Eq)

symbolToString :: Symbol -> String
symbolToString sym = "'" ++ name sym

hashCode :: Symbol -> Int
hashCode = hash

-- An array is a mutable fixed-size container holding any objects.
-- The elements are numbered from 0 to size-1.
-- An array may be void. It is not legal to inherit from ArrayAny.
data ArrayAny = ArrayAny
  { lengthArray :: Int
  , arrayField  :: [Any]
  } deriving (Show, Eq)

-- Return length of array.
arrayLength :: ArrayAny -> Int
arrayLength = lengthArray

-- Return a new array of size s (the original array is unchanged).
resizeArray :: ArrayAny -> Int -> ArrayAny
resizeArray = error "native"

-- Returns the entry at location index.
-- precondition: 0 <= index < length()
getArray :: ArrayAny -> Int -> Any
getArray = error "native"

-- Change the entry at location index.
-- Return the old value, if any (or null).
-- precondition: 0 <= index < length()
setArray :: ArrayAny -> Int -> Any -> Any
setArray = error "native"

-- Statistics gathering for SPIM. These only work with spim-8.0 and later with JTB stats extensions.
data Statistics = Statistics deriving (Show, Eq)

clearStats :: Statistics -> Statistics
clearStats = error "native"

getStat :: Statistics -> Int -> Int
getStat = error "native"

printStats :: Statistics -> Statistics
printStats = error "native"
