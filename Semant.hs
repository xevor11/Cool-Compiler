module Constants where

newtype Symbol = Symbol String deriving (Show)

io :: IO ()
io = putStrLn "IO action goes here"

anySym :: Symbol
anySym = Symbol "Any"

nullSym :: Symbol
nullSym = Symbol "Null"

unitSym :: Symbol
unitSym = Symbol "Unit"

intSym :: Symbol
intSym = Symbol "Int"

nothingSym :: Symbol
nothingSym = Symbol "Nothing"

booleanSym :: Symbol
booleanSym = Symbol "Boolean"

stringSym :: Symbol
stringSym = Symbol "String"

arrayAnySym :: Symbol
arrayAnySym = Symbol "ArrayAny"

ioSym :: Symbol
ioSym = Symbol "IO"

nativeSym :: Symbol
nativeSym = Symbol "native"

mainSym :: Symbol
mainSym = Symbol "Main"

symbolSym :: Symbol
symbolSym = Symbol "Symbol"

thisSym :: Symbol
thisSym = Symbol "this"

superSym :: Symbol
superSym = Symbol "super"

basicCoolSym :: Symbol
basicCoolSym = Symbol "/usr/local/lib/basic.cool"

invalidTypes :: [Symbol]
invalidTypes = [nothingSym, nullSym, nativeSym]
