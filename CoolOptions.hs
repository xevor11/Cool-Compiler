module CoolOptions where

data CoolOptions = CoolOptions
  { scanDebug            :: Bool
  , parseDebug           :: Bool
  , semantDebug          :: Bool
  , analysisDebug        :: Bool
  , cgenDebug            :: Bool
  , scan                 :: Bool
  , parse                :: Bool
  , semant               :: Bool
  , analyze              :: Bool
  , optimize             :: Bool
  , codegen              :: Bool
  , enableGc             :: Bool
  , rightToLeftChildren  :: Bool
  , genC                 :: Bool
  , basicFile            :: String
  , analysis             :: String
  , outFilename          :: Maybe String
  , maxErrors            :: Int
  , stripAttributes      :: Int
  } deriving (Show, Eq)

defaultCoolOptions :: CoolOptions
defaultCoolOptions = CoolOptions
  { scanDebug = False
  , parseDebug = False
  , semantDebug = False
  , analysisDebug = False
  , cgenDebug = False
  , scan = False
  , parse = True
  , semant = True
  , analyze = False
  , optimize = False
  , codegen = True
  , enableGc = False
  , rightToLeftChildren = False
  , genC = False
  , basicFile = "/usr/local/lib/basic.cool"
  , analysis = "resolve:inline:dead-let"
  , outFilename = Nothing
  , maxErrors = 50
  , stripAttributes = 0
  }

-- Getters
getScanDebug :: CoolOptions -> Bool
getScanDebug = scanDebug

getParseDebug :: CoolOptions -> Bool
getParseDebug = parseDebug

getSemantDebug :: CoolOptions -> Bool
getSemantDebug = semantDebug

getAnalysisDebug :: CoolOptions -> Bool
getAnalysisDebug = analysisDebug

getCgenDebug :: CoolOptions -> Bool
getCgenDebug = cgenDebug

getScan :: CoolOptions -> Bool
getScan = scan

getParse :: CoolOptions -> Bool
getParse = parse

getSemant :: CoolOptions -> Bool
getSemant = semant

getAnalyze :: CoolOptions -> Bool
getAnalyze = analyze

getOptimize :: CoolOptions -> Bool
getOptimize = optimize

getCodegen :: CoolOptions -> Bool
getCodegen = codegen

getEnableGc :: CoolOptions -> Bool
getEnableGc = enableGc

getRightToLeftChildren :: CoolOptions -> Bool
getRightToLeftChildren = rightToLeftChildren

getGenC :: CoolOptions -> Bool
getGenC = genC

getBasicFile :: CoolOptions -> String
getBasicFile = basicFile

getAnalysis :: CoolOptions -> String
getAnalysis = analysis

getOutFilename :: CoolOptions -> Maybe String
getOutFilename = outFilename

getMaxErrors :: CoolOptions -> Int
getMaxErrors = maxErrors

getStripAttributes :: CoolOptions -> Int
getStripAttributes = stripAttributes

setScanDebug :: Bool -> CoolOptions -> CoolOptions
setScanDebug val opts = opts { scanDebug = val }

setParseDebug :: Bool -> CoolOptions -> CoolOptions
setParseDebug val opts = opts { parseDebug = val }

setSemantDebug :: Bool -> CoolOptions -> CoolOptions
setSemantDebug val opts = opts { semantDebug = val }

setAnalysisDebug :: Bool -> CoolOptions -> CoolOptions
setAnalysisDebug val opts = opts { analysisDebug = val }

setCgenDebug :: Bool -> CoolOptions -> CoolOptions
setCgenDebug val opts = opts { cgenDebug = val }

setScan :: Bool -> CoolOptions -> CoolOptions
setScan val opts = opts { scan = val }

setParse :: Bool -> CoolOptions -> CoolOptions
setParse val opts = opts { parse = val }

setSemant :: Bool -> CoolOptions -> CoolOptions
setSemant val opts = opts { semant = val }

setAnalyze :: Bool -> CoolOptions -> CoolOptions
setAnalyze val opts = opts { analyze = val }

setOptimize :: Bool -> CoolOptions -> CoolOptions
setOptimize val opts = opts { optimize = val }

setCodegen :: Bool -> CoolOptions -> CoolOptions
setCodegen val opts = opts { codegen = val }

setEnableGc :: Bool -> CoolOptions -> CoolOptions
setEnableGc val opts = opts { enableGc = val }

setRightToLeftChildren :: Bool -> CoolOptions -> CoolOptions
setRightToLeftChildren val opts = opts { rightToLeftChildren = val }

setGenC :: Bool -> CoolOptions -> CoolOptions
setGenC val opts = opts { genC = val }

setBasicFile :: String -> CoolOptions -> CoolOptions
setBasicFile val opts = opts { basicFile = val }

setAnalysis :: String -> CoolOptions -> CoolOptions
setAnalysis val opts = opts { analysis = val }

setOutFilename :: Maybe String -> CoolOptions -> CoolOptions
setOutFilename val opts = opts { outFilename = val }

setMaxErrors :: Int -> CoolOptions -> CoolOptions
setMaxErrors val opts = opts { maxErrors = val }

setStripAttributes :: Int -> CoolOptions -> CoolOptions
setStripAttributes val opts = opts { stripAttributes = val }
