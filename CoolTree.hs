{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Parser.AST where

import Parser.ParserTerminalNode
import Basic

data CoolNode
    = CoolNode { get_id :: Int
               , set_id :: Unit
               , get_line_number :: Int
               , set_line_number :: Unit
               , toString :: String
               , accept :: Any
    }
    | Program Program
    | Class Class
    | Classes Classes
    | Feature Feature
    | Features Features
    | Formal Formal
    | Formals Formals
    | Expression Expression
    | Expressions Expressions
    | Case Case
    | Cases Cases
    deriving (Show, Read, Eq)

data Program
    = Program { getClasses :: [Class] }
  | Cprogram {
            get_name :: Symbol,
            set_name :: Unit,
            get_parent :: Symbol,
            set_parent :: Unit,
            get_features :: Features,
            set_features :: Unit,
            get_filename :: Symbol,
            set_filename :: Unit,
            accept :: Any
  } deriving (Show, Read, Eq)

data Class
    = Class { getClassName :: Symbol
            , getInheritName :: Symbol
            , getFeatures :: [Feature]
            }
    | Cclass_decl {
            get_name :: Symbol,
            set_name :: Unit,
            get_parent :: Symbol,
            set_parent :: Unit,
            get_features :: Features,
            set_features :: Unit,
            get_filename :: Symbol,
            set_filename :: Unit,
            accept :: Any
  } deriving (Show, Read, Eq)

data Classes
  = Classes {size :: Symbol,
             nth :: Class,
             concat :: [Class],
             addcop :: [Class],
             elements :: ClassEnumeration} |
    ClassesNil {concat :: Symbol} |
    ClassesOne {get :: Class,
                size :: Int,
                nth :: Class,
                accept :: Any} |
    ClassesAppend {
                get1 :: [Class],
                get2 :: [Class],
                size :: Int,
                nth :: Class,
                accept :: Any
    } deriving (Show, Read, Eq)

data ClassesEnumeration = ClassEnumeration
    {
        hasNext :: Boolean,
        next :: Class
    } deriving (Show, Read, Eq)

data Feature
    = Feature { get_owner :: Cclass_decl
              , set_owner :: Unit
              , get_overrides :: Unit
              , get_feature_of_class :: Class
              , set_feature_of_class :: Unit
              , getInitExpr :: Maybe Expression }
    | Cmethod { get_overridep :: Boolean
              , set_overridep :: Unit
              , get_name :: Symbol
              , set_name :: Unit
              , get_formals :: Formals
              , set_formals :: Unit
              , get_return_type :: Symbol
              , set_return_type :: Unit
              , get_expr :: Expression
              , set_expr :: Unit
              , accept :: Any
    }
    | Cattr { get_name :: Symbol
            , set_name :: Unit
            , get_of_type :: Symbol
            , set_of_type :: Unit
            , accept :: Any
    } deriving (Show, Read, Eq)

data Features
    = Features {
                size :: Int,
                nth :: Feature,
                concat :: [Feature],
                elements :: FeaturesEnumeration
    }
    | Features_nil {
                concat :: [Feature]
    }
    | Features_one {
                get :: Feature,
                size :: Int,
                nth :: Feature,
                accept :: Any
    }
    | Features_append {
                get1 :: [Feature],
                get2 :: [Feature],
                size :: Int,
                nth :: Feature,
                accept :: Any
    }  deriving (Show, Read, Eq)

data FeaturesEnumeration
    = FeaturesEnumeration {
        hasNext :: Boolean,
        next :: Feature
    } deriving (Show, Read, Eq)

data Formal
    = Formal { get_formal_of_class :: Class
             , set_formal_of_class :: Unit
    }
    | Cformal { get_name :: Symbol
              , set_name :: Unit
              , get_of_type :: Symbol
              , set_of_type :: Unit
              , accept :: Any
  } deriving (Show, Read, Eq)

data Formals
    = Formals {
                size :: Int,
                nth :: Formal,
                concat :: [Formal],
                elements :: FormalsEnumeration
    }
    | Formals_nil {
                concat :: [Formal]
    }
    | Formals_one {
                get :: Formal,
                size :: Int,
                nth :: Formal,
                accept :: Any
    }
    | Formals_append {
                get1 :: [Formal],
                get2 :: [Formal],
                size :: Int,
                nth :: Formal,
                accept :: Any
    }  deriving (Show, Read, Eq)

data Expression
  = Expression {
            get_of_type :: Symbol,
            set_of_type :: Unit,
            get_of_class :: Class,
            set_of_class :: Unit,
            get_binding :: CoolNode,
            set_binding :: Unit,
            get_mbinding :: Cmethod,
            set_mbinding :: Unit
  }  | Cassign { get_name :: Symbol
            , set_name :: Unit
            , get_expr :: Expression
            , set_expr :: Unit
            , accept :: Any
  }  | Cmethod { get_name :: Symbol
            , set_name :: Unit
            , get_expr :: Expression
            , set_expr :: Unit
            , accept :: Any
  }
  | Cstatic_dispatch { get_expr :: Expression
                     , set_expr :: Unit
                     , get_type_name :: Symbol
                     , set_type_name :: Unit
                     , get_name :: Symbol
                     , set_name :: Symbol
                     , get_actuals :: Expression
                     , set_actuals :: Unit
  }
  | Cdispatch { get_expr :: Expression
              , set_expr :: Unit
              , get_name :: Symbol
              , set_name :: Unit
              , get_actuals :: Expressions
              , set_actuals :: Unit
  }
  | Ccond { get_pred :: Expression
          , set_pred :: Unit
          , get_then_exp :: Expression
          , set_then_exp :: Unit
          , get_else_exp :: Expression
          , set_else_exp :: Unit
  }
  | Cloop { get_pred :: Expression
          , set_pred :: Unit
          , get_body :: Expression
          , set_body :: Unit
  }
  | Ctypecase { get_expr :: Expression
              , set_expr :: Unit
              , get_cases :: Cases
              , set_cases :: Unit
              , accept :: Any
  }
  | Cblock { get_body :: Expressions
           , set_body :: Unit
           , accept :: Any
  }
  | Clet { get_identifier :: Symbol
         , set_identifier :: Unit
         , get_local_type :: Symbol
         , set_local_type :: Unit
         , get_init :: Expression
         , set_init :: Unit
         , get_body :: Expression
         , set_body :: Unit
         , accept :: Any
  }
  | Cadd { get_e1 :: Expression
         , set_e1 :: Unit
         , get_e2 :: Expression
         , set_e2 :: Unit
         , accept :: Any
  }
  | Csub { get_e1 :: Expression
         , set_e1 :: Unit
         , get_e2 :: Expression
         , set_e2 :: Unit
         , accept :: Any
  }
  | Cmul { get_e1 :: Expression
         , set_e1 :: Unit
         , get_e2 :: Expression
         , set_e2 :: Unit
         , accept :: Any
  }
  | Cdiv { get_e1 :: Expression
         , set_e1 :: Unit
         , get_e2 :: Expression
         , set_e2 :: Unit
         , accept :: Any
  }
  | Cneg { get_e1 :: Expression
         , set_e1 :: Unit
         , accept :: Any
  }
  | Clt { get_e1 :: Expression
         , set_e1 :: Unit
         , get_e2 :: Expression
         , set_e2 :: Unit
         , accept :: Any
  }
  | Cleq { get_e1 :: Expression
         , set_e1 :: Unit
         , get_e2 :: Expression
         , set_e2 :: Unit
         , accept :: Any
  }
  | Ccomp { get_e1 :: Expression
          , set_e1 :: Unit
          , accept :: Any
  }
  | Cint_lit { get_token :: Symbol
             , set_token :: Unit
             , accept :: Any
  }
  | Cbool_lit { get_value :: Boolean
              , set_value :: Unit
              , accept :: Any
  }
  | Cstring_lit { get_token :: Symbol
             , set_token :: Unit
             , accept :: Any
  }
  | Calloc { get_type_name :: Symbol
           , set_type_name :: Unit
           , accept :: Any
  }
  | Cnil { accept :: Any }
  | Cunit { accept :: Any }
  | Cno_expr { accept :: Any }
  | Cvariable { get_name :: Symbol
              , set_name :: Unit
              , accept :: Any
  }
  deriving (Show, Read, Eq)

data Expressions
    = Expressions {
                size :: Int,
                nth :: Expression,
                concat :: [Expression],
                elements :: ExpressionsEnumeration
    }
    | Expressions_nil {
                concat :: [Expression]
    }
    | Expressions_one {
                get :: Expression,
                size :: Int,
                nth :: Expression,
                accept :: Any
    }
    | Expressions_append {
                get1 :: [Expression],
                get2 :: [Expression],
                size :: Int,
                nth :: Expression,
                accept :: Any
    }  deriving (Show, Read, Eq)


data Case
    = Case { get_case_of_type :: Symbol
           , set_case_of_type :: Unit
           , get_case_of_class :: Class
           , set_case_of_class :: Unit
    }
    | Cbranch { get_name :: Symbol
              , set_name :: Unit
              , get_local_type :: Symbol
              , set_local_type :: Unit
              , get_expr :: Expression
              , set_expr :: Unit
              , accept :: Any
  } deriving (Show, Read, Eq)

data Cases
    = Cases {
                size :: Int,
                nth :: Case,
                concat :: [Case],
                elements :: CasesEnumeration
    }
    | Cases_nil {
                concat :: [Case]
    }
    | Cases_one {
                get :: Case,
                size :: Int,
                nth :: Case,
                accept :: Any
    }
    | Cases_append {
                get1 :: [Case],
                get2 :: [Case],
                size :: Int,
                nth :: Case,
                accept :: Any
    }  deriving (Show, Read, Eq)

-- Methods

-- CoolNode Methods
data CoolNodeMethods = CoolNodeMethods
  { getId :: CoolNode -> Int
  , setId :: Int -> CoolNode -> CoolNode
  , getLineNumber :: CoolNode -> Int
  , setLineNumber :: Int -> CoolNode -> CoolNode
  , toString :: CoolNode -> String
  , accept :: forall v a. CoolVisitor v => v -> CoolNode -> a
  }

defaultCoolNodeMethods :: CoolNodeMethods
defaultCoolNodeMethods = CoolNodeMethods
  { getId = id
  , setId = \i node -> node { id = i }
  , getLineNumber = lineNumber
  , setLineNumber = \i node -> node { lineNumber = i }
  , toString = \node -> "@" ++ show node
  , accept = \_ _ -> error "accept method not implemented"
  }

-- Program Methods
data ProgramMethods = ProgramMethods
  { getAnyClass :: Program -> Maybe Class
  , setAnyClass :: Class -> Program -> Program
  , getUnitClass :: Program -> Maybe Class
  , setUnitClass :: Class -> Program -> Program
  , getIntClass :: Program -> Maybe Class
  , setIntClass :: Class -> Program -> Program
  , getBooleanClass :: Program -> Maybe Class
  , setBooleanClass :: Class -> Program -> Program
  , getStringClass :: Program -> Maybe Class
  , setStringClass :: Class -> Program -> Program
  }

-- Default Program methods implementation
defaultProgramMethods :: ProgramMethods
defaultProgramMethods = ProgramMethods
  { getAnyClass = anyClass
  , setAnyClass = \c prog -> prog { anyClass = Just c }
  , getUnitClass = unitClass
  , setUnitClass = \c prog -> prog { unitClass = Just c }
  , getIntClass = intClass
  , setIntClass = \c prog -> prog { intClass = Just c }
  , getBooleanClass = booleanClass
  , setBooleanClass = \c prog -> prog { booleanClass = Just c }
  , getStringClass = stringClass
  , setStringClass = \c prog -> prog { stringClass = Just c }
  }

-- Class Methods
data ClassMethods = ClassMethods
  { getSuperclass :: Class -> Maybe Class
  , setSuperclass :: Class -> Class -> Class
  , getInheritablep :: Class -> Bool
  , setInheritablep :: Bool -> Class -> Class
  }

defaultClassMethods :: ClassMethods
defaultClassMethods = ClassMethods
  { getSuperclass = superclass
  , setSuperclass = \sc cls -> cls { superclass = Just sc }
  , getInheritablep = inheritablep
  , setInheritablep = \inh cls -> cls { inheritablep = inh }
  }

-- Classes Methods
data ClassesMethods = ClassesMethods
  { size :: Classes -> Int
  , nth :: Classes -> Int -> Class
  , concatClasses :: Classes -> Classes -> Classes
  , addCopy :: Classes -> Class -> Classes
  , elements :: Classes -> ClassesEnumeration
  }

defaultClassesMethods :: ClassesMethods
defaultClassesMethods = ClassesMethods
  { size = const 0
  , nth = \_ _ -> error "abort: no more elements"
  , concatClasses = \c1 c2 -> Classes defaultClassesMethods (CoolNode 0 0 defaultCoolNodeMethods)
  , addCopy = \c e -> concatClasses c (ClassesOne e `asClasses`)
  , elements = ClassesEnumeration
  }

-- Classes_nil Methods
nilClassesMethods :: ClassesMethods
nilClassesMethods = defaultClassesMethods
  { concatClasses = \_ l -> l }

-- Classes_one Methods
classesOneMethods :: ClassesMethods
classesOneMethods = defaultClassesMethods
  { size = const 1
  , nth = \c i -> if i == 0 then getClass (classesData c) else nth defaultClassesMethods c i
  , accept = \v c -> visitClassesOne v (getClass $ classesData c)
  }

-- Classes_append Methods
classesAppendMethods :: ClassesMethods
classesAppendMethods = defaultClassesMethods
  { size = \c -> let (ClassesAppend l1 l2) = classesData c in size (classesMethods l1) l1 + size (classesMethods l2) l2
  , nth = \c i -> let (ClassesAppend l1 l2) = classesData c in
                   let n1 = size (classesMethods l1) l1 in
                   if i < n1 then nth (classesMethods l1) l1 i else nth (classesMethods l2) l2 (i - n1)
  , accept = \v c -> let (ClassesAppend l1 l2) = classesData c in
                     visitClassesAppend v l1 l2
  }

-- ClassesEnumeration Methods
data ClassesEnumerationMethods = ClassesEnumerationMethods
  { hasNext :: ClassesEnumeration -> Bool
  , next :: ClassesEnumeration -> (ClassesEnumeration, Class)
  }

defaultClassesEnumerationMethods :: ClassesEnumerationMethods
defaultClassesEnumerationMethods = ClassesEnumerationMethods
  { hasNext = \e -> i e + 1 < n e
  , next = \e -> let i' = i e + 1
                     caseResult = getArray (a e) i'
                 in (e { i = i' }, caseResult)
  }

-- Feature Methods
data FeatureMethods = FeatureMethods
  { getOwner :: Feature -> Maybe Cclass_decl
  , setOwner :: Cclass_decl -> Feature -> Feature
  , getOverrides :: Feature -> Maybe Cmethod
  , setOverrides :: Cmethod -> Feature -> Feature
  , getFeatureOfClass :: Feature -> Maybe Class
  , setFeatureOfClass :: Class -> Feature -> Feature
  }

defaultFeatureMethods :: FeatureMethods
defaultFeatureMethods = FeatureMethods
  { getOwner = owner
  , setOwner = \o f -> f { owner = Just o }
  , getOverrides = overrides
  , setOverrides = \o f -> f { overrides = Just o }
  , getFeatureOfClass = featureOfClass
  , setFeatureOfClass = \c f -> f { featureOfClass = Just c }
  }

-- Features Methods
data FeaturesMethods = FeaturesMethods
  { size :: Features -> Int
  , nth :: Features -> Int -> Feature
  , concatFeatures :: Features -> Features -> Features
  , addCopy :: Features -> Feature -> Features
  , elements :: Features -> FeaturesEnumeration
  }

defaultFeaturesMethods :: FeaturesMethods
defaultFeaturesMethods = FeaturesMethods
  { size = const 0
  , nth = \_ _ -> error "abort: no more elements"
  , concatFeatures = \f1 f2 -> asFeatures (FeaturesAppend f1 f2)
  , addCopy = \f e -> concatFeatures f (asFeatures (FeaturesOne e))
  , elements = FeaturesEnumeration
  }

-- Features_nil Methods
featuresNilMethods :: FeaturesMethods
featuresNilMethods = defaultFeaturesMethods
  { concatFeatures = \_ l -> l
  }

-- Features_one Methods
featuresOneMethods :: FeaturesMethods
featuresOneMethods = defaultFeaturesMethods
  { size = const 1
  , nth = \f i -> if i == 0 then getFeature (featuresData f) else nth defaultFeaturesMethods f i
  , accept = \v f -> visitFeaturesOne v (getFeature $ featuresData f)
  }

-- Features_append Methods
featuresAppendMethods :: FeaturesMethods
featuresAppendMethods = defaultFeaturesMethods
  { size = \f -> let (FeaturesAppend l1 l2) = featuresData f in size (featuresMethods l1) l1 + size (featuresMethods l2) l2
  , nth = \f i -> let (FeaturesAppend l1 l2) = featuresData f in
                   let n1 = size (featuresMethods l1) l1 in
                   if i < n1 then nth (featuresMethods l1) l1 i else nth (featuresMethods l2) l2 (i - n1)
  , accept = \v f -> let (FeaturesAppend l1 l2) = featuresData f in
                     visitFeaturesAppend v l1 l2
  }

-- FeaturesEnumeration Methods
data FeaturesEnumerationMethods = FeaturesEnumerationMethods
  { hasNext :: FeaturesEnumeration -> Bool
  , next :: FeaturesEnumeration -> (FeaturesEnumeration, Feature)
  }

defaultFeaturesEnumerationMethods :: FeaturesEnumerationMethods
defaultFeaturesEnumerationMethods = FeaturesEnumerationMethods
  { hasNext = \e -> i e + 1 < n e
  , next = \e -> let i' = i e + 1
                     caseResult = getArray (a e) i'
                 in (e { i = i' }, caseResult)
  }

-- Formal Methods
data FormalMethods = FormalMethods
  { getFormalOfClass :: Formal -> Maybe Class
  , setFormalOfClass :: Class -> Formal -> Formal
  }

defaultFormalMethods :: FormalMethods
defaultFormalMethods = FormalMethods
  { getFormalOfClass = formalOfClass
  , setFormalOfClass = \c f -> f { formalOfClass = Just c }
  }

-- Formals Methods
data FormalsMethods = FormalsMethods
  { size :: Formals -> Int
  , nth :: Formals -> Int -> Formal
  , concatFormals :: Formals -> Formals -> Formals
  , addCopy :: Formals -> Formal -> Formals
  , elements :: Formals -> FormalsEnumeration
  , accept :: forall v a. CoolVisitor v => v -> Formals -> a
  }

defaultFormalsMethods :: FormalsMethods
defaultFormalsMethods = FormalsMethods
  { size = const 0
  , nth = \_ _ -> error "abort: no more elements"
  , concatFormals = \f1 f2 -> asFormals (FormalsAppend f1 f2)
  , addCopy = \f e -> concatFormals f (asFormals (FormalsOne e))
  , elements = FormalsEnumeration
  , accept = \_ _ -> error "accept method not implemented"
  }

-- Formals_nil Methods
formalsNilMethods :: FormalsMethods
formalsNilMethods = defaultFormalsMethods
  { concatFormals = \_ l -> l
  }

-- Formals_one Methods
formalsOneMethods :: FormalsMethods
formalsOneMethods = defaultFormalsMethods
  { size = const 1
  , nth = \f i -> if i == 0 then getFormal (formalsData f) else nth defaultFormalsMethods f i
  , accept = \v f -> visitFormalsOne v (getFormal $ formalsData f)
  }

-- Formals_append Methods
formalsAppendMethods :: FormalsMethods
formalsAppendMethods = defaultFormalsMethods
  { size = \f -> let (FormalsAppend l1 l2) = formalsData f in size (formalsMethods l1) l1 + size (formalsMethods l2) l2
  , nth = \f i -> let (FormalsAppend l1 l2) = formalsData f in
                   let n1 = size (formalsMethods l1) l1 in
                   if i < n1 then nth (formalsMethods l1) l1 i else nth (formalsMethods l2) l2 (i - n1)
  , accept = \v f -> let (FormalsAppend l1 l2) = formalsData f in
                     visitFormalsAppend v l1 l2
  }

-- FormalsEnumeration Methods
data FormalsEnumerationMethods = FormalsEnumerationMethods
  { hasNext :: FormalsEnumeration -> Bool
  , next :: FormalsEnumeration -> (FormalsEnumeration, Formal)
  }

defaultFormalsEnumerationMethods :: FormalsEnumerationMethods
defaultFormalsEnumerationMethods = FormalsEnumerationMethods
  { hasNext = \e -> i e + 1 < n e
  , next = \e -> let i' = i e + 1
                     caseResult = getArray (a e) i'
                 in (e { i = i' }, caseResult)
  }

-- Expression Methods
data ExpressionMethods = ExpressionMethods
  { getOfType :: Expression -> Maybe Symbol
  , setOfType :: Symbol -> Expression -> Expression
  , getOfClass :: Expression -> Maybe Class
  , setOfClass :: Class -> Expression -> Expression
  , getBinding :: Expression -> Maybe CoolNode
  , setBinding :: CoolNode -> Expression -> Expression
  , getMBinding :: Expression -> Maybe Cmethod
  , setMBinding :: Cmethod -> Expression -> Expression
  }

defaultExpressionMethods :: ExpressionMethods
defaultExpressionMethods = ExpressionMethods
  { getOfType = ofType
  , setOfType = \s e -> e { ofType = Just s }
  , getOfClass = ofClass
  , setOfClass = \c e -> e { ofClass = Just c }
  , getBinding = binding
  , setBinding = \b e -> e { binding = Just b }
  , getMBinding = mBinding
  , setMBinding = \m e -> e { mBinding = Just m }
  }

-- Expressions Methods
data ExpressionsMethods = ExpressionsMethods
  { size :: Expressions -> Int
  , nth :: Expressions -> Int -> Expression
  , concatExpressions :: Expressions -> Expressions -> Expressions
  , addCopy :: Expressions -> Expression -> Expressions
  , elements :: Expressions -> ExpressionsEnumeration
  , accept :: forall v a. CoolVisitor v => v -> Expressions -> a
  }

defaultExpressionsMethods :: ExpressionsMethods
defaultExpressionsMethods = ExpressionsMethods
  { size = const 0
  , nth = \_ _ -> error "abort: no more elements"
  , concatExpressions = \e1 e2 -> asExpressions (ExpressionsAppend e1 e2)
  , addCopy = \e ex -> concatExpressions e (asExpressions (ExpressionsOne ex))
  , elements = ExpressionsEnumeration
  , accept = \_ _ -> error "accept method not implemented"
  }

-- Expressions_nil Methods
expressionsNilMethods :: ExpressionsMethods
expressionsNilMethods = defaultExpressionsMethods
  { concatExpressions = \_ e -> e
  }

-- Expressions_one Methods
expressionsOneMethods :: ExpressionsMethods
expressionsOneMethods = defaultExpressionsMethods
  { size = const 1
  , nth = \e i -> if i == 0 then getExpression (expressionsData e) else nth defaultExpressionsMethods e i
  , accept = \v e -> visitExpressionsOne v (getExpression $ expressionsData e)
  }

-- Expressions_append Methods
expressionsAppendMethods :: ExpressionsMethods
expressionsAppendMethods = defaultExpressionsMethods
  { size = \e -> let (ExpressionsAppend l1 l2) = expressionsData e in size (expressionsMethods l1) l1 + size (expressionsMethods l2) l2
  , nth = \e i -> let (ExpressionsAppend l1 l2) = expressionsData e in
                   let n1 = size (expressionsMethods l1) l1 in
                   if i < n1 then nth (expressionsMethods l1) l1 i else nth (expressionsMethods l2) l2 (i - n1)
  , accept = \v e -> let (ExpressionsAppend l1 l2) = expressionsData e in
                     visitExpressionsAppend v l1 l2
  }

-- ExpressionsEnumeration Methods
data ExpressionsEnumerationMethods = ExpressionsEnumerationMethods
  { hasNext :: ExpressionsEnumeration -> Bool
  , next :: ExpressionsEnumeration -> (ExpressionsEnumeration, Expression)
  }

defaultExpressionsEnumerationMethods :: ExpressionsEnumerationMethods
defaultExpressionsEnumerationMethods = ExpressionsEnumerationMethods
  { hasNext = \e -> i e + 1 < n e
  , next = \e -> let i' = i e + 1
                     caseResult = getArray (a e) i'
                 in (e { i = i' }, caseResult)
  }

-- Case Methods
data CaseMethods = CaseMethods
  { getCaseOfType :: Case -> Maybe Symbol
  , setCaseOfType :: Symbol -> Case -> Case
  , getCaseOfClass :: Case -> Maybe Class
  , setCaseOfClass :: Class -> Case -> Case
  }

defaultCaseMethods :: CaseMethods
defaultCaseMethods = CaseMethods
  { getCaseOfType = caseOfType
  , setCaseOfType = \s c -> c { caseOfType = Just s }
  , getCaseOfClass = caseOfClass
  , setCaseOfClass = \cl c -> c { caseOfClass = Just cl }
  }

-- Cases Methods
data CasesMethods = CasesMethods
  { size :: Cases -> Int
  , nth :: Cases -> Int -> Case
  , concatCases :: Cases -> Cases -> Cases
  , addCopy :: Cases -> Case -> Cases
  , elements :: Cases -> CasesEnumeration
  , accept :: forall v a. CoolVisitor v => v -> Cases -> a
  }

defaultCasesMethods :: CasesMethods
defaultCasesMethods = CasesMethods
  { size = const 0
  , nth = \_ _ -> error "abort: no more elements"
  , concatCases = \c1 c2 -> asCases (CasesAppend c1 c2)
  , addCopy = \c e -> concatCases c (asCases (CasesOne e))
  , elements = CasesEnumeration
  , accept = \_ _ -> error "accept method not implemented"
  }

-- Cases_nil Methods
casesNilMethods :: CasesMethods
casesNilMethods = defaultCasesMethods
  { concatCases = \_ c -> c
  }

-- Cases_one Methods
casesOneMethods :: CasesMethods
casesOneMethods = defaultCasesMethods
  { size = const 1
  , nth = \c i -> if i == 0 then getCase (casesData c) else nth defaultCasesMethods c i
  , accept = \v c -> visitCasesOne v (getCase $ casesData c)
  }

-- Cases_append Methods
casesAppendMethods :: CasesMethods
casesAppendMethods = defaultCasesMethods
  { size = \c -> let (CasesAppend l1 l2) = casesData c in size (casesMethods l1) l1 + size (casesMethods l2) l2
  , nth = \c i -> let (CasesAppend l1 l2) = casesData c in
                   let n1 = size (casesMethods l1) l1 in
                   if i < n1 then nth (casesMethods l1) l1 i else nth (casesMethods l2) l2 (i - n1)
  , accept = \v c -> let (CasesAppend l1 l2) = casesData c in
                     visitCasesAppend v l1 l2
  }

-- CasesEnumeration Methods
data CasesEnumerationMethods = CasesEnumerationMethods
  { hasNext :: CasesEnumeration -> Bool
  , next :: CasesEnumeration -> Case
  }

defaultCasesEnumerationMethods :: CasesEnumerationMethods
defaultCasesEnumerationMethods = CasesEnumerationMethods
  { hasNext = \e -> let (CasesEnumeration _ i n _ _) = e in i + 1 < n
  , next = \e -> let (CasesEnumeration seq i n arr _) = e in
                   let i' = i + 1
                       in let caseResult = getArray arr i' in
                          case caseResult of
                            c@(Case{}) -> e { i = i' } `seq` c
  }

-- Cprogram Methods
data CprogramMethods = CprogramMethods
  { getClasses :: Cprogram -> Classes
  , setClasses :: Classes -> Cprogram -> Cprogram
  , acceptCprogram :: forall v a. CoolVisitor v => v -> Cprogram -> a
  }

defaultCprogramMethods :: CprogramMethods
defaultCprogramMethods = CprogramMethods
  { getClasses = classes
  , setClasses = \newClasses prog -> prog { classes = newClasses }
  , acceptCprogram = \v prog -> visitProgram v prog (classes prog)
  }

-- Cclass_decl Methods
data CclassDeclMethods = CclassDeclMethods
  { getName :: CclassDecl -> Symbol
  , setName :: Symbol -> CclassDecl -> CclassDecl
  , getParent :: CclassDecl -> Symbol
  , setParent :: Symbol -> CclassDecl -> CclassDecl
  , getFeatures :: CclassDecl -> Features
  , setFeatures :: Features -> CclassDecl -> CclassDecl
  , getFilename :: CclassDecl -> Symbol
  , setFilename :: Symbol -> CclassDecl -> CclassDecl
  , acceptCclassDecl :: forall v a. CoolVisitor v => v -> CclassDecl -> a
  }

defaultCclassDeclMethods :: CclassDeclMethods
defaultCclassDeclMethods = CclassDeclMethods
  { getName = name
  , setName = \newName c -> c { name = newName }
  , getParent = parent
  , setParent = \newParent c -> c { parent = newParent }
  , getFeatures = features
  , setFeatures = \newFeatures c -> c { features = newFeatures }
  , getFilename = filename
  , setFilename = \newFilename c -> c { filename = newFilename }
  , acceptCclassDecl = \v c -> visitClassDecl v c (name c) (parent c) (features c) (filename c)
  }

-- Cmethod Methods
data CmethodMethods = CmethodMethods
  { getOverridep :: Cmethod -> Bool
  , setOverridep :: Bool -> Cmethod -> Cmethod
  , getName :: Cmethod -> Symbol
  , setName :: Symbol -> Cmethod -> Cmethod
  , getFormals :: Cmethod -> Formals
  , setFormals :: Formals -> Cmethod -> Cmethod
  , getReturnType :: Cmethod -> Symbol
  , setReturnType :: Symbol -> Cmethod -> Cmethod
  , getExpr :: Cmethod -> Expression
  , setExpr :: Expression -> Cmethod -> Cmethod
  , acceptCmethod :: forall v a. CoolVisitor v => v -> Cmethod -> a
  }

defaultCmethodMethods :: CmethodMethods
defaultCmethodMethods = CmethodMethods
  { getOverridep = overridep
  , setOverridep = \newOverridep m -> m { overridep = newOverridep }
  , getName = name
  , setName = \newName m -> m { name = newName }
  , getFormals = formals
  , setFormals = \newFormals m -> m { formals = newFormals }
  , getReturnType = returnType
  , setReturnType = \newReturnType m -> m { returnType = newReturnType }
  , getExpr = expr
  , setExpr = \newExpr m -> m { expr = newExpr }
  , acceptCmethod = \v m -> visitMethod v m (overridep m) (name m) (formals m) (returnType m) (expr m)
  }

-- Cattr Methods
data CattrMethods = CattrMethods
  { getName :: Cattr -> Symbol
  , setName :: Symbol -> Cattr -> Cattr
  , getOfType :: Cattr -> Symbol
  , setOfType :: Symbol -> Cattr -> Cattr
  , acceptCattr :: forall v a. CoolVisitor v => v -> Cattr -> a
  }

defaultCattrMethods :: CattrMethods
defaultCattrMethods = CattrMethods
  { getName = name
  , setName = \newName c -> c { name = newName }
  , getOfType = ofType
  , setOfType = \newOfType c -> c { ofType = newOfType }
  , acceptCattr = \v c -> visitAttr v c (name c) (ofType c)
  }

-- Cformal Methods
data CformalMethods = CformalMethods
  { getName :: Cformal -> Symbol
  , setName :: Symbol -> Cformal -> Cformal
  , getOfType :: Cformal -> Symbol
  , setOfType :: Symbol -> Cformal -> Cformal
  , acceptCformal :: forall v a. CoolVisitor v => v -> Cformal -> a
  }

defaultCformalMethods :: CformalMethods
defaultCformalMethods = CformalMethods
  { getName = name
  , setName = \newName f -> f { name = newName }
  , getOfType = ofType
  , setOfType = \newOfType f -> f { ofType = newOfType }
  , acceptCformal = \v f -> visitFormal v f (name f) (ofType f)
  }

-- Cbranch Methods
data CbranchMethods = CbranchMethods
  { getName :: Cbranch -> Symbol
  , setName :: Symbol -> Cbranch -> Cbranch
  , getLocalType :: Cbranch -> Symbol
  , setLocalType :: Symbol -> Cbranch -> Cbranch
  , getExpr :: Cbranch -> Expression
  , setExpr :: Expression -> Cbranch -> Cbranch
  , acceptCbranch :: forall v a. CoolVisitor v => v -> Cbranch -> a
  }

defaultCbranchMethods :: CbranchMethods
defaultCbranchMethods = CbranchMethods
  { getName = name
  , setName = \newName b -> b { name = newName }
  , getLocalType = localType
  , setLocalType = \newLocalType b -> b { localType = newLocalType }
  , getExpr = expr
  , setExpr = \newExpr b -> b { expr = newExpr }
  , acceptCbranch = \v b -> visitBranch v b (name b) (localType b) (expr b)
  }

-- Cassign Methods
data CassignMethods = CassignMethods
  { getName :: Cassign -> Symbol
  , setName :: Symbol -> Cassign -> Cassign
  , getExpr :: Cassign -> Expression
  , setExpr :: Expression -> Cassign -> Cassign
  , acceptCassign :: forall v a. CoolVisitor v => v -> Cassign -> a
  }

defaultCassignMethods :: CassignMethods
defaultCassignMethods = CassignMethods
  { getName = name
  , setName = \newName c -> c { name = newName }
  , getExpr = expr
  , setExpr = \newExpr c -> c { expr = newExpr }
  , acceptCassign = \v c -> visitAssign v c (name c) (expr c)
  }

-- Cstatic_dispatch Methods
data CstaticDispatchMethods = CstaticDispatchMethods
  { getExpr :: CstaticDispatch -> Expression
  , setExpr :: Expression -> CstaticDispatch -> CstaticDispatch
  , getTypeName :: CstaticDispatch -> Symbol
  , setTypeName :: Symbol -> CstaticDispatch -> CstaticDispatch
  , getName :: CstaticDispatch -> Symbol
  , setName :: Symbol -> CstaticDispatch -> CstaticDispatch
  , getActuals :: CstaticDispatch -> Expressions
  , setActuals :: Expressions -> CstaticDispatch -> CstaticDispatch
  , acceptCstaticDispatch :: forall v a. CoolVisitor v => v -> CstaticDispatch -> a
  }

defaultCstaticDispatchMethods :: CstaticDispatchMethods
defaultCstaticDispatchMethods = CstaticDispatchMethods
  { getExpr = expr
  , setExpr = \newExpr d -> d { expr = newExpr }
  , getTypeName = typeName
  , setTypeName = \newTypeName d -> d { typeName = newTypeName }
  , getName = name
  , setName = \newName d -> d { name = newName }
  , getActuals = actuals
  , setActuals = \newActuals d -> d { actuals = newActuals }
  , acceptCstaticDispatch = \v d -> visitStaticDispatch v d (expr d) (typeName d) (name d) (actuals d)
  }

-- Cdispatch Methods
data CdispatchMethods = CdispatchMethods
  { getExpr :: Cdispatch -> Expression
  , setExpr :: Expression -> Cdispatch -> Cdispatch
  , getName :: Cdispatch -> Symbol
  , setName :: Symbol -> Cdispatch -> Cdispatch
  , getActuals :: Cdispatch -> Expressions
  , setActuals :: Expressions -> Cdispatch -> Cdispatch
  , acceptCdispatch :: forall v a. CoolVisitor v => v -> Cdispatch -> a
  }

defaultCdispatchMethods :: CdispatchMethods
defaultCdispatchMethods = CdispatchMethods
  { getExpr = expr
  , setExpr = \newExpr d -> d { expr = newExpr }
  , getName = name
  , setName = \newName d -> d { name = newName }
  , getActuals = actuals
  , setActuals = \newActuals d -> d { actuals = newActuals }
  , acceptCdispatch = \v d -> visitDispatch v d (expr d) (name d) (actuals d)
  }

-- Ccond Methods
data CcondMethods = CcondMethods
  { getPred :: Ccond -> Expression
  , setPred :: Expression -> Ccond -> Ccond
  , getThenExp :: Ccond -> Expression
  , setThenExp :: Expression -> Ccond -> Ccond
  , getElseExp :: Ccond -> Expression
  , setElseExp :: Expression -> Ccond -> Ccond
  , acceptCcond :: forall v a. CoolVisitor v => v -> Ccond -> a
  }

defaultCcondMethods :: CcondMethods
defaultCcondMethods = CcondMethods
  { getPred = predExp
  , setPred = \newPred c -> c { predExp = newPred }
  , getThenExp = thenExp
  , setThenExp = \newThenExp c -> c { thenExp = newThenExp }
  , getElseExp = elseExp
  , setElseExp = \newElseExp c -> c { elseExp = newElseExp }
  , acceptCcond = \v c -> visitCond v c (predExp c) (thenExp c) (elseExp c)
  }

-- Cloop Methods
data CloopMethods = CloopMethods
  { getPred :: Cloop -> Expression
  , setPred :: Expression -> Cloop -> Cloop
  , getBody :: Cloop -> Expression
  , setBody :: Expression -> Cloop -> Cloop
  , acceptCloop :: forall v a. CoolVisitor v => v -> Cloop -> a
  }

defaultCloopMethods :: CloopMethods
defaultCloopMethods = CloopMethods
  { getPred = predExp
  , setPred = \newPred c -> c { predExp = newPred }
  , getBody = bodyExp
  , setBody = \newBody c -> c { bodyExp = newBody }
  , acceptCloop = \v c -> visitLoop v c (predExp c) (bodyExp c)
  }

-- Ctypecase Methods
data CtypecaseMethods = CtypecaseMethods
  { getExpr :: Ctypecase -> Expression
  , setExpr :: Expression -> Ctypecase -> Ctypecase
  , getCases :: Ctypecase -> Cases
  , setCases :: Cases -> Ctypecase -> Ctypecase
  , acceptCtypecase :: forall v a. CoolVisitor v => v -> Ctypecase -> a
  }

defaultCtypecaseMethods :: CtypecaseMethods
defaultCtypecaseMethods = CtypecaseMethods
  { getExpr = expr
  , setExpr = \newExpr c -> c { expr = newExpr }
  , getCases = cases
  , setCases = \newCases c -> c { cases = newCases }
  , acceptCtypecase = \v c -> visitTypecase v c (expr c) (cases c)
  }

-- Cblock Methods
data CblockMethods = CblockMethods
  { getBody :: Cblock -> Expressions
  , setBody :: Expressions -> Cblock -> Cblock
  , acceptCblock :: forall v a. CoolVisitor v => v -> Cblock -> a
  }

defaultCblockMethods :: CblockMethods
defaultCblockMethods = CblockMethods
  { getBody = body
  , setBody = \newBody c -> c { body = newBody }
  , acceptCblock = \v c -> visitBlock v c (body c)
  }

-- Clet Methods
data CletMethods = CletMethods
  { getIdentifier :: Clet -> Symbol
  , setIdentifier :: Symbol -> Clet -> Clet
  , getLocalType :: Clet -> Symbol
  , setLocalType :: Symbol -> Clet -> Clet
  , getInit :: Clet -> Expression
  , setInit :: Expression -> Clet -> Clet
  , getBody :: Clet -> Expression
  , setBody :: Expression -> Clet -> Clet
  , acceptClet :: forall v a. CoolVisitor v => v -> Clet -> a
  }

defaultCletMethods :: CletMethods
defaultCletMethods = CletMethods
  { getIdentifier = identifier
  , setIdentifier = \newIdentifier c -> c { identifier = newIdentifier }
  , getLocalType = localType
  , setLocalType = \newLocalType c -> c { localType = newLocalType }
  , getInit = initExpr
  , setInit = \newInit c -> c { initExpr = newInit }
  , getBody = bodyExpr
  , setBody = \newBody c -> c { bodyExpr = newBody }
  , acceptClet = \v c -> visitLet v c (identifier c) (localType c) (initExpr c) (bodyExpr c)
  }

-- Cadd Methods
data CaddMethods = CaddMethods
  { getE1 :: Cadd -> Expression
  , setE1 :: Expression -> Cadd -> Cadd
  , getE2 :: Cadd -> Expression
  , setE2 :: Expression -> Cadd -> Cadd
  , acceptCadd :: forall v a. CoolVisitor v => v -> Cadd -> a
  }

defaultCaddMethods :: CaddMethods
defaultCaddMethods = CaddMethods
  { getE1 = e1
  , setE1 = \newE1 c -> c { e1 = newE1 }
  , getE2 = e2
  , setE2 = \newE2 c -> c { e2 = newE2 }
  , acceptCadd = \v c -> visitAdd v c (e1 c) (e2 c)
  }

-- Csub Methods
data CsubMethods = CsubMethods
  { getE1 :: Csub -> Expression
  , setE1 :: Expression -> Csub -> Csub
  , getE2 :: Csub -> Expression
  , setE2 :: Expression -> Csub -> Csub
  , acceptCsub :: forall v a. CoolVisitor v => v -> Csub -> a
  }

defaultCsubMethods :: CsubMethods
defaultCsubMethods = CsubMethods
  { getE1 = e1
  , setE1 = \newE1 c -> c { e1 = newE1 }
  , getE2 = e2
  , setE2 = \newE2 c -> c { e2 = newE2 }
  , acceptCsub = \v c -> visitSub v c (e1 c) (e2 c)
  }

-- Cmul Methods
data CmulMethods = CmulMethods
  { getE1 :: Cmul -> Expression
  , setE1 :: Expression -> Cmul -> Cmul
  , getE2 :: Cmul -> Expression
  , setE2 :: Expression -> Cmul -> Cmul
  , acceptCmul :: forall v a. CoolVisitor v => v -> Cmul -> a
  }

defaultCmulMethods :: CmulMethods
defaultCmulMethods = CmulMethods
  { getE1 = e1
  , setE1 = \newE1 c -> c { e1 = newE1 }
  , getE2 = e2
  , setE2 = \newE2 c -> c { e2 = newE2 }
  , acceptCmul = \v c -> visitMul v c (e1 c) (e2 c)
  }

-- Cdiv Methods
data CdivMethods = CdivMethods
  { getE1 :: Cdiv -> Expression
  , setE1 :: Expression -> Cdiv -> Cdiv
  , getE2 :: Cdiv -> Expression
  , setE2 :: Expression -> Cdiv -> Cdiv
  , acceptCdiv :: forall v a. CoolVisitor v => v -> Cdiv -> a
  }

defaultCdivMethods :: CdivMethods
defaultCdivMethods = CdivMethods
  { getE1 = e1
  , setE1 = \newE1 c -> c { e1 = newE1 }
  , getE2 = e2
  , setE2 = \newE2 c -> c { e2 = newE2 }
  , acceptCdiv = \v c -> visitDiv v c (e1 c) (e2 c)
  }

-- Cneg Methods
data CnegMethods = CnegMethods
  { get_e1 :: Expression -> Expression
  , set_e1 :: Expression -> Expression -> Expression
  , accept :: forall v a. CoolVisitor v => v -> Expression -> a
  } deriving (Show, Read, Eq)

defaultcnegMethods :: CnegMethods
defaultcnegMethods = CnegMethods
  { get_e1 = e1
  , set_e1 = \new_e1 expr -> expr { e1 = new_e1 }
  , accept = \visitor expr@(Cneg e1) -> visitNeg visitor expr e1
  }

-- Cint_lit Methods
data CintLitMethods = CintLitMethods
  { getToken :: CintLit -> Symbol
  , setToken :: Symbol -> CintLit -> CintLit
  , acceptCintLit :: forall v a. CoolVisitor v => v -> CintLit -> a
  }

defaultCintLitMethods :: CintLitMethods
defaultCintLitMethods = CintLitMethods
  { getToken = token
  , setToken = \newToken c -> c { token = newToken }
  , acceptCintLit = \v c -> visitIntLit v c (token c)
  }

-- Cbool_lit Methods
data CboolLitMethods = CboolLitMethods
  { getValue :: CboolLit -> Bool
  , setValue :: Bool -> CboolLit -> CboolLit
  , acceptCboolLit :: forall v a. CoolVisitor v => v -> CboolLit -> a
  }

defaultCboolLitMethods :: CboolLitMethods
defaultCboolLitMethods = CboolLitMethods
  { getValue = value
  , setValue = \newValue c -> c { value = newValue }
  , acceptCboolLit = \v c -> visitBoolLit v c (value c)
  }

-- Cstring_lit Methods
data CstringLitMethods = CstringLitMethods
  { getToken :: CstringLit -> Symbol
  , setToken :: Symbol -> CstringLit -> CstringLit
  , acceptCstringLit :: forall v a. CoolVisitor v => v -> CstringLit -> a
  }

defaultCstringLitMethods :: CstringLitMethods
defaultCstringLitMethods = CstringLitMethods
  { getToken = token
  , setToken = \newToken c -> c { token = newToken }
  , acceptCstringLit = \v c -> visitStringLit v c (token c)
  }

-- Calloc Methods
data CallocMethods = CallocMethods
  { getTypeName :: Calloc -> Symbol
  , setTypeName :: Symbol -> Calloc -> Calloc
  , acceptCalloc :: forall v a. CoolVisitor v => v -> Calloc -> a
  }

defaultCallocMethods :: CallocMethods
defaultCallocMethods = CallocMethods
  { getTypeName = typeName
  , setTypeName = \newTypeName c -> c { typeName = newTypeName }
  , acceptCalloc = \v c -> visitAlloc v c (typeName c)
  }

-- Cnil Methods
data CnilMethods = CnilMethods
  { acceptCnil :: forall v a. CoolVisitor v => v -> Cnil -> a
  }

defaultCnilMethods :: CnilMethods
defaultCnilMethods = CnilMethods
  { acceptCnil = visitNil
  }

-- Cunit Methods
data CunitMethods = CunitMethods
  { acceptCunit :: forall v a. CoolVisitor v => v -> Cunit -> a
  }

defaultCunitMethods :: CunitMethods
defaultCunitMethods = CunitMethods
  { acceptCunit = visitUnit
  }

-- Cno_expr Methods
data CnoExprMethods = CnoExprMethods
  { acceptCnoExpr :: forall v a. CoolVisitor v => v -> CnoExpr -> a
  }

defaultCnoExprMethods :: CnoExprMethods
defaultCnoExprMethods = CnoExprMethods
  { acceptCnoExpr = visitNoExpr
  }

-- Cvariable Methods
data CvariableMethods = CvariableMethods
  { getName :: Cvariable -> Symbol
  , setName :: Symbol -> Cvariable -> Cvariable
  , acceptCvariable :: forall v a. CoolVisitor v => v -> Cvariable -> a
  }

defaultCvariableMethods :: CvariableMethods
defaultCvariableMethods = CvariableMethods
  { getName = name
  , setName = \newName c -> c { name = newName }
  , acceptCvariable = \v c -> visitVariable v c (name c)
  }
