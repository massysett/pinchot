{-# LANGUAGE TemplateHaskell #-}
module Pinchot.SyntaxTree.Optics where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Control.Lens as Lens
import qualified Language.Haskell.TH as T
import qualified Language.Haskell.TH.Syntax as Syntax

import Pinchot.Names
import Pinchot.Rules
import Pinchot.Types

-- | Creates optics declarations for a 'Rule', if optics can
-- be made for the 'Rule':
--
-- * 'Pinchot.terminal' gets a single 'Lens.Prism'
--
-- * 'Pinchot.nonTerminal' gets a 'Lens.Prism' for each constructor
--
-- * 'Pinchot.record' gets a single 'Lens.Lens'
--
-- * 'Pinchot.wrap', 'Pinchot.opt', 'Pinchot.star', and 'Pinchot.plus'
-- do not get optics.  For those, you will typically want to use
-- 'Pinchot.wrappedInstances'.
--
-- Each rule in the sequence of 'Rule', as well as all ancestors of
-- those 'Rule's, will be handled.
--
-- Example: "Pinchot.Examples.RulesToOptics".
rulesToOptics
  :: (Syntax.Lift t, Data t)
  => Qualifier
  -- ^ Qualifier for module containing the data types that will get
  -- optics
  -> T.Name
  -- ^ Type name for the terminal
  -> [Rule t]
  -> T.Q [T.Dec]
rulesToOptics qual termName
  = fmap concat
  . traverse (ruleToOptics qual termName)
  . families

-- | Creates optics declarations for a single 'Rule', if optics can
-- be made for the 'Rule':
--
-- * 'Terminal' gets a single 'Lens.Prism'
--
-- * 'NonTerminal' gets a 'Lens.Prism' for each constructor
--
-- * 'Series' gets a single 'Lens.Prism'
--
-- * 'Record' gets a single 'Lens.Lens'
--
-- * 'Wrap', 'Opt', 'Star', and 'Plus' do not get optics.
--
-- TODO add prism for 'Series'
ruleToOptics
  :: (Syntax.Lift t, Data t)
  => Qualifier
  -- ^ Qualifier for module containing the data type that will get
  -- optics
  -> T.Name
  -- ^ Type name for the terminal
  -> Rule t
  -> T.Q [T.Dec]
ruleToOptics qual termName (Rule nm _ ty) = case ty of
  Terminal pdct -> terminalToOptics qual termName nm pdct
  NonTerminal bs -> sequence $ nonTerminalToOptics qual nm bs
  Record sq -> sequence $ recordsToOptics qual nm sq
  Series ne -> seriesToOptics qual termName nm ne
  _ -> return []
  

-- | Creates a prism for a terminal type.  Although a newtype wraps
-- each terminal, do not make a Wrapped or an Iso, because the
-- relationship between the outer type and the type that it wraps
-- typically is not isometric.  Thus, use a Prism instead, which
-- captures this relationship properly.
terminalToOptics
  :: Syntax.Lift t
  => Qualifier
  -- ^ Qualifier for module containing the data type that will get
  -- optics
  -> T.Name
  -- ^ Terminal type name
  -> String
  -- ^ Rule name
  -> Predicate t
  -> T.Q [T.Dec]
terminalToOptics qual termName nm (Predicate pdct) = do
  ctorName <- lookupTypeName (quald qual nm)
  e1 <- T.sigD (T.mkName ('_':nm))
    $ T.forallT [ tyVarBndrA] (return [])
    [t| Lens.Prism' ( $(T.conT termName), $(typeA) )
                    ( $(T.conT ctorName) $(T.conT termName) $(typeA))
    |]
  
  e2 <- T.valD prismName (T.normalB expn) []
  return [e1, e2]
  where
    prismName = T.varP (T.mkName ('_' : nm))
    expn = do
      x <- T.newName "_x"
      ctorName <- lookupValueName (quald qual nm)
      let fetchPat = T.conP ctorName [T.varP x]
          fetchName = T.varE x
      [| let fetch $fetchPat = $fetchName
             store (term, a)
                 | $(fmap T.unType pdct) term
                    = Just ($(T.conE ctorName) (term, a))
                 | otherwise = Nothing
               in Lens.prism' fetch store
        |]


seriesToOptics
  :: (Data t, Syntax.Lift t)
  => Qualifier
  -- ^ Qualifier for module containing the data type that will get
  -- optics
  -> T.Name
  -- ^ Terminal type name
  -> String
  -- ^ Rule name
  -> NonEmpty t
  -> T.Q [T.Dec]
seriesToOptics qual termName nm terminals = do
  ctorName <- lookupTypeName (quald qual nm)
  e1 <- T.sigD (T.mkName ('_':nm))
    $ T.forallT [ tyVarBndrA] (return [])
    [t| Lens.Prism' (NonEmpty ( $(T.conT termName), $(typeA) ))
                    ( $(T.conT ctorName) $(T.conT termName) $(typeA))
    |]
  
  e2 <- T.valD prismName (T.normalB expn) []
  return [e1, e2]
  where
    prismName = T.varP (T.mkName ('_' : nm))
    expn = do
      x <- T.newName "_x"
      ctorName <- lookupValueName (quald qual nm)
      let fetchPat = T.conP ctorName [T.varP x]
          fetchName = T.varE x
      [| let fetch $fetchPat = $fetchName
             store terms
                 | fmap fst terms == $(Syntax.liftData terminals)
                    = Just ($(T.conE ctorName) terms )
                 | otherwise = Nothing
               in Lens.prism' fetch store
        |]


prismSignature
  :: Qualifier
  -> String
  -- ^ Rule name
  -> Branch t
  -> T.DecQ
prismSignature qual nm (Branch inner rules) = do
  ctorName <- lookupTypeName (quald qual nm)
  T.sigD prismName
    (forallA [t| Lens.Prism'
        ($(T.conT ctorName) $(typeT) $(typeA))
        $(fieldsType) |])
  where
    prismName = T.mkName ('_' : inner)
    fieldsType = case rules of
      [] -> T.tupleT 0
      Rule r1 _ _ : [] -> do
        ctorName <- lookupTypeName (quald qual r1)
        [t| $(T.conT ctorName) $(typeT) $(typeA) |]
      rs -> foldl addType (T.tupleT (length rs)) rs
        where
          addType soFar (Rule r _ _) = do
            ctorName <- lookupTypeName (quald qual r)
            soFar `T.appT`
              [t| $(T.conT ctorName) $(typeT) $(typeA) |]

setterPatAndExpn
  :: Qualifier
  -> BranchName
  -> [a]
  -- ^ List of rules
  -> T.Q (T.PatQ, T.ExpQ)
setterPatAndExpn qual inner rules = do
  names <- sequence . flip replicate (T.newName "_setterPatAndExpn")
    . length $ rules
  let pat = T.tupP . fmap T.varP $ names
      expn = foldl addVar start names
        where
          start = do
            ctorName <- lookupValueName (quald qual inner)
            T.conE ctorName
          addVar acc nm = acc `T.appE` (T.varE nm)
  return (pat, expn)

prismSetter
  :: Qualifier
  -> Branch t
  -> T.ExpQ
prismSetter qual (Branch inner rules) = do
  (pat, expn) <- setterPatAndExpn qual inner rules
  T.lamE [pat] expn

-- | Returns a pattern and expression to match a particular branch; if
-- there is a match, the expression will return each field, in a tuple
-- in a Right.
rightPatternAndExpression
  :: Qualifier
  -> BranchName
  -> Int
  -- ^ Number of fields
  -> T.Q (T.PatQ, T.ExpQ)
rightPatternAndExpression qual inner n = do
  names <- sequence . replicate n $ T.newName "_patternAndExpression"
  ctorName <- lookupValueName (quald qual inner)
  let pat = T.conP ctorName . fmap T.varP $ names
      expn = T.appE (T.conE 'Right)
        . T.tupE
        . fmap T.varE
        $ names
  return (pat, expn)

-- | Returns a pattern and expression for branches that did not match.
-- Does not return anything if there are no other branches.
leftPatternAndExpression
  :: [a]
  -- ^ List of all other branches
  -> Maybe (T.Q (T.PatQ, T.ExpQ))
leftPatternAndExpression ls
  | null ls = Nothing
  | otherwise = Just $ do
      local <- T.newName "_leftPatternAndExpression"
      return (T.varP local, T.appE (T.conE 'Left) (T.varE local))
  

prismGetter
  :: Qualifier
  -> Branch t
  -- ^ Make prism for this branch
  -> [Branch t] 
  -- ^ List of all branches
  -> T.ExpQ
prismGetter qual (Branch inner rules) bs = do
  local <- T.newName "_prismGetter"
  (patCtor, bodyCtor) <- rightPatternAndExpression qual inner (length rules)
  let firstElem = T.match patCtor (T.normalB bodyCtor) []
  lastElem <- case leftPatternAndExpression bs of
    Nothing -> return []
    Just computation -> do
      (patLeft, expLeft) <- computation
      return [T.match patLeft (T.normalB expLeft) []]
  T.lamE [T.varP local]
    (T.caseE (T.varE local) $ firstElem : lastElem)


-- | Creates prisms for each 'Branch'.
nonTerminalToOptics
  :: Qualifier
  -- ^ Qualifier for module containing the data type that will get
  -- optics
  -> String
  -- ^ Rule name
  -> NonEmpty (Branch t)
  -> [T.Q T.Dec]
nonTerminalToOptics qual nm bsSeq = concat $ fmap makePrism bs
  where
    bs = toList bsSeq
    makePrism branch@(Branch inner _) =
      [ prismSignature qual nm branch, binding ]
      where
        prismName = T.mkName ('_' : inner)
        binding = T.valD (T.varP prismName) body []
          where
            body = T.normalB
              $ (T.varE 'Lens.prism)
              `T.appE` (prismSetter qual branch)
              `T.appE` (prismGetter qual branch bs)

recordLensSignature
  :: Qualifier
  -> RuleName
  -- ^ Name of the main rule
  -> RuleName
  -- ^ Name of the rule for this lens
  -> Int
  -- ^ Index for this lens
  -> T.DecQ
recordLensSignature qual nm inner idx = do
  ctorOuter <- lookupTypeName (quald qual nm)
  ctorInner <- lookupTypeName (quald qual inner)
  T.sigD lensName (forallA
    [t| Lens.Lens' ($(T.conT ctorOuter) $(typeT) $(typeA))
                  ($(T.conT ctorInner) $(typeT) $(typeA))
    |])
  where
    lensName = T.mkName $ recordFieldName idx nm inner

recordLensGetter
  :: Qualifier
  -> String
  -- ^ Record field name
  -> T.ExpQ
recordLensGetter qual fieldNm = do
  namedRec <- T.newName "_namedRec"
  fieldNm <- lookupValueName $ quald qual ('_' : fieldNm)
  let pat = T.varP namedRec
      expn = (T.varE fieldNm)
        `T.appE` (T.varE namedRec)
  T.lamE [pat] expn

recordLensSetter
  :: Qualifier
  -> String
  -- ^ Record field name
  -> T.ExpQ
recordLensSetter qual fieldNm = do
  namedRec <- T.newName "_namedRec"
  namedNewVal <- T.newName "_namedNewVal"
  fieldName <- lookupValueName (quald qual ('_' : fieldNm))
  let patRec = T.varP namedRec
      patNewVal = T.varP namedNewVal
      expn = T.recUpdE (T.varE namedRec)
        [ return (fieldName , T.VarE namedNewVal) ]
  T.lamE [patRec, patNewVal] expn

recordLensFunction
  :: Qualifier
  -> RuleName
  -- ^ Name of the main rule
  -> RuleName
  -- ^ Name of the rule for this lens
  -> Int
  -- ^ Index for this lens
  -> T.DecQ
recordLensFunction qual nm inner idx =
  let fieldNm = recordFieldName idx nm inner
      lensName = T.mkName $ recordFieldName idx nm inner
      getter = recordLensGetter qual fieldNm
      setter = recordLensSetter qual fieldNm
      body = (T.varE 'Lens.lens) `T.appE` getter `T.appE` setter
  in T.funD lensName [T.clause [] (T.normalB body) []]


recordsToOptics
  :: Qualifier
  -- ^ Qualifier for module containing the data type that will get
  -- optics
  -> String
  -- ^ Rule name
  -> [Rule t]
  -> [T.Q T.Dec]
recordsToOptics qual nm rules = do
  let makeLens index (Rule inner _ _) = [ signature, function ]
        where
          signature = recordLensSignature qual nm inner index
          function = recordLensFunction qual nm inner index
  concat . zipWith makeLens [(0 :: Int) ..] $ rules

forallA :: T.TypeQ -> T.TypeQ
forallA = T.forallT [ tyVarBndrT, tyVarBndrA ] (return [])
