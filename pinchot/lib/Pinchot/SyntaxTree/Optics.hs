{-# LANGUAGE TemplateHaskell #-}
module Pinchot.SyntaxTree.Optics where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence.NonEmpty (NonEmptySeq)
import qualified Control.Lens as Lens
import qualified Language.Haskell.TH as T
import qualified Language.Haskell.TH.Syntax as Syntax

import Pinchot.Rules
import Pinchot.Types
import Pinchot.Intervals

-- | Creates optics declarations for a 'Rule', if optics can
-- be made for the 'Rule':
--
-- * 'Pinchot.terminal' gets a single 'Lens.Prism'
--
-- * 'Pinchot.nonTerminal' gets a 'Lens.Prism' for each constructor
--
-- * 'Pinchot.record' gets a single 'Lens.Lens'
--
-- * 'Pinchot.wrap', 'Pinchot.opt', 'Pinchot.star',
-- and 'Pinchot.plus' do not get optics.
--
-- Each rule in the sequence of 'Rule', as well as all ancestors of
-- those 'Rule's, will be handled.
--
-- Example: "Pinchot.Examples.RulesToOptics".
rulesToOptics
  :: Syntax.Lift t
  => Qualifier
  -- ^ Qualifier for module containing the data types that will get
  -- optics
  -> T.Name
  -- ^ Type name for the terminal
  -> Seq (Rule t)
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
-- * 'Terminals' gets a single 'Lens.Prism'
--
-- * 'Record' gets a single 'Lens.Lens'
--
-- * 'Wrap', 'Opt', 'Star', and 'Plus' do not get optics.
ruleToOptics
  :: Syntax.Lift t
  => Qualifier
  -- ^ Qualifier for module containing the data type that will get
  -- optics
  -> T.Name
  -- ^ Type name for the terminal
  -> Rule t
  -> T.Q [T.Dec]
ruleToOptics qual termName (Rule nm _ ty) = case ty of
  Terminal ivls -> terminalToOptics qual termName nm ivls
  NonTerminal bs -> sequence $ nonTerminalToOptics qual nm bs
  Record sq -> sequence $ recordsToOptics qual nm sq
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
  -> Intervals t
  -> T.Q [T.Dec]
terminalToOptics qual termName nm ivls = do
  e1 <- T.sigD (T.mkName ('_':nm))
    $ T.forallT [ T.PlainTV (T.mkName "a")] (return [])
    [t| Lens.Prism' ( $(T.conT termName), $(anyType) )
                    ($(T.conT (quald qual nm)) $(T.conT termName) $(anyType))
    |]
  
  e2 <- T.valD prismName (T.normalB expn) []
  return [e1, e2]
  where
    anyType = T.varT (T.mkName "a")
    prismName = T.varP (T.mkName ('_' : nm))
    fetchPat = T.conP (quald qual nm) [T.varP (T.mkName "_x")]
    fetchName = T.varE (T.mkName "_x")
    ctor = T.conE (quald qual nm)
    expn = [| let fetch $fetchPat = $fetchName
                  store (term, a)
                    | inIntervals ivls term = Just ($(ctor) (term, a))
                    | otherwise = Nothing
              in Lens.prism' fetch store
           |]

-- | Creates prisms for each 'Branch'.
nonTerminalToOptics
  :: Qualifier
  -- ^ Qualifier for module containing the data type that will get
  -- optics
  -> String
  -- ^ Rule name
  -> NonEmptySeq (Branch t)
  -> [T.Q T.Dec]
nonTerminalToOptics qual nm bsSeq = concat $ fmap makePrism bs
  where
    bs = toList bsSeq
    makePrism (Branch inner rulesSeq) = [ signature, binding ]
      where
        charType = T.varT (T.mkName "t")
        anyType = T.varT (T.mkName "a")
        rules = toList rulesSeq
        prismName = T.mkName ('_' : inner)
        signature = T.sigD prismName
          (forallA [t| Lens.Prism'
              ($(T.conT (quald qual nm)) $(charType) $(anyType))
              $(fieldsType) |])
          where
            fieldsType = case rules of
              [] -> T.tupleT 0
              Rule r1 _ _ : [] -> [t| $(T.conT (quald qual r1))
                $(charType) $(anyType) |]
              rs -> foldl addType (T.tupleT (length rs)) rs
                where
                  addType soFar (Rule r _ _) = soFar `T.appT`
                    [t| $(T.conT (quald qual r)) $(charType) $(anyType) |]
        binding = T.valD (T.varP prismName) body []
          where
            body = T.normalB
              $ (T.varE 'Lens.prism)
              `T.appE` setter
              `T.appE` getter
              where
                setter = T.lamE [pat] expn
                  where
                    (pat, expn) = case rules of
                      [] -> (T.tupP [], T.conE (quald qual inner))
                      _ : [] -> (T.varP local,
                        T.conE (quald qual inner)
                        `T.appE` T.varE local)
                        where
                          local = T.mkName "_x"
                      ls -> (T.tupP pats, set)
                        where
                          pats = fmap (\i -> T.varP (T.mkName ("_x" ++ show i)))
                            . take (length ls) $ [(0 :: Int) ..]
                          set = foldl addVar start . take (length ls)
                            $ [(0 :: Int) ..]
                            where
                              addVar acc i = acc `T.appE`
                                (T.varE (T.mkName ("_x" ++ show i)))
                              start = T.conE (quald qual inner)

                getter = T.lamE [pat] expn
                  where
                    local = T.mkName "_x"
                    pat = T.varP local
                    expn = T.caseE (T.varE (T.mkName "_x")) $
                      T.match patCtor bodyCtor []
                      : rest
                      where
                        patCtor = T.conP (quald qual inner)
                          . fmap (\i -> T.varP (T.mkName $ "_y" ++ show i))
                          . take (length rules)
                          $ [(0 :: Int) ..]
                        bodyCtor = T.normalB . (T.conE 'Right `T.appE`)
                          $ case rules of
                          [] -> T.tupE []
                          _:[] -> T.varE (T.mkName "_y0")
                          _ -> T.tupE
                            . fmap (\i -> T.varE (T.mkName $ "_y" ++ show i))
                            . take (length rules)
                            $ [(0 :: Int) ..]
                        rest = case bs of
                          [] -> []
                          _ -> [T.match patBlank bodyBlank []]
                          where
                            patBlank = T.varP (T.mkName "_z")
                            bodyBlank = T.normalB
                              $ T.conE ('Left)
                              `T.appE` T.varE (T.mkName "_z")

recordsToOptics
  :: Qualifier
  -- ^ Qualifier for module containing the data type that will get
  -- optics
  -> String
  -- ^ Rule name
  -> Seq (Rule t)
  -> [T.Q T.Dec]
recordsToOptics qual nm
  = concat . zipWith makeLens [(0 :: Int) ..] . toList
  where
    makeLens index (Rule inner _ _) = [ signature, function ]
      where
        charType = T.varT (T.mkName "t")
        anyType = T.varT (T.mkName "a")
        fieldNm = recordFieldName index nm inner
        lensName = T.mkName fieldNm
        signature = T.sigD lensName (forallA
          [t| Lens.Lens' ($(T.conT (quald qual nm)) $(charType) $(anyType))
                         ($(T.conT (quald qual inner)) $(charType) $(anyType))
          |])

        function = T.funD lensName [T.clause [] (T.normalB body) []]
          where
            namedRec = T.mkName "_namedRec"
            namedNewVal = T.mkName "_namedNewVal"
            body = (T.varE 'Lens.lens) `T.appE` getter `T.appE` setter
              where
                getter = T.lamE [pat] expn
                  where
                    pat = T.varP namedRec
                    expn = (T.varE (quald qual ('_' : fieldNm)))
                      `T.appE` (T.varE namedRec)

                setter = T.lamE [patRec, patNewVal] expn
                  where
                    patRec = T.varP namedRec
                    patNewVal = T.varP namedNewVal
                    expn = T.recUpdE (T.varE namedRec)
                      [ return ( quald qual ('_' : fieldNm)
                               , T.VarE namedNewVal) ]

forallA :: T.TypeQ -> T.TypeQ
forallA = T.forallT [ T.PlainTV (T.mkName "t")
                    , T.PlainTV (T.mkName "a")] (return [])
