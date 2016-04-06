{-# LANGUAGE TemplateHaskell #-}
module Pinchot.SyntaxTree.Optics where

import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import qualified Control.Lens as Lens
import qualified Language.Haskell.TH as T
import qualified Language.Haskell.TH.Syntax as Syntax

import Pinchot.Rules
import Pinchot.Types
import Pinchot.Intervals

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
  NonTerminal b1 bs -> sequence $ nonTerminalToOptics qual nm b1 bs
  Terminals sq -> terminalsToOptics qual termName nm sq
  Record sq -> return $ recordsToOptics qual nm sq
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
    [t| Lens.Prism' ( $(T.conT termName), $(anyType) )
                    ($(T.conT (quald qual nm)) $(anyType))
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
  -> Branch t
  -> Seq (Branch t)
  -> [T.Q T.Dec]
nonTerminalToOptics qual nm b1 bsSeq
  = concat $ makePrism b1 : fmap makePrism bs
  where
    bs = toList bsSeq
    makePrism (Branch inner rulesSeq) = [ signature, binding ]
      where
        anyType = T.varT (T.mkName "a")
        rules = toList rulesSeq
        prismName = T.mkName ('_' : inner)
        signature = T.sigD prismName
          [t| Lens.Prism' ($(T.conT (quald qual nm)) $(anyType))
                          $(fieldsType) |]
          where
            fieldsType = case rules of
              [] -> T.tupleT 0
              Rule r1 _ _ : [] -> [t| $(T.conT (quald qual r1)) $(anyType) |]
              rs -> foldl addType (T.tupleT (length rs)) rs
                where
                  addType soFar (Rule r _ _) = soFar `T.appT`
                    [t| $(T.conT (quald qual r)) $(anyType) |]
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

-- | Creates a prism for a 'Terminals'.
terminalsToOptics
  :: Syntax.Lift t
  => Qualifier
  -- ^ Qualifier for module containing the data type that will get
  -- optics
  -> T.Name
  -- ^ Terminal type name
  -> String
  -- ^ Rule name
  -> Seq t
  -> T.Q [T.Dec]
terminalsToOptics qual termName nm sq = do
  e1 <- T.sigD (T.mkName ('_':nm)) 
    [t| Lens.Prism' (Seq ( $(T.conT termName), $(anyType) ))
                    ( $(T.conT (quald qual nm)), $(anyType)) |]
  e2 <- T.valD prismName (T.normalB expn) []
  return [e1, e2]
  where
    anyType = T.varT (T.mkName "a")
    prismName = T.varP (T.mkName ('_' : nm))
    fetchPat = T.conP (quald qual nm) [T.varP (T.mkName "_x")]
    fetchName = T.varE (T.mkName "_x")
    ctor = T.conE (T.mkName nm)
    expn = [| let fetch = coerce
                  store _term
                    | $(liftSeq sq) == fmap fst _term = Just ($ctor _term)
                    | otherwise = Nothing
              in Lens.prism' fetch store
           |]

recordsToOptics
  :: Qualifier
  -- ^ Qualifier for module containing the data type that will get
  -- optics
  -> String
  -- ^ Rule name
  -> Seq (Rule t)
  -> [T.Dec]
recordsToOptics qual nm
  = concat . zipWith makeLens [(0 :: Int) ..] . toList
  where
    makeLens index (Rule inner _ _) = [ signature, function ]
      where
        fieldNm = recordFieldName index nm inner
        lensName = T.mkName fieldNm
        signature = T.SigD lensName
          $ (T.ConT ''Lens.Lens')
          `T.AppT` (T.ConT (quald qual nm))
          `T.AppT` (T.ConT (quald qual inner))

        function = T.FunD lensName [T.Clause [] (T.NormalB body) []]
          where
            namedRec = T.mkName "_namedRec"
            namedNewVal = T.mkName "_namedNewVal"
            body = (T.VarE 'Lens.lens) `T.AppE` getter `T.AppE` setter
              where
                getter = T.LamE [pat] expn
                  where
                    pat = T.VarP namedRec
                    expn = (T.VarE (T.mkName ('_' : fieldNm)))
                      `T.AppE` (T.VarE namedRec)

                setter = T.LamE [patRec, patNewVal] expn
                  where
                    patRec = T.VarP namedRec
                    patNewVal = T.VarP namedNewVal
                    expn = T.RecUpdE (T.VarE namedRec)
                      [ (T.mkName ('_' : fieldNm), T.VarE namedNewVal) ]


