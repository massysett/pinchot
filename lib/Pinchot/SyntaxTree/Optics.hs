{-# LANGUAGE TemplateHaskell #-}
module Pinchot.SyntaxTree.Optics where

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
  NonTerminal b1 bs -> return $ nonTerminalToOptics qual nm b1 bs
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
  e1 <- T.sigD (T.mkName ('_':nm)) (T.conT ''Lens.Prism'
        `T.appT` T.conT termName
        `T.appT` T.conT (quald qual nm))
  e2 <- T.valD prismName (T.normalB expn) []
  return [e1, e2]
  where
    prismName = T.varP (T.mkName ('_' : nm))
    fetchPat = T.conP (quald qual nm) [T.varP (T.mkName "_x")]
    fetchName = T.varE (T.mkName "_x")
    ctor = T.conE (quald qual nm)
    expn = [| let fetch $fetchPat = $fetchName
                  store _term
                    | inIntervals ivls _term = Right ($ctor _term)
                    | otherwise = Left _term
              in Lens.prism fetch store
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
  -> [T.Dec]
nonTerminalToOptics qual nm b1 bsSeq
  = concat $ makePrism b1 : fmap makePrism bs
  where
    bs = toList bsSeq
    makePrism (Branch inner rulesSeq) = [ signature, binding ]
      where
        rules = toList rulesSeq
        prismName = T.mkName ('_' : inner)
        signature = T.SigD prismName
          $ (T.ConT ''Lens.Prism')
          `T.AppT` (T.ConT (quald qual nm))
          `T.AppT` fieldsType
          where
            fieldsType = case rules of
              [] -> T.TupleT 0
              Rule r1 _ _ : [] -> T.ConT (quald qual r1)
              rs -> foldl addType (T.TupleT (length rs)) rs
                where
                  addType soFar (Rule r _ _) = soFar `T.AppT`
                    (T.ConT (quald qual r))
        binding = T.ValD (T.VarP prismName) body []
          where
            body = T.NormalB
              $ (T.VarE 'Lens.prism)
              `T.AppE` setter
              `T.AppE` getter
              where
                setter = T.LamE [pat] expn
                  where
                    (pat, expn) = case rules of
                      [] -> (T.TupP [], T.ConE (quald qual inner))
                      _ : [] -> (T.VarP local,
                        T.ConE (quald qual inner)
                        `T.AppE` T.VarE local)
                        where
                          local = T.mkName "_x"
                      ls -> (T.TupP pats, set)
                        where
                          pats = fmap (\i -> T.VarP (T.mkName ("_x" ++ show i)))
                            . take (length ls) $ [(0 :: Int) ..]
                          set = foldl addVar start . take (length ls)
                            $ [(0 :: Int) ..]
                            where
                              addVar acc i = acc `T.AppE`
                                (T.VarE (T.mkName ("_x" ++ show i)))
                              start = T.ConE (quald qual inner)

                getter = T.LamE [pat] expn
                  where
                    local = T.mkName "_x"
                    pat = T.VarP local
                    expn = T.CaseE (T.VarE (T.mkName "_x")) $
                      T.Match patCtor bodyCtor []
                      : rest
                      where
                        patCtor = T.ConP (quald qual inner)
                          . fmap (\i -> T.VarP (T.mkName $ "_y" ++ show i))
                          . take (length rules)
                          $ [(0 :: Int) ..]
                        bodyCtor = T.NormalB . (T.ConE 'Right `T.AppE`)
                          $ case rules of
                          [] -> T.TupE []
                          _:[] -> T.VarE (T.mkName "_y0")
                          _ -> T.TupE
                            . fmap (\i -> T.VarE (T.mkName $ "_y" ++ show i))
                            . take (length rules)
                            $ [(0 :: Int) ..]
                        rest = case bs of
                          [] -> []
                          _ -> [T.Match patBlank bodyBlank []]
                          where
                            patBlank = T.VarP (T.mkName "_z")
                            bodyBlank = T.NormalB
                              $ T.ConE ('Left)
                              `T.AppE` T.VarE (T.mkName "_z")

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
  e1 <- T.sigD (T.mkName ('_':nm)) (T.conT ''Lens.Prism'
    `T.appT` (T.conT ''Seq `T.appT` T.conT termName)
    `T.appT` T.conT (quald qual nm))
  e2 <- T.valD prismName (T.normalB expn) []
  return [e1, e2]
  where
    prismName = T.varP (T.mkName ('_' : nm))
    fetchPat = T.conP (quald qual nm) [T.varP (T.mkName "_x")]
    fetchName = T.varE (T.mkName "_x")
    ctor = T.conE (T.mkName nm)
    expn = [| let fetch $fetchPat = $fetchName
                  store _term
                    | $(liftSeq sq) == _term = Right ($ctor _term)
                    | otherwise = Left _term
              in Lens.prism fetch store
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

