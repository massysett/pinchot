{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Template Haskell names and values.
module Pinchot.Names where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as St
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language.Haskell.TH as T

-- | @t@
nameT :: T.Name
nameT = T.mkName "t"

-- | @a@
nameA :: T.Name
nameA = T.mkName "a"

-- | @r@
nameR :: T.Name
nameR = T.mkName "r"

-- | @t@ as a type
typeT :: T.TypeQ
typeT = T.varT nameT

-- | @a@ as a type
typeA :: T.TypeQ
typeA = T.varT nameA

-- | @r@ as a type
typeR :: T.TypeQ
typeR = T.varT nameR

-- | @t@ as a TyVarBndr
tyVarBndrT :: T.TyVarBndr
tyVarBndrT = T.PlainTV nameT

-- | @a@ as a TyVarBndr
tyVarBndrA :: T.TyVarBndr
tyVarBndrA = T.PlainTV nameA

-- | @r@ as a TyVarBndr
tyVarBndrR :: T.TyVarBndr
tyVarBndrR = T.PlainTV nameR

productionsStr :: String
productionsStr = "Productions"

-- | @Productions@
productions :: T.Name
productions = T.mkName productionsStr

-- | @a'@ followed by the given string.
recordName :: String -> T.Name
recordName n = T.mkName $ "a'" ++ n

-- | Qualified record name.
qualRecordName :: Qualifier -> String -> T.Name
qualRecordName q s = quald q ("a'" ++ s)

newtype Namer a = Namer (St.StateT (Map String T.Name) T.Q a)
  deriving (Functor, Applicative, Monad)

-- | Get th Name that corresponds to a particular string.  If
-- necessary, creates the name.
getName :: String -> Namer T.Name
getName str = Namer $ do
  names <- St.get
  case Map.lookup str names of
    Just n -> return n
    Nothing -> do
      new <- lift $ T.newName ("_getName_" ++ str)
      let newMap = Map.insert str new names
      St.put newMap
      return new

namerNewName :: Namer T.Name
namerNewName = Namer $ lift (T.newName "_namerNewName")

runNamer :: Namer a -> T.Q a
runNamer (Namer n) = fmap fst $ (St.runStateT n) Map.empty

-- | Many functions take an argument that holds the name qualifier
-- for the module that contains the data types created by applying a
-- function such as 'Pinchot.SyntaxTree.syntaxTrees' or
-- 'Pinchot.Earley.earleyProduct'.
--
-- You will have to make sure that these data types are in scope.
-- The spliced Template Haskell code has to know where to
-- look for these data types.  If you did an unqualified @import@ or
-- if the types are in the same module as the function that takes a
-- 'Qualifier' argument, just pass the empty string here.  If you did a
-- qualified import, use the appropriate qualifier here.
--
-- For example, if you used @import qualified MyAst@, pass
-- @\"MyAst\"@ here.  If you used @import qualified
-- Data.MyLibrary.MyAst as MyLibrary.MyAst@, pass
-- @\"MyLibrary.MyAst\"@ here.
type Qualifier = String


-- | Prepends a qualifier to a string, and returns the resulting
-- Name.
quald
  :: Qualifier
  -> String
  -- ^ Item to be named - constructor, value, etc.
  -> T.Name
quald qual suf
  | null qual = T.mkName suf
  | otherwise = T.mkName (qual ++ '.':suf)
