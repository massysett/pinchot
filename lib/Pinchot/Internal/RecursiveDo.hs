{-# LANGUAGE TemplateHaskell #-}

-- | Emulating recursive do notation, as TH does not support it.
module Pinchot.Internal.RecursiveDo where

import Control.Monad.Fix (mfix)
import qualified Language.Haskell.TH as T

-- | Creates a lazy pattern for all the given names.  Adds an empty
-- pattern onto the front.  This is the counterpart of 'bigTuple'.
-- All of the given names are bound.  In addition, a single,
-- wildcard pattern is bound to the front.
-- 
-- For example, @lazyPattern (map mkName ["x", "y", "z"])@ gives a
-- pattern that looks like
--
-- @~(_, (x, (y, (z, ()))))@
--
-- The idea is that the named patterns are needed so that the
-- recursive @do@ notation works, and that the wildcard pattern is
-- the return value, which is not needed here.
lazyPattern
  :: Foldable c
  => c T.Name
  -> T.Q T.Pat
lazyPattern = finish . foldr gen [p| () |]
  where
    gen name rest = [p| ($(T.varP name), $rest) |]
    finish pat = [p| ~(_, $pat) |]

-- | Creates a big tuple.  It is nested in the second element, such
-- as (1, (2, (3, (4, ())))).  Thus, the big tuple is terminated
-- with a unit value.  It resembles a list where each tuple is a
-- cons cell and the terminator is unit.
bigTuple
  :: Foldable c
  => T.ExpQ
  -- ^ This expression will be the first one in the tuple.
  -> c T.ExpQ
  -- ^ Remaining expressions in the tuple.
  -> T.ExpQ
bigTuple top = finish . foldr f [| () |]
  where
    f n rest = [| ( $(n), $rest) |]
    finish tup = [| ($(top), $tup) |]

-- | Builds a recursive @do@ expression (because TH has no support
-- for @mdo@ notation).
recursiveDo
  :: [(T.Name, T.ExpQ)]
  -- ^ Binding statements
  -> T.ExpQ
  -- ^ Final return value from @do@ block.  The type of this 'ExpQ'
  -- must be in the same monad as the @do@ block; it must not be a
  -- pure value.
  -> T.ExpQ
  -- ^ Returns an expression whose value is the final return value
  -- from the @do@ block.
recursiveDo binds final = [| fmap fst $ mfix $(fn) |]
  where
    fn = [| \ $(lazyPattern (fmap fst binds)) -> $doBlock |]
    doBlock = T.doE (bindStmts ++ returnStmts)
    bindStmts = map mkBind binds
      where
        mkBind (name, exp)
          = T.bindS (T.varP name) exp
    returnStmts = [bindRtnVal, returner]
      where
        rtnValName = T.mkName "_returner"
        bindRtnVal = T.bindS (T.varP rtnValName) final
        returner
          = T.noBindS
            [| return $(bigTuple (T.varE rtnValName) 
                                 (fmap (T.varE . fst) binds)) |]

