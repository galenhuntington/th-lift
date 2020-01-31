{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper functions used in code that "Language.Haskell.TH.Lift" generates.
--
-- Note: this is an internal module, and as such, the API presented here is not
-- guaranteed to be stable, even between minor releases of this library.
module Language.Haskell.TH.Lift.Internal where

#if MIN_VERSION_template_haskell(2,16,0)
import GHC.Exts (RuntimeRep, TYPE)
#endif

import Language.Haskell.TH.Syntax

-- | A type-restricted version of 'error' that ensures 'makeLift' always
-- returns a value of type @q 'Exp'@ (where @q@ is an instance of 'Quote'),
-- even when used on an empty datatype.
#if MIN_VERSION_template_haskell(2,17,0)
errorQuoteExp :: Quote q => String -> q Exp
#else
errorQuoteExp ::            String -> Q Exp
#endif
errorQuoteExp = error

-- | This is an alias for 'unsafeCodeCoerce', if built with
-- @template-haskell-2.17.0.0@ or later, or 'unsafeTExpCoerce', if built with
-- older versions of @template-haskell@.
unsafeCodeCoerceCompat ::
#if MIN_VERSION_template_haskell(2,17,0)
  forall (r :: RuntimeRep) (a :: TYPE r) m . Quote m => m Exp -> Code m a
unsafeCodeCoerceCompat = unsafeCodeCoerce
#elif MIN_VERSION_template_haskell(2,16,0)
  forall (r :: RuntimeRep) (a :: TYPE r) . Q Exp -> Q (TExp a)
unsafeCodeCoerceCompat = unsafeTExpCoerce
#else
  forall a . Q Exp -> Q (TExp a)
unsafeCodeCoerceCompat = unsafeTExpCoerce
#endif
