{-# LANGUAGE CPP #-}
-- Re-exports a whole backend module.
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- | The DOM primitives of the backend being compiled for:
--
-- * "FunPage.Internal.FFI.JS": GHC's JavaScript backend;
-- * "FunPage.Internal.FFI.MHS": MicroHs in the browser (the @browser@ flag; needs emscripten);
-- * "FunPage.Internal.FFI.Stubs": no DOM (native GHC, or MicroHs without the @browser@ flag).
--
-- The cabal file builds only the backend selected here.
module FunPage.Internal.FFI (module Backend) where

#if defined(javascript_HOST_ARCH)
import FunPage.Internal.FFI.JS as Backend
#elif defined(__MHS__) && defined(FUN_PAGE_BROWSER)
import FunPage.Internal.FFI.MHS as Backend
#else
import FunPage.Internal.FFI.Stubs as Backend
#endif
