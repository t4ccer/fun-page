{-# LANGUAGE CPP #-}

-- | Call stacks, for the source locations in markup errors.
module FunPage.Internal.Stack (HasCallStack, CallStack, callStack, callSiteText) where

-- MicroHs has 'HasCallStack' but no 'CallStack' values, so there errors have no location. Has an
-- export list (unlike other internal modules) because it re-exports from "GHC.Stack".
#if defined(__MHS__)
import Data.Kind (Type)
import GHC.Stack (HasCallStack)

type CallStack :: Type
data CallStack = CallStack

callStack :: CallStack
callStack = CallStack

-- | Where the call stack was captured, as @file:line:column: @ (empty under MicroHs).
callSiteText :: CallStack -> String
callSiteText _ = ""
#else
import GHC.Stack (CallStack, HasCallStack, callStack, getCallStack, srcLocFile, srcLocStartCol, srcLocStartLine)

-- | Where the call stack was captured, as @file:line:column: @.
callSiteText :: CallStack -> String
callSiteText cs = case getCallStack cs of
  (_, loc) : _ -> srcLocFile loc <> ":" <> show (srcLocStartLine loc) <> ":" <> show (srcLocStartCol loc) <> ": "
  [] -> ""
#endif
