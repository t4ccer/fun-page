{-# LANGUAGE ImportQualifiedPost #-}

-- | The playground's compiler: MicroHs, run in the browser (compiled with emscripten), with
-- fun-page and its dependencies embedded. It compiles and runs the module given on its command
-- line (@-r Main@), whose @main@ mounts a page.
--
-- fun-page's JavaScript imports are compiled in (@--embed-ffis@), because interpreted code cannot
-- add new ones. Importing "FunPage.Site" here also compiles in fun-page's exported event dispatcher.
module Compiler (main) where

import MicroHs.Main qualified

import FunPage.Site ()

main :: IO ()
main = MicroHs.Main.main
