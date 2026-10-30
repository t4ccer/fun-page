# fun-page playground (proof of concept)

Edit a fun-page program in the browser and run it.
The playground is itself a fun-page site (built with GHC's JavaScript backend).
It runs programs with MicroHs, compiled to WebAssembly with fun-page embedded: the compiler compiles and interprets the program in a fresh preview frame.

Programs cannot add their own `foreign import javascript` (the JavaScript of imports is compiled into the WebAssembly when the playground is built) so only fun-page's are available.
Use `FunPage.JS` (`jsEval`, `jsAwait`) instead.

## Building

With Nix, from the fun-page checkout:

```sh
MICROHS=/path/to/MicroHs cabal run fun-page-playground
```

```sh
nix-build -A playground
```
