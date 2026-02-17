{- |
   Empty Haskell module. Compiled with ghc -threaded to pull in
   the threaded RTS and all base library closures that the RTS needs.
   The C program provides main() via -no-hs-main.
-}
module HsStub where
