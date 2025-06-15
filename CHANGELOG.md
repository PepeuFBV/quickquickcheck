# Revision history for quickquickcheck

## 0.1.0.0 -- 02/06/2025 -- Pedro Figueira

* First version. Released on an unsuspecting world.
* Initial cabal and docs setup.

## 0.1.1.0 -- 02/06/2025 -- Pedro Figueira

* Added reading haskell files to extract the annoation + function declaration combination.

## 0.1.1.1 -- 02/06/2025 -- Pedro Figueira

* Changed the annotation format to use `-- @quickcheck` instead of `-- qqc:`.

## 0.1.1.2 -- 05/06/2025 -- Pedro Figueira

* Bug fix for missing cabal file in the package.
* Fixed quickcheck call in Test.hs file to the correct usage.

## 0.1.2.0 -- 14/06/2025 -- Pedro Figueira

* Added support for multiple annotations (line by line) above the function declaration.
