Haskell Regex Generator
=======================

Generate a random string of bytes from a PCRE.

Example
-------

```haskell
import Text.Regex.Regen (generate)

main :: IO ()
main = print =<< generate "^foo[[:alpha:]\\d]{3,5}(?<GROUP>xyz)\\k<GROUP>"
```
