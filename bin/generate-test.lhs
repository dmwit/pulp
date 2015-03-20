#!runghc -ilib
Example usage:
bin/generate-test.lhs <tests/foo.log >tests/foo.out

> import Text.Pulp
> import GHC.IO.Encoding
> main = setLocaleEncoding latin1 >> interact (uglyPrint . parse)
