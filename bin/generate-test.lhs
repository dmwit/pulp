#!runghc -ilib
Example usage:
bin/generate-test.lhs <tests/foo.log >tests/foo.out

> import Text.Pulp
> main = interact (uglyPrint . parse)
