library(compiler)
library(bctools)

options(keep.source = TRUE)

# This is an example function
factory <- function(a){
  f <- compiler::cmpfun(function(x){a*x})
  function(x){
    f(x)
  }
}

r2 <- factory(2)
r3 <- factory(3)


dput("Dumping r2")
compiled <- compiler::cmpfun(r2)
print(compiler::disassemble(compiled), verbose=1)

dput("Dumping r3")
compiled <- compiler::cmpfun(r3)
print(compiler::disassemble(compiled), verbose=1)