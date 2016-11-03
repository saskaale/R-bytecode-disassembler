library(compiler)

options(keep.source = TRUE)

source("disasm.R")

# This is an example function
factory <- function(a){
  f <- compiler::cmpfun(function(x){a*x})
  function(x){
    f(x)
  }
}

r2 <- factory(2)
r3 <- factory(3)


#r2(1)
#r2(2)

#r3(-1)
#r3(-2)

dput("Dumping r2")
compiled <- compiler::cmpfun(r2)
dumpDisassemble(compiler::disassemble(compiled), verbose=TRUE)

dput("Dumping r3")
compiled <- compiler::cmpfun(r3)
dumpDisassemble(compiler::disassemble(compiled), verbose=TRUE)