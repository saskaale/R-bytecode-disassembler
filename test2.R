library(compiler)

options(keep.source = TRUE)

source("disasm.R")



# This is an example function
r <- function(x, y) {
 i <- 0
 z <- 1
 while(i < 10){
    i <- i + 1
    z <- z + z*z
 }
 z
}




dumpDisassemble(compiler::disassemble(compiler::cmpfun(r)), verbose=1)