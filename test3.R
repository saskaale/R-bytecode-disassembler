library(compiler)

options(keep.source = TRUE)

source("disasm.R")



# This is an example function
r <- function(x, y) {
 i <- 0
 z <- 1
 repeat{
    i <- i + 1
    z <- z + z*z
    if(i>10){
        break;
    }
 }
 z
}




dumpDisassemble(compiler::disassemble(compiler::cmpfun(r)), verbose=1)