library(compiler)

options(keep.source = TRUE)

source("disasm.R")

# This is an example function
r <- function(){
    f <- (function(){
        v <- 12
        function(){
            v * 10
        }
    })()

    f()+1
}



dumpDisassemble(compiler::disassemble(compiler::cmpfun(r)), verbose=TRUE)