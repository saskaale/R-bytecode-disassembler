library(compiler)
library(bctools)

options(keep.source = TRUE)

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



print(compiler::disassemble(compiler::cmpfun(r)), verbose=1)