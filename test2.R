library(compiler)
library(bctools)

options(keep.source = TRUE)



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




print(compiler::disassemble(compiler::cmpfun(r)), verbose=1)