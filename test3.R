library(compiler)
library(bctools)

options(keep.source = TRUE)



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




print(compiler::disassemble(compiler::cmpfun(r)), verbose=1)