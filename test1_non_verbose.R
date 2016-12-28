library(compiler)
library(bctools)

options(keep.source = TRUE)

source("basics.R")



# This is an example function
r <- function(x, y) {
 z <- x * x + y
 z <- sin(z) + z^2
 if(x > y){
   x * x + y
 }else{
   x * x
 }
}




print(compiler::disassemble(compiler::cmpfun(sf)), verbose=FALSE)