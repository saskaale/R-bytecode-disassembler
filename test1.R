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



parsed <- compiler::cmpfun(sf)

print(compiler::disassemble(parsed), verbose=1)