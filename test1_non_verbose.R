library(compiler)

options(keep.source = TRUE)

source("disasm.R")



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




dumpDisassemble(compiler::disassemble(compiler::cmpfun(r)), verbose=FALSE)