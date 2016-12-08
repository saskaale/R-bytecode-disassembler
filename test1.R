library(compiler)

options(keep.source = TRUE)

source("basics.R")
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



#dput(compiler::disassemble(compiler::cmpfun(r)));

parsed <- compiler::cmpfun(sf)
#getParseData(parsed)

dumpDisassemble(compiler::disassemble(parsed), verbose=1)