library(compiler)
library(bctools)

options(keep.source = TRUE)

#source("disasm.R")



# This is an example function
r <- function(x, y) {
# v <- LETTERS[1:4]
 add <- function(v){
  v+10
 }
 x*y
}




#dput(compiler::cmpfun(r))
print(compiler::disassemble(compiler::cmpfun(r)), verbose=1)