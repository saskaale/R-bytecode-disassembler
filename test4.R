library(compiler)

options(keep.source = TRUE)

source("disasm.R")



# This is an example function
r <- function(x, y) {
# v <- LETTERS[1:4]
 v <- c("a", "b", "c", "d")
 for ( i in v) {
    print(i)
 }
}




dumpDisassemble(compiler::disassemble(compiler::cmpfun(r)), verbose=1)