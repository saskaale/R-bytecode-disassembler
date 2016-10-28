library(compiler)

options(keep.source = TRUE)

source("disasm.R")



# This is an example function
r <- function(type) {
 switch(type,
        mean = 1,
        median = 2,
        trimmed = 3)
}





dumpDisassemble(compiler::disassemble(compiler::cmpfun(r)), verbose=TRUE)