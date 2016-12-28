library(compiler)
library(bctools)

options(keep.source = TRUE)




# This is an example function
r <- function(type) {
 switch(type,
        mean = 1,
        median = 2,
        trimmed = 3)
}





print(compiler::disassemble(compiler::cmpfun(r)), verbose=1)