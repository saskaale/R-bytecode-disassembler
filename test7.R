library(compiler)
library(bctools)

options(keep.source = TRUE)


source("basics.R")

parsed <- compiler::cmpfun(sf)
#getParseText(parsed, c(2,3,4,5,6))
#lapply(attr(parsed, "srcref"), unclass)

print(compiler::disassemble(parsed), verbose=1)

