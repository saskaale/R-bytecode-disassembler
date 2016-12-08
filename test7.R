library(compiler)

options(keep.source = TRUE)


source("basics.R")
source("disasm.R")

parsed <- compiler::cmpfun(sf)
#getParseText(parsed, c(2,3,4,5,6))
#lapply(attr(parsed, "srcref"), unclass)

dumpDisassemble(compiler::disassemble(parsed), verbose=1)

