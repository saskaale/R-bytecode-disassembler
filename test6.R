library(compiler)

options(keep.source = TRUE)

source("disasm.R")



compiler::cmpfile("basics.R")
file <- compiler::loadcmp("basics.Rc")

dumpDisassemble(compiler::disassemble(file), verbose=1)