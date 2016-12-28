library(compiler)
library(bctools)

options(keep.source = TRUE)




compiler::cmpfile("basics.R")
file <- compiler::loadcmp("basics.Rc")

print(compiler::disassemble(file), verbose=1)