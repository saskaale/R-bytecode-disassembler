library(compiler)

#just checks version of current supported ByteCode

bc <- disassemble(cmpfun(function(x) x+1))
bcversion <- bc[[2]][[1]]

stopifnot(bcversion == 10)