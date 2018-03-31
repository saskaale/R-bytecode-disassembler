source("_tools.R")

options(keep.source=TRUE)

## closures
r <- function(){
    f <- (function(){
        v <- 12
        function(){
            v * 10
        }
    })()

    f()+1
}

out <- getOutput(cmpfun(r))
code <- c("MAKECLOSURE <FUNCTION>",
          "LDCONST 12",
          "SETVAR v",
          "POP",
          "MAKECLOSURE <FUNCTION>",
              "GETVAR v",
              "LDCONST 10",
              "MUL",
              "RETURN",
          "RETURN",
          "CHECKFUN",
          "CALL (function() { v <- 12; function() { v * 10 } })()",
          "SETVAR f",
          "POP",
          "GETFUN f",
          "CALL f()",
          "LDCONST 1",
          "ADD",
          "RETURN")

stopifnot(eqOut(operands(out), code))
