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
code <- c("", 
          "  @ function() {  v <- 12;  function() {  v * 10  } }", 
          "  MAKECLOSURE         <FUNCTION>", 
          "     @ 12", 
          "     LDCONST             12", 
          "     @ v <- 12", 
          "     SETVAR              v", 
          "     POP                 ", 
          "     @ function() {  v * 10 }", 
          "     MAKECLOSURE         <FUNCTION>", 
          "        @ v", 
          "        GETVAR              v", 
          "        @ 10", 
          "        LDCONST             10", 
          "        @ v * 10", 
          "        MUL                 ", 
          "        RETURN              ", 
          "", 
          "", 
          "     RETURN              ", 
          "", 
          "", 
          "  @ (function() {  v <- 12;  function() {  v * 10  } })()", 
          "  CHECKFUN            ", 
          "  CALL                (function() {  v <- 12;  function() {  v * 10  } })()", 
          "  @ f <- (function() {  v <- 12;  function() {  v * 10  } })()", 
          "  SETVAR              f", 
          "  POP                 ", 
          "  @ f()", 
          "  GETFUN              f", 
          "  CALL                f()", 
          "  @ 1", 
          "  LDCONST             1", 
          "  @ f() + 1", 
          "  ADD                 ", 
          "  RETURN              ")
stopifnot(eqOut(out, code))

