source("_tools.R")

options(keep.source=TRUE)

## labels

# This is an example function
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
code <- c(
            "",
            " - #2: f <- (function(){", 
            "  MAKECLOSURE         <FUNCTION>", 
            "    - #3: v <- 12", 
            "     LDCONST             12", 
            "     SETVAR              v", 
            "     POP                 ", 
            "    - #4: function(){", 
            "     MAKECLOSURE         <FUNCTION>", 
            "       - #5: v * 10", 
            "        GETVAR              v", 
            "        LDCONST             10", 
            "        MUL                 ", 
            "        RETURN              ", 
            "", 
            "", 
            "     RETURN              ", 
            "", 
            "", 
            "  CHECKFUN            ", 
            "  CALL                (function() {  v <- 12  function() {  v * 10  } })()", 
            "  SETVAR              f", 
            "  POP                 ", 
            " - #9: f()+1", 
            "  GETFUN              f", 
            "  CALL                f()", 
            "  LDCONST             1", 
            "  ADD                 ", 
            "  RETURN              ")
stopifnot(eqOut(out, code))

