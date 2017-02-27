source("_tools.R")

options(keep.source=TRUE)

## very minimal
x <- 2


out <- getOutput(compile(quote(x + 1)))
code <- c("GETVAR x", 
        "LDCONST 1", 
        "ADD", 
        "RETURN")



stopifnot(eqOut(operands(out), code))


## simple code generation

f <- function(x) x

out <- getOutput( compile( quote({f(1); f(2)}) ))
code <- c("GETFUN f",
            "PUSHCONSTARG 1", 
            "CALL f(1)", 
            "POP", 
            "GETFUN f", 
            "PUSHCONSTARG 2", 
            "CALL f(2)", 
            "RETURN")

stopifnot(eqOut(operands(out), code))


## names and ... args
f <- function(...) list(...)

out <- getOutput(cmpfun(f))
code <- c("BASEGUARD list(...) | $1",
            "GETFUN list", 
            "DODOTS", 
            "CALL list(...)", 
            "RETURN", 
            "1:", 
            "RETURN")

stopifnot(eqOut(operands(out), code))


## simple loops
sr <- function(x) {
    n <- length(x)
    i <- 1
    s <- 0
    repeat {
        if (i > n) break
        s <- s + x[i]
        i <- i + 1
    }
    s
}
sw <- function(x) {
    n <- length(x)
    i <- 1
    s <- 0
    while (i <= n) {
        s <- s + x[i]
        i <- i + 1
    }
    s
}
sf <- function(x) {
    s <- 0
    for (y in x)
        s <- s + y
    s
}
switchf <- function(type) {
 switch(type,
        mean = 1,
        median = 2,
        trimmed = 3)
}
closuref <- function(){
    f <- (function(){
        v <- 12
        function(){
            v * 10
        }
    })()

    f()+1
}




src <- cmpfun(sr)
swc <- cmpfun(sw)
sfc <- cmpfun(sf)
switchfc <- cmpfun(switchf)
closurefc <- cmpfun(closuref)

out <- getOutput(src)
code <- c("BASEGUARD length(x) | $1", "GETBUILTIN length", "GETVAR x", 
        "PUSHARG", "CALLBUILTIN length(x)", "1:", "SETVAR n", "POP", 
        "LDCONST 1", "SETVAR i", "POP", "LDCONST 0", "SETVAR s", "POP", 
        "2:", "GETVAR i", "GETVAR n", "GT", "BRIFNOT if (i > n) break | $3", 
        "GOTO $6", "GOTO $4", "3:", "LDNULL", "4:", "POP", "GETVAR s", 
        "GETVAR x", "STARTSUBSET_N x[i] | $5", "GETVAR_MISSOK i", "VECSUBSET x[i]", 
        "5:", "ADD", "SETVAR s", "POP", "GETVAR i", "LDCONST 1", "ADD", 
        "SETVAR i", "POP", "GOTO $2", "6:", "LDNULL", "POP", "GETVAR s", 
        "RETURN")
stopifnot(eqOut(operands(out), code))


out <- getOutput(swc)
code <- c("BASEGUARD length(x) | $1", "GETBUILTIN length", "GETVAR x", 
        "PUSHARG", "CALLBUILTIN length(x)", "1:", "SETVAR n", "POP", 
        "LDCONST 1", "SETVAR i", "POP", "LDCONST 0", "SETVAR s", "POP", 
        "2:", "GETVAR i", "GETVAR n", "LE", "BRIFNOT while (i <= n) { s <- s + x[i]; i <- i + 1 } | $4", 
        "GETVAR s", "GETVAR x", "STARTSUBSET_N x[i] | $3", "GETVAR_MISSOK i", 
        "VECSUBSET x[i]", "3:", "ADD", "SETVAR s", "POP", "GETVAR i", 
        "LDCONST 1", "ADD", "SETVAR i", "POP", "GOTO $2", "4:", "LDNULL", 
        "POP", "GETVAR s", "RETURN")
stopifnot(eqOut(operands(out), code))


out <- getOutput(sfc)
code <- c("LDCONST 0", "SETVAR s", "POP", "GETVAR x", "STARTFOR y | $2", 
        "1:", "GETVAR s", "GETVAR y", "ADD", "SETVAR s", "POP", "2:", 
        "STEPFOR $1", "ENDFOR", "POP", "GETVAR s", "RETURN")
stopifnot(eqOut(operands(out), code))

out <- getOutput(switchfc)
code <- c("GETVAR type", "SWITCH switch(type, mean = 1, median = 2, trimmed = 3) | c(\"mean\", \"median\", \"trimmed\", \"\") | c(11L, 14L, 17L, 8L) | c(11L, 14L, 17L, 8L)", 
        "LDNULL", "INVISIBLE", "RETURN", "LDCONST 1", "RETURN", "LDCONST 2", 
        "RETURN", "LDCONST 3", "RETURN")
stopifnot(eqOut(operands(out), code))


out <- getOutput(closurefc)
code <- c("MAKECLOSURE <FUNCTION>", "LDCONST 12", "SETVAR v", "POP", 
        "MAKECLOSURE <FUNCTION>", "GETVAR v", "LDCONST 10", "MUL", "RETURN", 
        "RETURN", "CHECKFUN", "CALL (function() { v <- 12; function() { v * 10 } })()", 
        "SETVAR f", "POP", "GETFUN f", "CALL f()", "LDCONST 1", "ADD", 
        "RETURN")
stopifnot(eqOut(operands(out), code))
