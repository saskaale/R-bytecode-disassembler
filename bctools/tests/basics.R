library(compiler)
library(bctools)

options(keep.source=TRUE)

getOutput <- function(bc){
    capture.output(print(compiler::disassemble(bc)))
}

operands <- function(out){
    out <- filterWhitespaces(out)

    #extract src and expr references
    out <- grep("(- #[0-9])|( @ )",out, value=TRUE, invert=TRUE)

    out
}

filterWhitespaces <- function(v){
    #extract empty lines
    v <- Filter(function(l){nchar(l) > 0}, v)

    #extract whitespaces
    lapply(v, function(v) gsub("(^\\s)|(\\s$)","",gsub("\\s+", " ", v)) )
}

eqOut <- function(c1, c2){
    identical(c1,c2)
}

## very minimal
x <- 2

out <- getOutput(compile(quote(x + 1)))
code <- c("GETVAR x", 
        "LDCONST 1", 
        "ADD", 
        "RETURN");

stopifnot(eqOut(operands(out), code));


## simple code generation

f <- function(x) x

out <- getOutput(compile( quote({f(1); f(2)})))
code <- c("GETFUN f",
            "PUSHCONSTARG 1", 
            "CALL f(1)", 
            "POP", 
            "GETFUN f", 
            "PUSHCONSTARG 2", 
            "CALL f(2)", 
            "RETURN");

stopifnot(eqOut(operands(out), code));


## names and ... args
f <- function(...) list(...)

out <- getOutput(cmpfun(f))
code <- c("GETFUN list", 
            "DODOTS", 
            "CALL list(...)", 
            "RETURN");

stopifnot(eqOut(operands(out), code));


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
src <- cmpfun(sr)
swc <- cmpfun(sw)
sfc <- cmpfun(sf)


out <- getOutput(src)
code <- c("GETFUN length", "MAKEPROM <INTERNAL_FUNCTION>", "CALL length(x)", 
        "SETVAR n", "POP", "LDCONST 1", "SETVAR i", "POP", "LDCONST 0", 
        "SETVAR s", "POP", "1:", "GETVAR i", "GETVAR n", "GT", "BRIFNOT if (i > n) break | $2", 
        "GOTO $5", "GOTO $3", "2:", "LDNULL", "3:", "POP", "GETVAR s", 
        "GETVAR x", "STARTSUBSET_N x[i] | $4", "GETVAR_MISSOK i", "VECSUBSET x[i]", 
        "4:", "ADD", "SETVAR s", "POP", "GETVAR i", "LDCONST 1", "ADD", 
        "SETVAR i", "POP", "GOTO $1", "5:", "LDNULL", "POP", "GETVAR s", 
        "RETURN");
stopifnot(eqOut(operands(out), code));


out <- getOutput(swc)
code <- c("GETFUN length", "MAKEPROM <INTERNAL_FUNCTION>", "CALL length(x)", 
        "SETVAR n", "POP", "LDCONST 1", "SETVAR i", "POP", "LDCONST 0", 
        "SETVAR s", "POP", "1:", "GETVAR i", "GETVAR n", "LE", "BRIFNOT while (i <= n) { s <- s + x[i] i <- i + 1 } | $3", 
        "GETVAR s", "GETVAR x", "STARTSUBSET_N x[i] | $2", "GETVAR_MISSOK i", 
        "VECSUBSET x[i]", "2:", "ADD", "SETVAR s", "POP", "GETVAR i", 
        "LDCONST 1", "ADD", "SETVAR i", "POP", "GOTO $1", "3:", "LDNULL", 
        "POP", "GETVAR s", "RETURN")
stopifnot(eqOut(operands(out), code));


out <- getOutput(sfc)
code <- c("LDCONST 0", "SETVAR s", "POP", "GETVAR x", "STARTFOR y | $2", 
        "1:", "GETVAR s", "GETVAR y", "ADD", "SETVAR s", "POP", "2:", 
        "STEPFOR $1", "ENDFOR", "POP", "GETVAR s", "RETURN")
stopifnot(eqOut(operands(out), code));