source("_tools.R")

options(keep.source=TRUE)


x <- 2

whilef <- function(v) { 
    i<- 10; 
    while(i) { 
        v <- v + x; 
        i <- i - 1;
    }
    v
};

addf <- function(v){
    v+1
}


### test for info about bytecode version
stopifnot(truefromlvl(function(lvl){
        out <- getOutput(compile(quote(x + 1)), verbose=lvl)
        grepOp(out, "Bytecode ver\\.")
    }, 1))





### test for source references

f <- function(v) v + x;
stopifnot(truefromlvl(function(lvl){
        out <- getOutput(cmpfun(f), verbose=lvl)
        grepOp(out, "- #")
    }, 0))




### test for expr references

stopifnot(truefromlvl(function(lvl){
        out <- getOutput(cmpfun(whilef), verbose=lvl)
        grepOp(out, "@ while")
    }, 1))


### printing aditional arguments ( for example in "ADD      v + 1" )

stopifnot(truefromlvl(function(lvl){
        out <- getOutput(cmpfun(addf), verbose=lvl)
        grepOp(out, "ADD[[:space:]]+v \\+ 1$")
    }, 2))















