source("_tools.R")

options(keep.source=TRUE)

## labels
x <- 2

whilef <- function(x){
    i <- 1;
    while(i < 10){
        dput("OUT");
        i <- i + 1;
    }
}

out <- getOutput(cmpfun(whilef))
stopifnot(!grepOp(out, "^0:$"));
stopifnot(grepOp(out, "^1:$"));
stopifnot(grepOp(out, "^2:$"));
stopifnot(!grepOp(out, "^3:$"));


