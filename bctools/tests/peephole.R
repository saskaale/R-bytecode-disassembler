source("_tools.R")

options(keep.source=TRUE)


intSwitch <- function(x){
    switch(x, 0, -1, -2, -3, -4, -5, -6)
}

code <- c(
    ">>>RETURN",
    "5:",
    "LDCONST -3",
    "RETURN",
    "6:",
    "LDCONST -4",
    "RETURN"
)

out <- operands(getOutput(cmpfun(intSwitch), select=19, peephole=TRUE))
stopifnot(eqOut(out, code))
