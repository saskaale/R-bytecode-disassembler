source("_tools.R")

options(keep.source=TRUE)

## labels

strSwitch <- function(x){
    switch(x, "FRONT", "BACK")
}

intSwitch <- function(x){
    switch(x, 0, -1, -2)
}

code <- c("GETVAR x",
"SWITCH switch(x, \"FRONT\", \"BACK\") | NULL | - | $2, $3, $1",
"1:",
"LDNULL",
"INVISIBLE",
"RETURN",
"2:",
"LDCONST \"FRONT\"",
"RETURN",
"3:",
"LDCONST \"BACK\"",
"RETURN")

out <- getOutput(cmpfun(strSwitch))
stopifnot(eqOut(operands(out), code))

code <- c("GETVAR x",
"SWITCH switch(x, 0, -1, -2) | NULL | - | $2, $3, $4, $1",
"1:",
"LDNULL",
"INVISIBLE",
"RETURN",
"2:",
"LDCONST 0",
"RETURN",
"3:",
"LDCONST -1",
"RETURN",
"4:",
"LDCONST -2",
"RETURN")


out <- getOutput(cmpfun(intSwitch))
stopifnot(eqOut(operands(out), code))
