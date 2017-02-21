########################################################
############ tests for bcverbose function ##############


source("_tools.R")
options(keep.source=TRUE)





#default verbosity level is 0
stopifnot(bcverbose() == 0)


#default verbose level get and set
stopifnot(bcverbose(0) == 0)
stopifnot(bcverbose(2) == 2)
stopifnot(bcverbose() == 2)
stopifnot(bcverbose(NULL) == 2)
stopifnot(bcverbose() == 2)



#test for applying default verbosity level >> checks if Bytecode version occures in lvl 1

bcverbose(0) #reset default version to 0

stopifnot(truefromlvl(function(lvl){
        bcverbose(lvl)
        out <- getOutput(compile(quote(x + 1)))
        grepOp(out, "Bytecode ver\\.")
    }, 1))

