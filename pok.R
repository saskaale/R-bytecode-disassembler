library(compiler)

source("basics.R")

options(keep.source = TRUE)

dumpDisassemble <- function(raw, prefix="", deph=0){
    maxdeph <- 3

#    dput(raw)

    constants <- raw[[3]]
    code <- raw[[2]]

    cat(paste0(prefix,"Bytecode ver. ",code[[1]],"\n"))

    #first pass to mark instruction with labels
    n <- length(code)
    labels <- rep(-2, n)        # -2=not used, -1=used, >0=index of label
    i <- 2
    while( i <= n ) {
        v <- code[[i]]
        if(
            v == "GOTO.OP"
            ||
            v == "STEPFOR.OP"
        ){
            i<-i+1
            v <- code[[i]]
            labels[[v+1]] <- -1
        }else if(
            v == "BRIFNOT.OP"
            ||
            v == "AND1ST.OP"
            ||
            v == "OR1ST.OP"
            ||
            v == "STARTSUBSET_N.OP"
            ||
            v == "STARTLOOPCNTXT.OP"
        ){
            i<-i+2
            v <- code[[i]]
            labels[[v+1]] <- -1
        }else if(
            v == "STARTFOR.OP"
        ){
            i<-i+3
            v <- code[[i]]
            labels[[v+1]] <- -1
        }else{
            #every other instruction is treated as instruction following with only constants arguments

        }
        i<-i+1
    }

    #second pass to count labels
    i <- 2
    lastlabelno <- 0;
    while( i <= n ) {
        if(labels[[i]] == -1){
            lastlabelno <- lastlabelno+1
            labels[[i]] <- lastlabelno
        }
        i<-i+1
    }

    #third pass to print result
    i <- 2
    dumpConstant<-function(v){
        v <- constants[[v+1]]
        if(typeof(v) == "list"){
            cat(" <CLOSURE>")
            if(deph < maxdeph){
                cat("\n")
                dumpDisassemble(v, paste0(prefix,"   "),deph+1)
            }
        }else{
            cat(paste0("\t",v))
        }
    }
    dumpLabel<-function(v){
#        cat(paste0( "\t#",labels[[ v+1 ]], "->", code[[v+1]] ))
        cat(paste0( "\t#",labels[[ v+1 ]] ))
    }
    dumpOp<-function(v){
        cat(paste(v))
    }
    cat(paste0(prefix,"#0:"))
    while( i <= n ) {
        v <- code[[i]]

        cat("\n")
        if(labels[[i]] > 0){
            cat(paste0(prefix,"#",labels[[i]],":\n"))
        }
        cat(paste0(prefix,"  "))
        dumpOp(v)

        if(
            v == "GOTO.OP"
            ||
            v == "STEPFOR.OP"
        ){
            i<-i+1
            v <- code[[i]]
            dumpLabel(v)
        }else if(
            v == "BRIFNOT.OP"
            ||
            v == "AND1ST.OP"
            ||
            v == "OR1ST.OP"
            ||
            v == "STARTSUBSET_N.OP"
            ||
            v == "STARTLOOPCNTXT.OP"
        ){
            i<-i+1
            v <- code[[i]]
            dumpConstant(v)
            i<-i+1
            v <- code[[i]]
            dumpLabel(v)
        }else if(
            v == "STARTFOR.OP"
        ){
            i<-i+1
            v <- code[[i]]
            dumpConstant(v)
            i<-i+1
            v <- code[[i]]
            dumpConstant(v)
            i<-i+1
            v <- code[[i]]
            dumpLabel(v)
        }else{
            #every other instruction is treated as instruction following with only constants arguments

            ni <- i+1
            while(ni <= n && typeof(code[[ni]]) == "integer"){   #reference to constant table or label
                i<-ni
                v <- code[[i]]
                dumpConstant(v)
                ni <- i+1
            }

        }
        i<-i+1
    }
    cat("\n")
}


r <- function(x,y){
    ret <- 20

    i <- 2
    while( i < 10 && (i < 100 || i > 140) ){
        i <- i + f()
        r <- r + i
    }

    ret
}



getSrcFilename(sr)
getSrcLocation(sr)

#d <- compiler::disassemble(compiler::cmpfun(function(x,y) x + y))
#compiler::disassemble(compiler::cmpfun(sr))
dumpDisassemble(compiler::disassemble(compiler::cmpfun(r)))

#help(getSrcref)

#dput(d)