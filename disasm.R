library(compiler)

Opcodes.argc <- list(
BCMISMATCH.OP = 0,
RETURN.OP = 0,
GOTO.OP = 1,
BRIFNOT.OP = 2,
POP.OP = 0,
DUP.OP = 0,
PRINTVALUE.OP = 0,
STARTLOOPCNTXT.OP = 2,
ENDLOOPCNTXT.OP = 1,
DOLOOPNEXT.OP = 0,
DOLOOPBREAK.OP = 0,
STARTFOR.OP = 3,
STEPFOR.OP = 1,
ENDFOR.OP = 0,
SETLOOPVAL.OP = 0,
INVISIBLE.OP = 0,
LDCONST.OP = 1,
LDNULL.OP = 0,
LDTRUE.OP = 0,
LDFALSE.OP = 0,
GETVAR.OP = 1,
DDVAL.OP = 1,
SETVAR.OP = 1,
GETFUN.OP = 1,
GETGLOBFUN.OP = 1,
GETSYMFUN.OP = 1,
GETBUILTIN.OP = 1,
GETINTLBUILTIN.OP = 1,
CHECKFUN.OP = 0,
MAKEPROM.OP = 1,
DOMISSING.OP = 0,
SETTAG.OP = 1,
DODOTS.OP = 0,
PUSHARG.OP = 0,
PUSHCONSTARG.OP = 1,
PUSHNULLARG.OP = 0,
PUSHTRUEARG.OP = 0,
PUSHFALSEARG.OP = 0,
CALL.OP = 1,
CALLBUILTIN.OP = 1,
CALLSPECIAL.OP = 1,
MAKECLOSURE.OP = 1,
UMINUS.OP = 1,
UPLUS.OP = 1,
ADD.OP = 1,
SUB.OP = 1,
MUL.OP = 1,
DIV.OP = 1,
EXPT.OP = 1,
SQRT.OP = 1,
EXP.OP = 1,
EQ.OP = 1,
NE.OP = 1,
LT.OP = 1,
LE.OP = 1,
GE.OP = 1,
GT.OP = 1,
AND.OP = 1,
OR.OP = 1,
NOT.OP = 1,
DOTSERR.OP = 0,
STARTASSIGN.OP = 1,
ENDASSIGN.OP = 1,
STARTSUBSET.OP = 2,
DFLTSUBSET.OP = 0,
STARTSUBASSIGN.OP = 2,
DFLTSUBASSIGN.OP = 0,
STARTC.OP = 2,
DFLTC.OP = 0,
STARTSUBSET2.OP = 2,
DFLTSUBSET2.OP = 0,
STARTSUBASSIGN2.OP = 2,
DFLTSUBASSIGN2.OP = 0,
DOLLAR.OP = 2,
DOLLARGETS.OP = 2,
ISNULL.OP = 0,
ISLOGICAL.OP = 0,
ISINTEGER.OP = 0,
ISDOUBLE.OP = 0,
ISCOMPLEX.OP = 0,
ISCHARACTER.OP = 0,
ISSYMBOL.OP = 0,
ISOBJECT.OP = 0,
ISNUMERIC.OP = 0,
VECSUBSET.OP = 1,
MATSUBSET.OP = 1,
VECSUBASSIGN.OP = 1,
MATSUBASSIGN.OP = 1,
AND1ST.OP = 2,
AND2ND.OP = 1,
OR1ST.OP = 2,
OR2ND.OP = 1,
GETVAR_MISSOK.OP = 1,
DDVAL_MISSOK.OP = 1,
VISIBLE.OP = 0,
SETVAR2.OP = 1,
STARTASSIGN2.OP = 1,
ENDASSIGN2.OP = 1,
SETTER_CALL.OP = 2,
GETTER_CALL.OP = 1,
SWAP.OP = 0,
DUP2ND.OP = 0,
SWITCH.OP = 4,
RETURNJMP.OP = 0,
STARTSUBSET_N.OP = 2,
STARTSUBASSIGN_N.OP = 2,
VECSUBSET2.OP = 1,
MATSUBSET2.OP = 1,
VECSUBASSIGN2.OP = 1,
MATSUBASSIGN2.OP = 1,
STARTSUBSET2_N.OP = 2,
STARTSUBASSIGN2_N.OP = 2,
SUBSET_N.OP = 2,
SUBSET2_N.OP = 2,
SUBASSIGN_N.OP = 2,
SUBASSIGN2_N.OP = 2,
LOG.OP = 1,
LOGBASE.OP = 1,
MATH1.OP = 2,
DOTCALL.OP = 2,
COLON.OP = 1,
SEQALONG.OP = 1,
SEQLEN.OP = 1
)

Opcodes.names <- names(Opcodes.argc)


BCMISMATCH.OP <- 0
RETURN.OP <- 1
GOTO.OP <- 2
BRIFNOT.OP <- 3
POP.OP <- 4
DUP.OP <- 5
PRINTVALUE.OP <- 6
STARTLOOPCNTXT.OP <- 7
ENDLOOPCNTXT.OP <- 8
DOLOOPNEXT.OP <- 9
DOLOOPBREAK.OP <- 10
STARTFOR.OP <- 11
STEPFOR.OP <- 12
ENDFOR.OP <- 13
SETLOOPVAL.OP <- 14
INVISIBLE.OP <- 15
LDCONST.OP <- 16
LDNULL.OP <- 17
LDTRUE.OP <- 18
LDFALSE.OP <- 19
GETVAR.OP <- 20
DDVAL.OP <- 21
SETVAR.OP <- 22
GETFUN.OP <- 23
GETGLOBFUN.OP <- 24
GETSYMFUN.OP <- 25
GETBUILTIN.OP <- 26
GETINTLBUILTIN.OP <- 27
CHECKFUN.OP <- 28
MAKEPROM.OP <- 29
DOMISSING.OP <- 30
SETTAG.OP <- 31
DODOTS.OP <- 32
PUSHARG.OP <- 33
PUSHCONSTARG.OP <- 34
PUSHNULLARG.OP <- 35
PUSHTRUEARG.OP <- 36
PUSHFALSEARG.OP <- 37
CALL.OP <- 38
CALLBUILTIN.OP <- 39
CALLSPECIAL.OP <- 40
MAKECLOSURE.OP <- 41
UMINUS.OP <- 42
UPLUS.OP <- 43
ADD.OP <- 44
SUB.OP <- 45
MUL.OP <- 46
DIV.OP <- 47
EXPT.OP <- 48
SQRT.OP <- 49
EXP.OP <- 50
EQ.OP <- 51
NE.OP <- 52
LT.OP <- 53
LE.OP <- 54
GE.OP <- 55
GT.OP <- 56
AND.OP <- 57
OR.OP <- 58
NOT.OP <- 59
DOTSERR.OP <- 60
STARTASSIGN.OP <- 61
ENDASSIGN.OP <- 62
STARTSUBSET.OP <- 63
DFLTSUBSET.OP <- 64
STARTSUBASSIGN.OP <- 65
DFLTSUBASSIGN.OP <- 66
STARTC.OP <- 67
DFLTC.OP <- 68
STARTSUBSET2.OP <- 69
DFLTSUBSET2.OP <- 70
STARTSUBASSIGN2.OP <- 71
DFLTSUBASSIGN2.OP <- 72
DOLLAR.OP <- 73
DOLLARGETS.OP <- 74
ISNULL.OP <- 75
ISLOGICAL.OP <- 76
ISINTEGER.OP <- 77
ISDOUBLE.OP <- 78
ISCOMPLEX.OP <- 79
ISCHARACTER.OP <- 80
ISSYMBOL.OP <- 81
ISOBJECT.OP <- 82
ISNUMERIC.OP <- 83
VECSUBSET.OP <- 84
MATSUBSET.OP <- 85
VECSUBASSIGN.OP <- 86
MATSUBASSIGN.OP <- 87
AND1ST.OP <- 88
AND2ND.OP <- 89
OR1ST.OP <- 90
OR2ND.OP <- 91
GETVAR_MISSOK.OP <- 92
DDVAL_MISSOK.OP <- 93
VISIBLE.OP <- 94
SETVAR2.OP <- 95
STARTASSIGN2.OP <- 96
ENDASSIGN2.OP <- 97
SETTER_CALL.OP <- 98
GETTER_CALL.OP <- 99
SWAP.OP <- 100
DUP2ND.OP <- 101
SWITCH.OP <- 102
RETURNJMP.OP <- 103
STARTSUBSET_N.OP <- 104
STARTSUBASSIGN_N.OP <- 105
VECSUBSET2.OP <- 106
MATSUBSET2.OP <- 107
VECSUBASSIGN2.OP <- 108
MATSUBASSIGN2.OP <- 109
STARTSUBSET2_N.OP <- 110
STARTSUBASSIGN2_N.OP <- 111
SUBSET_N.OP <- 112
SUBSET2_N.OP <- 113
SUBASSIGN_N.OP <- 114
SUBASSIGN2_N.OP <-115
LOG.OP <- 116
LOGBASE.OP <- 117
MATH1.OP <- 118
DOTCALL.OP <- 119
COLON.OP <- 120
SEQALONG.OP <- 121
SEQLEN.OP <- 122



dumpDisassemble <- function(raw, prefix="", verbose=FALSE, deph=0){
    maxdeph <- 3

    constants <- raw[[3]]
    code <- raw[[2]]

    for (cnst in rev(constants)){
      if (class(cnst)=="srcrefsIndex")     srcrefsIndex <- cnst
      if (class(cnst)=="expressionsIndex") expressionsIndex <- cnst
      if (class(cnst)=="srcref")           srcref <- cnst
    }

#    dput(srcref);
#   dput(srcrefsIndex);


    dumpExpressions <- exists("expressionsIndex") && exists("srcrefsIndex");
    dumpSrcrefs     <- exists("expressionsIndex") && exists("srcrefsIndex");

    #pre-process expressions index to find out last expression of each source expression
    if(dumpExpressions){

        myExpressionsIndex <- rep(-1, length(expressionsIndex));

        n <- length(code)
        i <- n
        lastSrcRef <- -1
        lastExprIndex <- -1;
        while( i > 1 ) {
            srcRef <- srcrefsIndex[[i]]
            if(srcRef != lastSrcRef){
                lastSrcRef <- srcRef
                lastExprIndex <- expressionsIndex[[i]]
            }
            myExpressionsIndex[[i]] <- lastExprIndex

            i <- i-1
        }
    }


    #print leading source reference
    if(exists("srcref")){
      environm <- attr(srcref, "srcfile")
      filename <- get("filename", envir=environm)
      if(nchar(filename) > 0){
          cat(paste0(prefix,"@ ",filename,"#",srcref[[1]],"\n"))
      }
    }





    #2 operands and address is on the 2nd place
    op2addr2 <- c(GOTO.OP, STEPFOR.OP)
    #3 operands and address is on the 3rd place
    op3addr3 <- c(BRIFNOT.OP, AND1ST.OP, OR1ST.OP, STARTSUBSET_N.OP, STARTLOOPCNTXT.OP)
    #4 operands and address is on the 4th place
    op4addr4 <- c(STARTFOR.OP)

    op2addr2 <- lapply(op2addr2, function(v){Opcodes.names[v+1]})
    op3addr3 <- lapply(op3addr3, function(v){Opcodes.names[v+1]})
    op4addr4 <- lapply(op4addr4, function(v){Opcodes.names[v+1]})

    if(verbose)
        cat(paste0(prefix,"Bytecode ver. ",code[[1]],"\n"))

    #first pass to mark instruction with labels
    n <- length(code)
    labels <- rep(-2, n)        # -2=not used, -1=used, >0=index of label
    i <- 2
    while( i <= n ) {
        v <- code[[i]]
        if( paste0(v) %in% op2addr2 ){
            i<-i+1
            v <- code[[i]]
            labels[[v+1]] <- -1
        }else if( paste0(v) %in% op3addr3 ){
            i<-i+2
            v <- code[[i]]
            labels[[v+1]] <- -1
        }else if( paste0(v) %in% op4addr4 ){
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
            if(deph < maxdeph){

#                dput(v);
                if(typeof(v[[2]]) == "bytecode"){
                    v <- compiler::disassemble(v[[2]])
                    cat("<FUNCTION>")
                    dumpDisassemble(v, paste0(prefix,"   "),verbose=verbose, deph=deph+1)
                    cat("\n")
                }else{
                    cat("<INTERNAL_FUNCTION>")
#                    dumpDisassemble(v, paste0(prefix,"   "),verbose=verbose, deph=deph+1)
#                    cat("\n")
#                    dput(v);
                }

            }
        }else{
            #hack to print expression tree in infix notation instead of prefix
            z <- capture.output(dput(v))
            z <- sub("(\\s)\\s*", "\\1", z, perl=TRUE);
            cat(paste0(z))
        }
    }

    dumpLabel<-function(v){
        cat(paste0( "$", labels[[ v+1 ]] ))
    }
    dumpOp<-function(v){
        v <- sub("\\.OP$", "", v, perl=TRUE) # example "GOTO.OP" >> "GOTO"
        v <- sprintf("%-20s", v)
        cat(paste(v))
    }
    lastExprIndex <- -1

    while( i <= n ) {
        v <- code[[i]]

        cat("\n")

        if(labels[[i]] > 0){
            cat(paste0(prefix,labels[[i]],":\n"))
        }


        if(dumpExpressions){
            curExprIndex <- myExpressionsIndex[[i]]
            if(curExprIndex != lastExprIndex){
                cat(paste0(prefix,"  @ "))
                dumpConstant(curExprIndex)
                cat("\n")
                lastExprIndex <- curExprIndex
            }
        }

        cat(paste0(prefix,"  "))
        dumpOp(v)

        if( paste0(v) %in% op2addr2 ){
            i<-i+1
            v <- code[[i]]
            dumpLabel(v)
        }else if( paste0(v) %in% op3addr3 ){
            i<-i+1
            v <- code[[i]]
            dumpConstant(v)
            i<-i+1
            v <- code[[i]]
            cat("\t")
            dumpLabel(v)
        }else if( paste0(v) %in% op4addr4 ){
            i<-i+1
            v <- code[[i]]
            dumpConstant(v)
            i<-i+1
            v <- code[[i]]
            cat("\t")
            dumpConstant(v)
            i<-i+1
            v <- code[[i]]
            cat("\t")
            dumpLabel(v)
        }else{
            #every other instruction is treated as instruction following with only constants arguments

            first <- TRUE
            ni <- i+1
            while(ni <= n && typeof(code[[ni]]) == "integer"){   #reference to constant table
                i<-ni
                v <- code[[i]]
                if(first){
                    first = FALSE
                }else{
                    cat("\t")
                }
                dumpConstant(v)
                ni <- i+1
            }

        }
        i<-i+1
    }
    cat("\n")
}
