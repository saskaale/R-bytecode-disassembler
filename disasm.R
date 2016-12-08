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


SKIP.ARGTYPE<--1
LABEL.ARGTYPE<-0
CONSTANTS.ARGTYPE<-3
CONSTANTS_DBG.ARGTYPE<-4
BOOL.ARGTYPE<-11
INT.ARGTYPE<-10

Opcodes.argdescr <- list(

BCMISMATCH.OP = c(),
RETURN.OP = c(),
GOTO.OP = c(LABEL.ARGTYPE),
BRIFNOT.OP = c(CONSTANTS.ARGTYPE,LABEL.ARGTYPE),
POP.OP = c(),
DUP.OP = c(),
PRINTVALUE.OP = c(),
STARTLOOPCNTXT.OP = c(BOOL.ARGTYPE, LABEL.ARGTYPE),#  bool is_for_loop, pc for break
ENDLOOPCNTXT.OP = c(BOOL.ARGTYPE),
DOLOOPNEXT.OP = c(),
DOLOOPBREAK.OP = c(),
STARTFOR.OP = c(CONSTANTS_DBG.ARGTYPE, CONSTANTS.ARGTYPE, LABEL.ARGTYPE),
STEPFOR.OP = c(LABEL.ARGTYPE),
ENDFOR.OP = c(),
SETLOOPVAL.OP = c(),
INVISIBLE.OP = c(),
LDCONST.OP = c(CONSTANTS.ARGTYPE),
LDNULL.OP = c(),
LDTRUE.OP = c(),
LDFALSE.OP = c(),
GETVAR.OP = c(CONSTANTS.ARGTYPE),
DDVAL.OP = c(CONSTANTS.ARGTYPE),
SETVAR.OP = c(CONSTANTS.ARGTYPE),
GETFUN.OP = c(CONSTANTS.ARGTYPE),
GETGLOBFUN.OP = c(CONSTANTS.ARGTYPE),
GETSYMFUN.OP = c(CONSTANTS.ARGTYPE),
GETBUILTIN.OP = c(CONSTANTS.ARGTYPE),
GETINTLBUILTIN.OP = c(CONSTANTS.ARGTYPE),
CHECKFUN.OP = c(),
MAKEPROM.OP = c(CONSTANTS.ARGTYPE),
DOMISSING.OP = c(),
SETTAG.OP = c(CONSTANTS.ARGTYPE),
DODOTS.OP = c(),
PUSHARG.OP = c(),
PUSHCONSTARG.OP = c(CONSTANTS.ARGTYPE),
PUSHNULLARG.OP = c(),
PUSHTRUEARG.OP = c(),
PUSHFALSEARG.OP = c(),
CALL.OP = c(CONSTANTS.ARGTYPE),
CALLBUILTIN.OP = c(CONSTANTS.ARGTYPE),
CALLSPECIAL.OP = c(CONSTANTS.ARGTYPE),
MAKECLOSURE.OP = c(CONSTANTS.ARGTYPE),
UMINUS.OP = c(CONSTANTS_DBG.ARGTYPE),
UPLUS.OP = c(CONSTANTS_DBG.ARGTYPE),
ADD.OP = c(CONSTANTS_DBG.ARGTYPE),
SUB.OP = c(CONSTANTS_DBG.ARGTYPE),
MUL.OP = c(CONSTANTS_DBG.ARGTYPE),
DIV.OP = c(CONSTANTS_DBG.ARGTYPE),
EXPT.OP = c(CONSTANTS_DBG.ARGTYPE),
SQRT.OP = c(CONSTANTS_DBG.ARGTYPE),
EXP.OP = c(CONSTANTS_DBG.ARGTYPE),
EQ.OP = c(CONSTANTS_DBG.ARGTYPE),
NE.OP = c(CONSTANTS_DBG.ARGTYPE),
LT.OP = c(CONSTANTS_DBG.ARGTYPE),
LE.OP = c(CONSTANTS_DBG.ARGTYPE),
GE.OP = c(CONSTANTS_DBG.ARGTYPE),
GT.OP = c(CONSTANTS_DBG.ARGTYPE),
AND.OP = c(CONSTANTS_DBG.ARGTYPE),
OR.OP = c(CONSTANTS_DBG.ARGTYPE),
NOT.OP = c(CONSTANTS_DBG.ARGTYPE),
DOTSERR.OP = c(),
STARTASSIGN.OP = c(CONSTANTS.ARGTYPE),
ENDASSIGN.OP = c(CONSTANTS.ARGTYPE),
STARTSUBSET.OP = c(CONSTANTS.ARGTYPE, LABEL.ARGTYPE),
DFLTSUBSET.OP = c(),
STARTSUBASSIGN.OP = c(CONSTANTS.ARGTYPE, LABEL.ARGTYPE),
DFLTSUBASSIGN.OP = c(),
STARTC.OP = c(CONSTANTS.ARGTYPE, LABEL.ARGTYPE),
DFLTC.OP = c(),
STARTSUBSET2.OP = c(CONSTANTS.ARGTYPE, LABEL.ARGTYPE),
DFLTSUBSET2.OP = c(),
STARTSUBASSIGN2.OP = c(CONSTANTS.ARGTYPE, LABEL.ARGTYPE),
DFLTSUBASSIGN2.OP = c(),
DOLLAR.OP = c(CONSTANTS.ARGTYPE, CONSTANTS.ARGTYPE),
DOLLARGETS.OP = c(CONSTANTS.ARGTYPE, CONSTANTS.ARGTYPE),
ISNULL.OP = c(),
ISLOGICAL.OP = c(),
ISINTEGER.OP = c(),
ISDOUBLE.OP = c(),
ISCOMPLEX.OP = c(),
ISCHARACTER.OP = c(),
ISSYMBOL.OP = c(),
ISOBJECT.OP = c(),
ISNUMERIC.OP = c(),
VECSUBSET.OP = c(CONSTANTS.ARGTYPE),
MATSUBSET.OP = c(CONSTANTS.ARGTYPE),
VECSUBASSIGN.OP = c(CONSTANTS.ARGTYPE),
MATSUBASSIGN.OP = c(CONSTANTS.ARGTYPE),
AND1ST.OP = c(CONSTANTS_DBG.ARGTYPE, LABEL.ARGTYPE),
AND2ND.OP = c(CONSTANTS_DBG.ARGTYPE),
OR1ST.OP = c(CONSTANTS_DBG.ARGTYPE, LABEL.ARGTYPE),
OR2ND.OP = c(CONSTANTS_DBG.ARGTYPE),
GETVAR_MISSOK.OP = c(CONSTANTS.ARGTYPE),
DDVAL_MISSOK.OP = c(CONSTANTS.ARGTYPE),
VISIBLE.OP = c(),
SETVAR2.OP = c(CONSTANTS.ARGTYPE),
STARTASSIGN2.OP = c(CONSTANTS.ARGTYPE),
ENDASSIGN2.OP = c(CONSTANTS.ARGTYPE),
SETTER_CALL.OP = c(CONSTANTS.ARGTYPE, CONSTANTS.ARGTYPE),
GETTER_CALL.OP = c(CONSTANTS.ARGTYPE),
SWAP.OP = c(),
DUP2ND.OP = c(),
SWITCH.OP = c(CONSTANTS.ARGTYPE, CONSTANTS.ARGTYPE, CONSTANTS.ARGTYPE, CONSTANTS.ARGTYPE),
RETURNJMP.OP = c(),
STARTSUBSET_N.OP = c(CONSTANTS.ARGTYPE, LABEL.ARGTYPE),
STARTSUBASSIGN_N.OP = c(CONSTANTS.ARGTYPE, LABEL.ARGTYPE),
VECSUBSET2.OP = c(CONSTANTS.ARGTYPE),
MATSUBSET2.OP = c(CONSTANTS.ARGTYPE),
VECSUBASSIGN2.OP = c(CONSTANTS.ARGTYPE),
MATSUBASSIGN2.OP = c(CONSTANTS.ARGTYPE),
STARTSUBSET2_N.OP = c(CONSTANTS.ARGTYPE, LABEL.ARGTYPE),
STARTSUBASSIGN2_N.OP = c(CONSTANTS.ARGTYPE, LABEL.ARGTYPE),
SUBSET_N.OP = c(CONSTANTS.ARGTYPE, INT.ARGTYPE),
SUBSET2_N.OP = c(CONSTANTS.ARGTYPE, INT.ARGTYPE),
SUBASSIGN_N.OP = c(CONSTANTS.ARGTYPE, INT.ARGTYPE),
SUBASSIGN2_N.OP = c(CONSTANTS.ARGTYPE, INT.ARGTYPE),
LOG.OP = c(CONSTANTS.ARGTYPE),
LOGBASE.OP = c(CONSTANTS.ARGTYPE),
MATH1.OP = c(CONSTANTS.ARGTYPE, INT.ARGTYPE), #second argument is one of the math1funs
DOTCALL.OP = c(CONSTANTS.ARGTYPE, INT.ARGTYPE),
COLON.OP = c(SKIP.ARGTYPE),
SEQALONG.OP = c(SKIP.ARGTYPE),
SEQLEN.OP = c(SKIP.ARGTYPE)
)

Opcodes.argc <- lapply(Opcodes.argdescr, length)

#  {"floor", NULL, floor},
#  {"ceiling", NULL, ceil},
#  {"sign", NULL, sign},
#
#  {"expm1", NULL, expm1},
#  {"log1p", NULL, log1p},
#
#  {"cos", NULL, cos},
#  {"sin", NULL, sin},
#  {"tan", NULL, tan},
#  {"acos", NULL, acos},
#  {"asin", NULL, asin},
#  {"atan", NULL, atan},
#
#  {"cosh", NULL, cosh},
#  {"sinh", NULL, sinh},
#  {"tanh", NULL, tanh},
#  {"acosh", NULL, acosh},
#  {"asinh", NULL, asinh},
#  {"atanh", NULL, atanh},
#
#  {"lgamma", NULL, lgammafn},
#  {"gamma", NULL, gammafn},
#  {"digamma", NULL, digamma},
#  {"trigamma", NULL, trigamma},
#
#  {"cospi", NULL, cospi},
#  {"sinpi", NULL, sinpi},
# ##ifndef HAVE_TANPI
#  {"tanpi", NULL, tanpi}
#  #else
#  {"tanpi", NULL, Rtanpi}



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



dumpDisassemble <- function(raw, prefix="", verbose=0, deph=0){
    maxdeph <- 3

    constants <- raw[[3]]
    code <- raw[[2]]

    for (cnst in rev(constants)){
      if (class(cnst)=="srcrefsIndex")     srcrefsIndex <- cnst
      if (class(cnst)=="expressionsIndex") expressionsIndex <- cnst
      if (class(cnst)=="srcref")           srcref <- cnst
    }

    dumpExpressions <- verbose > 0 && exists("expressionsIndex") && exists("srcrefsIndex");
    dumpSrcrefs     <- exists("expressionsIndex") && exists("srcrefsIndex");

    #pre-process expressions index to find out last expression of each source expression
    if(dumpExpressions || dumpSrcrefs){

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
      filename <- getSrcFilename(environm)
#      dput(getParseText(environm))
      if(!identical(filename, character(0))){
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

    if(verbose > 0)
        cat(paste0(prefix,"Bytecode ver. ",code[[1]],"\n"))

    #first pass to mark instruction with labels
    n <- length(code)
    labels <- rep(-2, n)        # -2=not used, -1=used, >0=index of label
    i <- 2
    while( i <= n ) {
        v <- code[[i]]
        argdescr <- Opcodes.argdescr[[paste0(v)]]
        j <- 1
        while(j <= length(argdescr)){
            i<-i+1
            v <- code[[i]]
            if(argdescr[[j]] == LABEL.ARGTYPE){
              labels[[v+1]] <- -1
            }
            j<-j+1
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
    dumpConstant<-function(v){
        v <- constants[[v+1]]
        if(typeof(v) == "list"){
            if(deph < maxdeph){
                if(typeof(v[[2]]) == "bytecode"){
                    v <- compiler::disassemble(v[[2]])
                    cat("<FUNCTION>")
                    dumpDisassemble(v, paste0(prefix,"   "),verbose=verbose, deph=deph+1)
                    cat("\n")
                }else{
                    cat("<INTERNAL_FUNCTION>")
                }

            }
        }else{
            #hack to print expression tree in infix notation instead of prefix
            z <- capture.output(dput(v))
            z <- sub("(\\s)\\s*", "\\1", z, perl=TRUE);
            cat(paste0(z))
        }
        TRUE
    }
    dumpDbgConstant <- function(v){
        if(verbose > 1){
            dumpConstant(v);
        }else{
            FALSE
        }
    }
    dumpLabel<-function(v){
        cat(paste0( "$", labels[[ v+1 ]] ))
        TRUE
    }
    dumpOp<-function(v){
        v <- sub("\\.OP$", "", v, perl=TRUE) # example "GOTO.OP" >> "GOTO"
        v <- sprintf("%-20s", v)
        cat(paste(v))
        TRUE
    }
    dumpSrcRef<-function(cursrcref){
        filename <- getSrcFilename(cursrcref);
        lineno   <- getSrcLocation(cursrcref);

        o <- capture.output(print(cursrcref))

        cat(paste0(prefix," - ",filename,"#",lineno,": ",o[[1]],"\n"))
        TRUE
    }
    dumpValue<-function(v){
        cat(v)
        TRUE
    }
    lastExprIndex <- -1

    i <- 2
    while( i <= n ) {
        v <- code[[i]]

        cat("\n")

        #print labels
        if(labels[[i]] > 0){
            cat(paste0(prefix,labels[[i]],":\n"))
        }

        if(dumpExpressions || dumpSrcrefs){
            curExprIndex <- myExpressionsIndex[[i]]

            if(curExprIndex != lastExprIndex){
                if(dumpSrcrefs){
                    cursrcref <- constants[[srcrefsIndex[[i]] + 1 ]];
                    dumpSrcRef(cursrcref)
                }
                if(dumpExpressions){
                    cat(paste0(prefix,"  @ "))
                    dumpConstant(curExprIndex)
                    cat("\n")
                }

                lastExprIndex <- curExprIndex
            }
        }

        cat(paste0(prefix,"  "))
        dumpOp(v)

        argdescr <- Opcodes.argdescr[[paste0(v)]]

        j <- 1
        printed <- 0
        while(j <= length(argdescr)){
            if(printed >= 1){
                cat("\t | ")
            }

            i<-i+1
            v <- code[[i]]

            t = argdescr[[j]]
            if(t==LABEL.ARGTYPE){
                if(dumpLabel(v))
                    printed <- printed + 1
            }else if(t==CONSTANTS.ARGTYPE){
                if(dumpConstant(v))
                    printed <- printed + 1
            }else if(t==CONSTANTS_DBG.ARGTYPE){
                if(dumpDbgConstant(v))
                    printed <- printed + 1
            }else if(t==BOOL.ARGTYPE){
                if(dumpValue(v))
                    printed <- printed + 1
            }else if(t==INT.ARGTYPE){
                if(dumpValue(v))
                    printed <- printed + 1
            }

            j<-j+1
        }
        i<-i+1
    }
    cat("\n")
}
