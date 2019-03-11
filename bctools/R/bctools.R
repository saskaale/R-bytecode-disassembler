library(compiler)

###
###
### Definition of argument types
###
###


SKIP.ARGTYPE<--1
LABEL.ARGTYPE<-0
CONSTANTS.ARGTYPE<-3
CONSTANTS_DBG.ARGTYPE<-4
CONSTANTS_LABEL.ARGTYPE<-5
BOOL.ARGTYPE<-11
INT.ARGTYPE<-10

###
###
### Names list of argument types internal usage
###
###

Opcodes.argtypes = list(
    LABEL = LABEL.ARGTYPE,
    CONSTANT = CONSTANTS.ARGTYPE,
    CONSTANT_LABEL = CONSTANTS_LABEL.ARGTYPE,
    CONSTANT_DBG = CONSTANTS_DBG.ARGTYPE,
    BOOL = BOOL.ARGTYPE,
    INT = INT.ARGTYPE
)

###
###
### Version of bytecode being annotated
###
###

Opcodes.bcversion = 10L

###
###
### Instruction annotation for every bytecode of the code
###
###

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
    SEQLEN.OP = c(SKIP.ARGTYPE),
    BASEGUARD.OP = c(CONSTANTS.ARGTYPE, LABEL.ARGTYPE)
)



conf <- new.env(parent = emptyenv())
conf$verbosity <- 0

#' Print bytecode object to output and returns it \emph{invisibly} (via \code{\link{invisible}(x)})
#'
#' \code{print.disassembly} print bytecode object into output in human-friendly way.
#'
#' This is implementation of print method for bytecode object.
#' It works under internal R Bytecode structure.
#' You can manually create bytecode object through \emph{compiler} package ( via for example \code{\link{cmpfun}} function )
#'
#' @param x Bytecode object to be printed
#' @param select instruction position to be highlighted
#' @param prefix number of spaces to print before each line ( used for intendation )
#' @param verbose verbosity level ( 0 or 1 or 2)
#'             0 (default value) - display only source references ( if they are available, if they aren't print expression references instead )
#'             1 - the same as 0 + display bytecode version and display expression references ( if they are available )
#'             2 - the same as 1 + display every operand's argument ( including ones used just for debugging )
#'             default value can be pre-set by \emph{bcverbose} function
#' @param maxdepth Maximum depth of nested functions which are printed
#' @param depth Current depth of nested functions which are being printed ( used for internal purposes in print recursion )
#' @param select Position of currently selected instruction ( used in debugger )
#' @param peephole Turn the peephole on - show just area surronding the selected instruction ( must have selected, used in debugger )
#' @param ... Numeric, complex, or logical vectors.
#'
#' @examples
#' library(compiler)
#' library(bctools)
#' a <- function(x){
#'   r <- 1
#'   while(x){
#'     r = r + x*x
#'     x = x-1
#'   }
#'   r
#' }
#' bc <- compiler::cmpfun(a)
#'
#' #these two does the same
#' disassemble(bc)
#' print(disassemble(bc))
#'
#' #manually set verbose level
#' print(disassemble(bc), verbose=1)
#' print(disassemble(bc), verbose=2)
#'
#' @export

printBC <- function(x, prefix="", verbose=NULL, constantpool=FALSE, maxdepth=2, depth=0, select=NULL, peephole=FALSE, ...){
    if( is.null(verbose) ) verbose <- conf$verbosity

    ### if the function is passed, calls the internal disassembly of the code
    if(typeof(x) == "closure")
        tryCatch({
            capture.output({    ### suppress output of the compiler::disassemble function
                x = compiler::disassemble(x)
            })
        }, error = function(e) {
            stop("An error occured trying to disassemble (compiler::disassemble) the passed object. Check whether is it BC compiled.")
        })

    #if you have the peephole turned on, you have to pass select flag for line
    if( peephole && is.null(select) )
      stop("if you have the peephole turned on ( peephole=TRUE ) , you have to pass select flag for line ( select=SOME_LINE )");

    #can contain BREAKPOINT[0-9] instructions
    code_breakpoint   <- x[[2]]
    #never contains BREAKPOINT[0-9] instruction
    code              <- x[[2]]
    #constant buffer
    constants         <- x[[3]]

    if(code[[1]] > Opcodes.bcversion){
        warning(paste0("The bytecode version of your GNU-R is not supported by the disassembler. ",
            "Please make update both your GNU-R and disasembler to the most recent versions"));
    }
    
    srcrefsIndex <- NULL
    expressionsIndex <- NULL
    srcref <- NULL

    #get needed properties from constants object
    for (cnst in rev(constants)){
      if (class(cnst)=="srcrefsIndex")     srcrefsIndex <- cnst
      if (class(cnst)=="expressionsIndex") expressionsIndex <- cnst
      if (class(cnst)=="srcref")           srcref <- cnst
    }

    dumpExpressions <- ( verbose > 0 || is.null(srcrefsIndex) ) && !is.null(expressionsIndex);
    dumpSrcrefs     <- !is.null(srcrefsIndex);

    #print leading source reference
    if(!peephole && !is.null(srcref)){
      environm <- attr(srcref, "srcfile")
      filename <- getSrcFilename(environm)
      if(!identical(filename, character(0))){
          cat(paste0(prefix,"@ ",filename,"#",srcref[[1]],"\n"))
      }
    }

    if(!peephole){
        if(verbose > 0) {
            cat(paste0(prefix,"Bytecode ver. ",code[[1]],"\n"))
        }
        cat("\n")
    }

    #first pass to mark instruction with labels
    #labels is array that describes if each instruction has label
    n <- length(code)
    labels <- rep(-2, n)        #labels now contains -2=not used, -1=used
    i <- 2
    instrCnt<-0 # count number of instructions
    while( i <= n ) {
        v <- code[[i]]
        argdescr <- Opcodes.argdescr[[paste0(v)]]
        j <- 1
        while(j <= length(argdescr)){
            i<-i+1
            if(argdescr[[j]] == Opcodes.argtypes$LABEL){
                labels[[ code[[i]] + 1 ]] <- -1
            }else if(argdescr[[j]] == Opcodes.argtypes$CONSTANT_LABEL){
                v <- constants[[ code[[i]] + 1 ]]
                if(!is.null(v)){
                    for(k in 1:length(v)){
                        labels[[v[[k]] + 1]] <- -1
                    }
                }
            }
            j<-j+1
        }
        instrCnt<-instrCnt+1
        i<-i+1
    }

    #second pass to count labels
    #loop through labels array and if that instruction has label marked on it
    #labels array now contains values: -2=not used, -1=used, >0=index of label
    i <- 2
    lastlabelno <- 0;
    while( i <= n ) {
        if(labels[[i]] == -1){
            lastlabelno <- lastlabelno+1
            labels[[i]] <- lastlabelno
        }
        i<-i+1
    }


    #functions to print each type of information ( arguments / source and expression references )
    #each functions return TRUE / FALSE that indicates if any output has been printed
    dumpConstant<-function(v){
        v <- constants[[v+1]]
        if(typeof(v) == "list"){

            #the max depth of recursion is definied via maxdepth parameter
            if(depth < maxdepth){
                if(typeof(v[[2]]) == "bytecode"){
                    v <- compiler::disassemble(v[[2]])
                    cat("<FUNCTION>")
                    print.disassembly(v, select=NULL, prefix=paste0(prefix,"   "),verbose=verbose, constantpool=constantpool, maxdepth=maxdepth, depth=depth+1)
                    cat("\n")
                }else{
                    cat("<INTERNAL_FUNCTION>")
                }
            }else{
                cat("<FUNCTION>")
            }

        }else{
            #hack to print expression tree in infix notation instead of prefix
            z <- capture.output(dput(v))
            z <- sub("(\\s)\\s*", "\\1", z, perl=TRUE)

            if(length(z) > 1){
                # convert >>while (i) {  print(i)  i <- i - 1 }<< to >>while (i) {  print(i);  i <- i - 1 }<<
                #   see the semicolon after print(i)

                #because the printed code does not contain ; after each instruction we have to add to it
                #called with first argument of current row and second of following row
                z <- mapply(function(cur, nex){

                    #current row does not end with { and following row does not start with } append
                    #   semilon after this instruction
                    if( length(grep("\\{\\s*$", cur)) <= 0 && length(grep("^\\s*\\}", nex)) <= 0 ){
                        paste0(cur, ";")
                    }else{
                        cur
                    }

                }, z, c(z[2:(length(z))], "}"))
            }

            cat(paste0(z))
        }
        TRUE
    }
    dumpDbgConstant <- function(v){
        #there are 2 types of constants in bytecode
        #this function corresponds to CONSTANT_DBG
        #  which means that this type of constant is used just for debugging inside bytecode
        if(verbose > 1){
            dumpConstant(v);
        }else{
            FALSE
        }
    }
    dumpConstantReferenceToArr <- function(v){
        cat(paste0( "#", v ))
        TRUE
    }
    dumpLabel <- function(v){
        cat(paste0( "$", labels[[ v+1 ]] ))
        TRUE
    }
    dumpConstantLabels <- function(v){
        if(is.null(v))
            return(dumpConstant(v))

        if(length(constants[[v+1]]) > 0){
            v <- lapply(constants[[v+1]],
                    function(v){
                        paste0("$", labels[[ v+1 ]])
                    })
            cat(paste(v,collapse=', '))
        }else{
            cat('-')
        }
        TRUE
    }
    dumpOp<-function(v){
        v <- sub("\\.OP$", "", v, perl=TRUE) # example "GOTO.OP" >> "GOTO"
        v <- sprintf("%-20s", v)
        cat(paste(v))
        TRUE
    }
    dumpValue<-function(v){
        cat(v)
        TRUE
    }
    dumpSrcRef<-function(cursrcref){
        filename <- getSrcFilename(cursrcref)
        lineno   <- getSrcLocation(cursrcref)

        o <- capture.output(print(cursrcref))

        cat(paste0(prefix," - ",filename,"#",lineno,": ",o[[1]],"\n"))
        TRUE
    }
    dumpExprRef<-function(exprIndex){
        cat(paste0(prefix,"  @ "))
        dumpConstant(exprIndex)
        cat("\n")
        TRUE
    }
    dumpUnknown<-function(v){
        cat("???")
        TRUE
    }


    printCodeArray <- function(){
        #third pass to print result
        selected <- FALSE
        lastExprIndex <- -1
        lastSrcrefsIndex <- -1
        i <- 2
        n <- length(code)
        printedInstructions <- 0
        while( i <= n ) {

            #extract instruction from code array
            instr <- code[[i]]
            instrname <- instr

            #instruction arguments description which is imported also from compiler package
            #contains array in which each parameter describes type of argument
            argdescr <- Opcodes.argdescr[[paste0(instr)]]

            #if the peephole mode is turned on we skip every elements before select
            # and all after 5 printed instructions after select
            if( peephole
                && printedInstructions < instrCnt-5
                && (i < select || printedInstructions >= 5) ){
                i <- i + 1 + length(argdescr)
                next
            }

            #this instruction has label pointing to it
            if(labels[[i]] > 0){
                cat(paste0(prefix,labels[[i]],":\n"))
            }

            if(dumpSrcrefs){
                curSrcrefsIndex <- srcrefsIndex[[i]]
                if(curSrcrefsIndex != lastSrcrefsIndex){
                    dumpSrcRef(constants[[curSrcrefsIndex + 1 ]])
                    lastSrcrefsIndex <- curSrcrefsIndex
                }
            }
            if(dumpExpressions){
                curExprIndex <- expressionsIndex[[i]]
                if(curExprIndex != lastExprIndex){
                    dumpExprRef(curExprIndex)
                    lastExprIndex <- curExprIndex
                }
            }


            #print prefix ( one of argument ) before each instruction
            pr <- paste0(prefix,"  ");
            if(!selected && !is.null(select) && (i - 1) >= select ) {
                #replace beginning of pr ( prefix ) with >>> ( current instruction )
                pr <- paste0(substr(pr, 1 ,nchar(pr)-3), ">>>")
                selected = TRUE
            }
            cat(pr)
            if(verbose > 0 || constantpool){
                cat(sprintf("%2d: ", i-1))
            }

            if(grepl("^BREAKPOINT[0-9]+\\.OP$", code_breakpoint[[i]])){
                instrname <- paste0("(BR) ", instrname)
            }

            #print instruction ( eg. ADD / SUB ... )
            dumpOp(instrname)

            #iterate over each argument of instruction and print them
            #the arguments are stored inside bytecode just after the instruction
            #  so as we loop through instructions we increments the index into
            #  code array ( i<-i+1 )
            j <- 1
            printed <- 0
            while(j <= length(argdescr)){
                if(printed >= 1){
                    cat("\t | ")
                }

                i<-i+1
                #extract instruction argument from code array
                v <- code[[i]]

                t = paste0(argdescr[[j]])


                #lookup table for argument types
                argTypesDump <- c(dumpLabel,
                                ifelse(constantpool,dumpConstantReferenceToArr,dumpConstant),
                                ifelse(constantpool,dumpConstantReferenceToArr,dumpDbgConstant),
                                ifelse(constantpool,dumpConstantReferenceToArr,dumpConstantLabels),
                                dumpValue,
                                dumpValue)
                names(argTypesDump) <- c(
                                Opcodes.argtypes$LABEL,
                                Opcodes.argtypes$CONSTANT,
                                Opcodes.argtypes$CONSTANT_DBG,
                                Opcodes.argtypes$CONSTANT_LABEL,
                                Opcodes.argtypes$BOOL,
                                Opcodes.argtypes$INT)

                dumpFun <- argTypesDump[[t]]
                if(is.null(dumpFun))
                    dumpFun <- dumpUnknown

                #printting the argument
                if(dumpFun(v))
                    printed <- printed + 1

                j<-j+1
            }
            printedInstructions<-printedInstructions+1
            i<-i+1

            cat("\n")
        }
        cat("\n")
    }

    printConstantArray <- function(){
        i <- 0
        n <- length(constants)
        printedInstructions <- 0
        while( i < n ) {
            cat(sprintf("  %2d: ", i))
            dumpConstant(i)
            cat("\n")
            i<-i+1
        }
    }


    if(constantpool){
        cat("-------- code array --------\n")
        printCodeArray()
        cat("------ constant array ------\n")
        printConstantArray()
    }else{
        printCodeArray()
    }

    #returns invisible(x) as the default behavior of print method
    invisible(x)
}


#' Try-catch wrapper over the print.disassembly function
#'
#' for additional instruction see the print.disassembly function definition
#'
#' @param x Bytecode object to be printed
#' @param ... additional parameters passed to print.disassembly code
#'
#' @export

tryPrint.disassembly <- function(x, ...)
    tryCatch(print.disassembly(x, ...), error = function(err) {
        cat(paste(gettext("Error: bytecode dump failed - "), err$message, "at", deparse(err$call), "\n"))
        x
    })


#' Set default verbosity level for bytecode \emph{print} method
#'
#' \code{bcverbose} Set and/or get default verbosity level for bytecode \emph{print} method
#'
#'
#' @param lvl verbosity level ( 0 or 1 or 2) - optional
#'           if setted - set default bytecode verbosity level
#'             0 - display only source references ( if they are available, if they aren't print expression references instead )
#'             1 - the same as 0 + display bytecode version and display expression references ( if they are available )
#'             2 - the same as 1 + display every operand's argument ( including ones used just for debugging )
#'
#'
#' @return current verbosity level
#'
#' @examples
#'
#' library(compiler)
#' library(bctools)
#' a <- function(x){
#'   r <- 1
#'   while(x){
#'     r = r + x*x
#'     x = x-1
#'   }
#'   r
#' }
#' bc <- compiler::cmpfun(a)
#'
#' #set default verbosity level
#' bcverbose(2)
#' disassemble(bc)
#'
#' @export

bcverbose <- function( lvl=NULL ){
    if( !is.null(lvl) ) { conf$verbosity <- lvl }
    conf$verbosity
}
