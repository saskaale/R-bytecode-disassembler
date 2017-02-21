library(compiler)

BCINFO <- bcinfo();
argtypes <- BCINFO$Argtypes
Opcodes.argdescr <- BCINFO$Arguments;

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
#' @param prefix number of spaces to print before each line ( used for intendation )
#' @param verbose verbosity level ( 0 or 1 or 2)
#'             0 - display only source references ( if they are available, if they aren't print expression references instead )
#'             1 - the same as 0 + display bytecode version and display expression references ( if they are available )
#'             2 - the same as 1 + display every operand's argument ( including ones used just for debugging )
#'             default value can be pre-set by \emph{bcverbose} function
#' @param maxdepth Maximum depth of nested functions which are printed
#' @param depth Current depth of nested functions which are being printed ( used for internal purposes in print recursion )
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

print.disassembly <- function(x, prefix="", verbose=NULL, maxdepth=2, depth=0, ...){
    if( is.null(verbose) ) verbose <- conf$verbosity

    constants <- x[[3]]
    code <- x[[2]]

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
    if(!is.null(srcref)){
      environm <- attr(srcref, "srcfile")
      filename <- getSrcFilename(environm)
      if(!identical(filename, character(0))){
          cat(paste0(prefix,"@ ",filename,"#",srcref[[1]],"\n"))
      }
    }

    if(verbose > 0)
        cat(paste0(prefix,"Bytecode ver. ",code[[1]],"\n"))

    #first pass to mark instruction with labels
    #labels is array that describes if each instruction has label
    n <- length(code) 
    labels <- rep(-2, n)        #labels now contains -2=not used, -1=used
    i <- 2
    while( i <= n ) {
        v <- code[[i]]
        argdescr <- Opcodes.argdescr[[paste0(v)]]
        j <- 1
        while(j <= length(argdescr)){
            i<-i+1
            v <- code[[i]]
            if(argdescr[[j]] == argtypes$LABEL){
              labels[[v+1]] <- -1
            }
            j<-j+1
        }
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
                    print.disassembly(v, paste0(prefix,"   "),verbose=verbose, maxdepth = maxdepth, depth=depth+1)
                    cat("\n")
                }else{
                    cat("<INTERNAL_FUNCTION>")
                }
            }

        }else{
            #hack to print expression tree in infix notation instead of prefix
            z <- capture.output(dput(v))
            z <- sub("(\\s)\\s*", "\\1", z, perl=TRUE)
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

    #third pass to print result
    lastExprIndex <- -1
    lastSrcrefsIndex <- -1
    i <- 2
    while( i <= n ) {

        #extract instruction from code array
        instr <- code[[i]]

        cat("\n")

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
        cat(paste0(prefix,"  "))

        #print instruction ( eg. ADD / SUB ... )
        dumpOp(instr)


        #instruction arguments description which is imported also from compiler package
        #contains array in which each parameter describes type of argument
        argdescr <- Opcodes.argdescr[[paste0(instr)]]

        #iterate over each argument of instruction and print them
        #the arguments are stored inside bytecode just after the instruction
        #  so as we loop through instructions we increments the index in them
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

            t = argdescr[[j]]
            if(t==argtypes$LABEL){
                if(dumpLabel(v))
                    printed <- printed + 1
            }else if(t==argtypes$CONSTANT){
                if(dumpConstant(v))
                    printed <- printed + 1
            }else if(t==argtypes$CONSTANT_DBG){
                if(dumpDbgConstant(v))
                    printed <- printed + 1
            }else if(t==argtypes$BOOL){
                if(dumpValue(v))
                    printed <- printed + 1
            }else if(t==argtypes$INT){
                if(dumpValue(v))
                    printed <- printed + 1
            }

            j<-j+1
        }
        i<-i+1
    }
    cat("\n")

    #returns invisible(x) as the default behavior of print method
    invisible(x)
}


#' Set default verbosity level for bytecode \emph{print} method
#'
#' \code{bcverbose} Set default verbosity level for bytecode \emph{print} method
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
