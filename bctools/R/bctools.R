library(compiler)

BCINFO=bcinfo();
argtypes = BCINFO$Argtypes
Opcodes.argdescr <- BCINFO$Arguments;


#' Print bytecode object to output and returns it \emph{invisibly} (via \code{\link{invisible}(x)})
#'
#' \code{print.disassembly} print bytecode object into output in human-friendly way.
#'
#' This is implementation of print method for bytecode object. 
#' It works under internal R Bytecode structure.
#'
#' @param x Bytecode object to be printed
#' @param prefix number of spaces to print before each line ( used for intendation )
#' @param verbose verbosity level ( 0 or 1 or 2)
#'             0 - display only source references ( if they are available )
#'             1 - the same as 0 + display bytecode version and display expression references ( if they are available )
#'             2 - the same as 1 + display every operand's argument ( including ones used just for debugging )
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
#' print(compiler::disassemble(bc));
#' print(compiler::disassemble(bc), verbose=1);
#' print(compiler::disassemble(bc), verbose=2);
#'
#' @export

print.disassembly <- function(x, prefix="", verbose=0, maxdepth=3, depth=0, ...){
    constants <- x[[3]]
    code <- x[[2]]

    srcrefsIndex <- NULL
    expressionsIndex <- NULL
    srcref <- NULL


    for (cnst in rev(constants)){
      if (class(cnst)=="srcrefsIndex")     srcrefsIndex <- cnst
      if (class(cnst)=="expressionsIndex") expressionsIndex <- cnst
      if (class(cnst)=="srcref")           srcref <- cnst
    }


    dumpExpressions <- verbose > 0 && !is.null(expressionsIndex) && !is.null(srcrefsIndex);
    dumpSrcrefs     <- !is.null(expressionsIndex) && !is.null(srcrefsIndex);

    #pre-process expressions index to find out last expression of each source expression
    #there is need to preprocess the expressions 
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
            if(argdescr[[j]] == argtypes$LABEL){
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

    dumpConstant<-function(v){
        v <- constants[[v+1]]
        if(typeof(v) == "list"){
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
    dumpValue<-function(v){
        cat(v)
        TRUE
    }
    dumpSrcRef<-function(cursrcref){
        filename <- getSrcFilename(cursrcref);
        lineno   <- getSrcLocation(cursrcref);

        o <- capture.output(print(cursrcref))

        cat(paste0(prefix," - ",filename,"#",lineno,": ",o[[1]],"\n"))
        TRUE
    }
    dumpExprRef<-function(exprIndex){
        cat(paste0(prefix,"  @ "))
        dumpConstant(exprIndex)
        cat("\n")
    }

    #third pass to print result
    lastExprIndex <- -1
    i <- 2
    while( i <= n ) {
        v <- code[[i]]

        cat("\n")

        #this instruction has label pointing to it
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
                    dumpExprRef(curExprIndex)
                }

                lastExprIndex <- curExprIndex
            }
        }


        #print prefix ( one of argument ) before each instruction
        cat(paste0(prefix,"  "))

        #print instruction ( eg. ADD )
        dumpOp(v)

        argdescr <- Opcodes.argdescr[[paste0(v)]]

        #iterate over each argument of instruction and print them
        j <- 1
        printed <- 0
        while(j <= length(argdescr)){
            if(printed >= 1){
                cat("\t | ")
            }

            i<-i+1
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
