############################FUNCTIONS############################

match_columns <- function(columns){
  n <- length(columns)
  max_len = max(unlist(lapply(columns, length)))
  new_cols <- list()
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      v <- rep(NA,length = max_len)
      for (k in seq(min(length(columns[[i]]), length(columns[[j]])))){
        if(columns[[i]][k] == columns[[j]][k]) v[k] <- columns[[i]][k]
      }
      nm <- paste0("col",i,"vcol",j)
      new_cols[[nm]] <- v
    }
  }
  data.frame(new_cols)
}

hhi <- function(y) {
  # calculate sum
  total <- rowSums(y)
  # calculate share
  share <- y/total
  # add
  return(rowSums(share^2))#c(,rowSums(-1*share*log(share)/log(total)))
}

affcon <- function(y) { #enjoy the fu..nction! source: https://solomonmessing.wordpress.com/2012/09/30/working-with-bipartiteaffiliation-network-data-in-r/
  A <- spMatrix(
    nrow=length(unique(y[,2])), #number of rows (the affiliated)
    ncol=length(unique(y[,1])), #number of columns (the affiliation)
    i = as.numeric(factor(y[,2])), #unique classes of rows (the affiliated)
    j = as.numeric(factor(y[,1])), #unique classes of rows (the affiliation)
    x = rep(1, length(as.numeric(y[,2])))) #content: 1 for the length of the edgelist to denote an incidence
  row.names(A) <- levels(factor(y[,2])) #naming rows
  colnames(A) <- levels(factor(y[,1])) #naming columns
  return(tcrossprod(A))
}

freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), "\\, |\\,| ")))), n)
}


# 
# file_find_replace <- function(source_path, target_path, pattern, replacement) {
#   file_contents <- readLines(source_path)
#   updated_contents <- gsub(x = file_contents, pattern = pattern, replacement = replacement)
#   # write_lines(updated_contents, file = target_path, sep = "\n")
#   cat(updated_contents, file = target_path, sep = "\n")
#   
# }

install.packages("gsubfn")
library(gsubfn)

file_find_replace <- function(source_path, target_path, toreplace) {
  file_contents <- readLines(source_path)
  updated_contents <- gsubfn(paste(names(toreplace), collapse="|"), toreplace, x= file_contents)
  # write_lines(updated_contents, file = target_path, sep = "\n")
  cat(updated_contents, file = target_path, sep = "\n")
  
}


# file_number_replace <- function(source_path, target_path, asghar, akbar) {
#   file_contents <- readLines(source_path)
#   asghar<-1
#   updated_contents <- gsub(x = file_contents, pattern = as.character(asghar), replacement = as.character(akbar))
#   updated_contents <- gsub(x = file_contents, pattern = as.character(asghar-1), replacement = as.character(akbar-1))
#   updated_contents <- gsub(x = file_contents, pattern = as.character(asghar-2), replacement = as.character(akbar-2))
#   
#   # write_lines(updated_contents, file = target_path, sep = "\n")
#   cat(updated_contents, file = target_path, sep = "\n")
# }


corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc, xtable)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }    ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }    ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format

pbptest <-function(x, ...) {
  ## residual heteroskedasticity test based on the residuals of the demeaned
  ## model and the regular bptest() in {lmtest}
  
  ## structure:
  ## 1: take demeaned data from 'plm' object
  ## 2: est. auxiliary model by OLS on demeaned data
  ## 3: apply bptest() to auxiliary model and return the result
  
  if (!inherits(x, "plm")) stop("need to supply a panelmodel estimated with plm()")
  model <- plm:::describe(x, "model")
  effect <- plm:::describe(x, "effect")
  theta <- x$ercomp$theta
  
  ## retrieve demeaned data
  demX <- model.matrix(x, model = model, effect = effect, theta = theta)
  demy <- pmodel.response(model.frame(x), model = model, effect = effect, theta = theta)
  
  Ti <- pdim(x)$Tint$Ti
  
  if (is.null(order)) order <- min(Ti)
  
  ## bgtest on the demeaned model:
  
  ## check package availability and load if necessary
  lm.ok <- require("lmtest")
  if(!lm.ok) stop("package lmtest is needed but not available")
  
  ## pbptest is the bptest, exception made for the method attribute
  dots <- match.call(expand.dots=FALSE)[["..."]]
  if (!is.null(dots$type)) type <- dots$type else type <- "Chisq"
  if (!is.null(dots$order.by)) order.by <- dots$order.by else order.by <- NULL
  
  auxformula <- demy~demX-1
  lm.mod <- lm(auxformula)
  return(lmtest::bptest(lm.mod, ...)) # call and return lmtest::bptest
} # END pbptest()

medianWithoutNA<-function(x) {median(x[which(!is.na(x))])}

dum <- function(kw, col, type=c(T, F)) {
  t <- as.data.frame(grep(as.character(kw), col, ignore.case=T))
  t$one <- type[1]
  colnames(t) <- c("col1","dummy") 
  t2 <- as.data.frame(grep(as.character(kw), col, ignore.case=T,
                           invert=T))
  t2$zero <- type[2]
  colnames(t2) <- c("col1","dummy")
  t3<-rbind(t, t2)
  t3<-t3[order(t3$col1), ]
  return(t3$dummy)
}


extract.pglm <- function (model, include.nobs = TRUE, include.loglik = TRUE, ...) {
  s <- summary(model, ...)
  coefficient.names <- rownames(s$estimate)
  coefficients <- s$estimate[, 1]
  standard.errors <- s$estimate[, 2]
  significance <- s$estimate[, 4]
  loglik.value <- s$loglik
  n <- nrow(model$model)
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglik == TRUE) {
    gof <- c(gof, loglik.value)
    gof.names <- c(gof.names, "Log-Likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.nobs == TRUE) {
    gof <- c(gof, n)
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  tr <- createTexreg(coef.names = coefficient.names, coef = coefficients, 
                     se = standard.errors, pvalues = significance, gof.names = gof.names, 
                     gof = gof, gof.decimal = gof.decimal)
  return(tr)
}


