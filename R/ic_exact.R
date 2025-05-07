#' @title One-Shot Iman–Conover Transformation
#' @name ic_exact
#' @param x A numeric matrix or data.frame.
#' @param target_r Target rank correlation matrix (optional).
#' @importFrom stats cor qnorm
ic_exact<-function(x,target_r=NULL){x<-as.matrix(x);if(is.null(target_r))target_r<-cor(x,method='spearman');n<-nrow(x);S<-apply(x,2,function(col)qnorm(rank(col,ties.method='average')/(n+1)));Y<-S%*%solve(t(chol(cor(S))),t(chol(target_r)));out<-x;for(j in seq_len(ncol(x)))out[,j]<-sort(x[,j])[rank(Y[,j],ties.method='first')];attr(out,'err')<-max(abs(cor(out,method='spearman')[upper.tri(target_r)]-target_r[upper.tri(target_r)]));out}
