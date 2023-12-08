#' @import boot
#' @import bootstrap
#' @import twosamples
#' @import stats4
#' @import microbenchmark
#' @import Rcpp
#' @importFrom stats rbinom
#' @useDynLib SA23204186
NULL




#' @title generate SBM model 
#' @description generate a SBM model with some settings
#' @param B the probability matrix (double)
#' @param p the probability of the node belonging to each clusters (numeric)
#' @param n the number of node (numeric)
#' @return A the adjacency matrix of the model
#' @examples
#' \dontrun{
#' C<-generater_model()
#' }
#' @export
generater_model<-function(B=matrix(c(.2,.3,.3,.1),nrow = 2),p=c(0.3,0.7),n=1000){
  if(nrow(B)!=ncol(B))
    return("ERROR")
  m<-nrow(B)
  for (i in 1:m) {
    for (j in 1:m) {
      if(B[i,j]!=B[j,i])
        return("ERROR")
      if(B[i,j]<0)
        return("ERROR")
    }
    
  }
  g<- sample(1:m, size = n, replace = TRUE, prob = p)
  A<-matrix(0,ncol = n,nrow = n)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      A[i,j]<-rbinom(1,1,B[g[i],g[j]])
      A[j,i]<-A[i,j]
    }
  }
  return(A)
}


#' @title Adjacency spectral embedding and Laplacian embedding
#' @description The function can get the spectral of the adjacency matrix and laplacian matrix
#' @param A the adjacency matrix (double)
#' @param d the dimension of the embedding space (numeric)
#' @param lapace whether to obtain laplacian matrix or not (logical)
#' @return the d dimension spectral embedding of the data
#' @examples
#' \dontrun{
#' C<-matrix(c(0,1,1,1,0,0,1,0,0),nrow = 3)
#' ASE(C,2,lapace = TRUE)
#' }
#' @export
ASE <- function(A,d,lapace="FALSE") {
if(nrow(A)!=ncol(A))
  return("ERROR")
n=nrow(A)
for (i in 1:n) {
  for (j in 1:n) {
    if(A[i,j]!=A[j,i])
      return("ERROR")
    if(A[i,j]<0)
      return("ERROR")
  }
  
}
for (i in 1:n) {
  A[i,i]<-0
}

if(lapace=="TRUE"){
  D<-matrix(0,nrow = n,ncol = n)
  for (i in 1:n) {
    D[i,i]=1/sqrt(sum(A[,i]))
  }
  A<-D%*%A%*%D
}

E<-eigen(A)
s<-E$values
v<-E$vectors
v1<-matrix(0,nrow = n,ncol = d)
for (i in 1:d) {
  v1[,i]<-v[,i]*sqrt(abs(s[i]))
}
return(v1)
}




