# Generation of two groups that will be compared with both tests
# Group 1 is drawn from a normal distribution with size 'samp' and mean 'es'
# Group 2 is drawn from a multivariate normal distribution with 'samp' variables and mean 0
# The covariance matrix for group 2 dependes on the value of 'corr'
MyDataGeneration <- function(samp,es,corr){
  var1 <- rnorm(samp,0+es,1)
  if (corr>1){
    A=genPositiveDefMat(samp,covMethod = 'onion',rangeVar = c(1,1))
    covMatrix=A$Sigma
  } else {
  covMatrix=diag(rep(1,samp))
  for (i in 1:samp){
    for (j in 1:samp){
      if (i!=j){
        covMatrix[i,j]=corr
      }
    }
  }
}
  var2 <- mvrnorm(1,c(rep(0,samp)),covMatrix)
  Y <- c(var1, var2)
  group <- as.factor(c(rep(1, samp), rep(2,samp)))
  dat <- data.frame(Y,group)
  return(dat) 
}