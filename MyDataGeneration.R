# Generation of two groups that will be compared with both tests
# Group 1 is drawn from a normal distribution with size 'samp' and mean 'es'
# Group 2 is drawn from a multivariate normal distribution with 'samp' variables and mean 0
# The covariance matrix for group 2 dependes on the value of 'corr'
MyDataGeneration <- function(samp,es,corr,ex){
  if (ex == 'exmet'){
    covMatrix=diag(rep(1,samp*2))
    for (i in 1:(samp*2)){
      for (j in 1:(samp*2)){
        if (i!=j){
          covMatrix[i,j]=corr
        }
      }
    }
    var1var2 <- mvrnorm(1,c(rep(0,samp*2)),covMatrix)
    var1 <- var1var2[1:samp]
    var2 <- var1var2[(samp+1):(samp*2)]+es
    } else if (ex == 'inex1'){
        A=genPositiveDefMat(samp*2,covMethod = 'onion',rangeVar = c(1,1))
        covMatrix=A$Sigma
        var1var2 <- mvrnorm(1,c(rep(0,samp*2)),covMatrix)
        var1 <- var1var2[1:samp]
        var2 <- var1var2[(samp+1):(samp*2)]+es
      } else if (ex == 'inex2'){
        covMatrix=diag(rep(1,samp))
        for (i in 1:samp){
          for (j in 1:samp){
            if (i!=j){
              covMatrix[i,j]=corr
            }
          }
        }
        var1 <- mvrnorm(1,c(rep(0,samp)),covMatrix)
        var2 <- mvrnorm(1,c(rep(es,samp)),covMatrix)
      }
  Y <- c(var1, var2)
  group <- as.factor(c(rep(1, samp), rep(2,samp)))
  dat <- data.frame(Y,group)
  return(dat) 
}
