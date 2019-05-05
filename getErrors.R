# Providing the type I error rates/power of each condition
# Additionally, McNemar's test with it corresponding p-value and significance is added
library(dplyr)
groups <- group_by_at(ResultsSimAll, names(Design))
pVal <- function(Method1,Method2){
  return(mcnemar.test(factor(Method1,levels=c(0,1)),factor(Method2,levels=c(0,1)))$p.value)
}
results <- as.data.frame(summarise(groups, mean(Method1), mean(Method2),pVal(Method1,Method2)))
results$typeI <- results$es==0
results$`pVal(Method1, Method2)` <- results$`pVal(Method1, Method2)`* nrow(results)
results$`pVal(Method1, Method2)`[results$`mean(Method1)`==results$`mean(Method2)`] <- 1
results$`pVal(Method1, Method2)`[results$`pVal(Method1, Method2)`>1] <- 1
results$sig <- rep(FALSE,nrow(results))
results$sig[results$`pVal(Method1, Method2)`<0.05] <- TRUE
results <- merge(Design,results,by=names(Design),sort=FALSE)
results <- merge(results,timeResults,by=names(Design),sort=FALSE)
totalTime <- sum(results$timeNeeded)