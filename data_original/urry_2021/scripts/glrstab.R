#function by lgluca, https://github.com/crsh/papaja/issues/210
# modified by Heather Urry not to note significance for p < .10 and to add 95% CI

glrstab<- function(x, export=FALSE) {
  
  r <-corr.test(x)$r	#taking just the correlation matrix; no N, or p
  p <-corr.test(x)$p	#taking the p*s
  
  #define notions for significance levels
  mystars <- ifelse(p < .001, "**"
                    , ifelse(p < .01, "**"
                             , ifelse(p < .05, "*", " "))) 
                                     # , ifelse(p < .10, "+", " "))))
  
  #round r, define new matrix Rnew with the correlations from rnd and paste mystars
  rnd  <- papaja::printnum(r, gt1 = FALSE, digits = 2)  #round, drop leading 0 - Thanks CRSH!								                     
  Rnew <- matrix(paste(rnd, mystars, sep=""), ncol=ncol(rnd)) 
  
  #remove 1.0 correlations from diagonal  and set the strings
  diag(Rnew) <- ''		
  Rnew[upper.tri(Rnew)] <- ''								                	
  
  rownames(Rnew) <- paste(1:ncol(rnd), colnames(rnd), sep=" ")         #define number and name
  colnames(Rnew) <- paste(1:ncol(rnd), "", sep="") 			       #define number
  
  #fun-part: we trim the top half 
  Rnew[upper.tri(Rnew)] <- ''			
  Rnew
  
  # edited by HLU: get CI (relies on descr function in R package userfriendlyscience)
  CI <- as.character(1:length(x))
  for (i in 1:length(x)){
    CI[i] <- gsub(";", ",", descr(as.numeric(x[ , i]))$`central tendency`$`95% CI mean`)
  }
  
  Rnew <- cbind(round(data.frame(describe(x))[,3:4],2), CI, Rnew)		     #describe x, M sD, Ci - put them in the matrix
  colnames(Rnew)[1:3] <- c("M","SD","95% CI")					      		#Beschriftung der neuen Spalten
  Rnew <- Rnew[,1:(ncol(Rnew)-1)]							        	#delete the last column (ugly)
  
  #export to clipboard
  
  if (export==TRUE){
    result<-write.table(Rnew
                        , "clipboard"
                        , sep=";"
                        , row.names=FALSE)
  }
  else result <- Rnew
  return(result)
  
}