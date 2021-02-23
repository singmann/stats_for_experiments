
#####################
# Functions to compute Bayes factors with replications
# Belonging to ReplicationBayesfactors.R
# First full function, then separate supporting functions
# Additional function to compute the posterior distribution for the JZS prior
# which can be plotted together with the replication prior. 
#####################

BFSALL <- function(
  tobs,                  # t value in first experiment
  trep,                  # t value in replicated experiment
  n1,                    # first experiment: n in group 1 or total n  
  n2,                    # second experiment: n in group 1 or total n 
  m1       = 1,          # first experiment: n in group 2 or total n
  m2       = 1,          # second experiment: n in group 2 or total n
  sample   = 1,          # 1 = one sample t-test (or within), 2 = two sample t-test  
  Type = 'JZS2Z'         # REPBF is Replication BF10
)
{
  nrep <- length(trep) 
  name <- matrix(0,1,nrep)
  for ( i in 1:nrep){
    name[i]<-paste("Replication",i)
  }
  
  if(Type == 'JZS2Z' ){  
    A <-  RegBayesfactorsREP(tobs,trep,n1,n2, m1, m2, sample=sample)
    OUT <- matrix(A[1,],1,)
    colnames(OUT) <- c("Original", name)
    rownames(OUT) <- c("JZS Bayes Factor 10")
  }
  
  if(Type == "JZS1U" ){  
    A <-  RegBayesfactorsREP(tobs,trep,n1,n2, m1, m2, sample=sample)
    OUT <- matrix(A[2,],1,)
    colnames(OUT) <- c("Original", name)
    rownames(OUT) <- c("JZS 1-sided Upper 10")
  }
  
  if(Type == "JZS1L" ){  
    A <-  RegBayesfactorsREP(tobs,trep,n1,n2, m1, m2, sample=sample)
    OUT <- matrix(A[3,],1,)
    colnames(OUT) <- c("Original", name)
    rownames(OUT) <- c("JZS 1-sided Lower 10")
  }
  
  if(Type == "REPBF" ){  
    B <- trep
    for ( i in 1:nrep) {
      B[i] <- ReplicationBayesfactorNCT(tobs, trep[i], n1, n2[i], m1, m2[i], sample =sample, plot=0)$BF
    }
    OUT <- matrix(B[1:nrep],1,)
    colnames(OUT) <- name[1:nrep]
    rownames(OUT) <- c("Replication BF 10")
  }
  
  
  if(Type == "EFFBF" ){  
    C <- trep
    for ( i in 1:nrep) {
      C[i] <- BayMayBF(tobs, trep[i], n1, n2[i], m1, m2[i], sample =sample)
    }
    OUT <- matrix(C[1:nrep],1,)
    colnames(OUT) <- name[1:nrep]
    rownames(OUT) <- c("Effect Size BF 01")
  }
  
  if(Type == "METBF" ){  
    D <- BFmeta(tobs,trep,nrep,n1,n2, m1, m2, sample=sample)
    OUT <- matrix(D,1,)
    rownames(OUT) <- c("Meta-analysis BF 10")
  }
  
  
  if(Type == "SINGLE" ){  
    A <-  RegBayesfactorsREP(tobs,trep,n1,n2, m1, m2, sample=sample)
    OUT <- A[1:3,]
    rownames(OUT) <- c("JZS Bayes Factor","JZS 1-sided Upper","JZS 1-sided Lower")
  }
  
  
  if(Type == "ALL" ){  
    A <-  RegBayesfactorsREP(tobs,trep,n1,n2, m1, m2, sample=sample)
    
    nrep <- length(trep) 
    B <- trep
    for ( i in 1:nrep) {
      B[i] <- ReplicationBayesfactorNCT(tobs, trep[i], n1, n2[i], m1, m2[i], sample =sample, plot=0)$BF
    }
    C <- trep
    for ( i in 1:nrep) {
      C[i] <- BayMayBF(tobs, trep[i], n1, n2[i], m1, m2[i], sample =sample)
    }
    D <- BFmeta(tobs,trep,nrep,n1,n2, m1, m2, sample=sample)
    OUT <- rbind(A[1:3,],c(NA,B),c(NA,C), c(D,rep(NA,nrep)  ))
    rownames(OUT) <- c("JZS Bayes Factor","JZS 1-sided Upper","JZS 1-sided Lower","Replication BF","Effect Size BF","Meta-analysis BF")
  }
  
  return(OUT)
}


effectsizes <- function(
  tobs,                  # t value in first experiment
  trep,                  # t value in replicated experiment
  n1,                    # first experiment: n in group 1 or total n  
  n2,                    # second experiment: n in group 1 or total n 
  m1       = 1,          # first experiment: n in group 2 or total n
  m2       = 1,          # second experiment: n in group 2 or total n
  sample   = 1        # 1 = one sample t-test (or within), 2 = two sample t-test  
)
{
  
  if (sample==1) { 
    sqrt.n  <-  sqrt(n1)
    df <- n1 -1 }
  
  if (sample==2) {
    sqrt.n <- sqrt( 1/(1/n1+1/m1)) # two sample alternative for sqrt(n)
    df <- n1 + m1 -2   #degrees of freedom
  }
  
  delta <- c(tobs,trep)/sqrt.n
  
  return(delta)
}



BFsensitivity <- function(
  t,  # value in replicated study
  n,  # n in replication study group 1 
  m = 1,  # n in replication study group 2
  sample = 1,  # 1 = one sample t-test (or within), 2 = two sample t-test  
  r = 1,  # scale for the cauchy prior  
  s = 1,  # scale for the normal prior
  type = "ALL"
)
{
  ###################
  #Generates Bayes Factors from t values and n for one and two sample t-tests
  #Bayes factors included are: 
  #JZS (adapted from Ruud Wetzels)
  #Unit information (adapted from Ruud Wetzels)
  #Crystal Ball maximum information
  #Crystal Ball maximum variance
  ###################
  
  library(MCMCpack)
  
  # Define functions (Adapted from Ruud Wetzels): 
  # Numerator Bayes factor component (H0) 
  bf0 <- function(t, nu) {
    (1+t^2/(nu))^(-(nu+1)/2)
  }
  # Denominator Bayes factor component Unit information prior
  bf1a <- function(g, t, n, nu){
    (1+n*g)^(-.5)*(1+t^2/((1+n*g)*(nu)))^(-(nu+1)/2)
  }
  # Construction Bayes factor JZS: 
  # Inverse gamma prior 
  prior <- function(g, n){
    dinvgamma(g, .5, n/2)
  }
  # Prior times likelihood
  joint <- function(g, t, n, nu, r2){
    prior(g, 1)*bf1a((g*r2), t, n, nu)
  }
  # Denominator Bayes factor component JZS prior
  bf1 <-  function(t, n, nu, r2){
    integrate(joint, lower=0, upper=Inf, t=t, n=n, nu=nu, r2=r2)
  }
  # Bayes Factor JZS: 
  bf <- function(t, n, nu, r2){
    bf0(t, nu)/ bf1(t, n, nu, r2)$value
  }
  # Bayes factor unit information prior: 
  unit <- function(t, n, nu, r){
    bf0(t, nu)/ bf1a(r, t, n, nu)
  }
  
  if (sample == 2) {
    N <- n*m/(n+m)
    df.rep <- n+m-2  
  }
  if (sample == 1) {
    N <- n
    df.rep <- n-1  
  }
  nu <- df.rep
  
  # Compute Unit prior Bayes Factor 10
  UnitBF <- 1/unit(t, N, nu, s)
  # Compute JZS prior Bayes Factor 10
  JZSBF <- 1/bf(t, N, nu, r)
  
  # Edwards, Savage, Lindley (1963) higher bound Bayes factor 10
  MaxBF <- dt(0, nu)/dt(t, nu)
  
  # Normal prior with variance leading to maximum BF, Bayes factor 10
  sds = seq(0, 3, length = 100)
  BF_seq = rep(NA, length(sds))
  for(i in 2:length(sds)){
    BF_seq[i] = unit(t, N, nu, (sds[i]^2))
  }    
  MaxvarBF <- 1/min(BF_seq[2:100])
  
  if ( type == "Normal"){
    return(UnitBF )
  }
  if ( type == "JZS"){
    return(JZSBF)
  }
  if ( type == "ALL"){
    return(list(JZSBF, UnitBF, MaxBF, MaxvarBF))
  }
  
  
}


# 1) Default priors: test H0 (effect is zero) versus H1 (effect is nonzero). 

## Computation for original and replicated studies:  


RegBayesfactorsREP <- function(
  tobs,                  # t value in first experiment
  trep,                  # t value (s) in replicated experiment
  n1,                    # first experiment: n in group 1 or total n  
  n2,                    # replicated experiment(s): n in group 1 or total n 
  m1       = 1,          # first experiment: n in group 2 
  m2       = 1,          # replicated experiment(s): n in group 2 
  sample   = 1        # 1 = one sample t-test (or within), 2 = two sample t-test
)	
{
  library(MCMCpack)
  library(BayesFactor)
  
  nrep  <- length(trep) 
  
  if (sample ==1) {BFSO <-DefaultBF(tobs, n1)  }
  if (sample ==2) {BFSO <-DefaultBF(tobs, n1, m1, sample = 2)  }  
  
  BFSR <- matrix(0,6,nrep)
  name <- matrix(0,1,nrep)
  for ( i in 1:nrep){
    if (sample ==1) {BFSR[,i] <-DefaultBF(trep[i], n2[i])  }
    if (sample ==2) {BFSR[,i] <-DefaultBF(trep[i], n2[i], m2[i], sample = 2)  }  
    name[i]<-paste("Replication",i)
  }
  
  BFSout <- cbind(BFSO,BFSR)
  rownames(BFSout) <- c("JZS Bayes Factor","JZS 1-sided Upper","JZS 1-sided Lower","Unit information BF","Maximum BF (variance)","Maximum BF")
  colnames(BFSout) <- c("Original", name)
  
  return(BFSout)
}

###PLot   
bf1s = function(t,n1, m1=0) { 
  res1 <- ttest.tstat(t, n1, m1, nullInterval = c(0,Inf) , rscale = 1)
  res2 <- ttest.tstat(t, n1, m1, nullInterval = c(-Inf,0) , rscale = 1)
  return( c(exp(res1[['bf']][1]),exp(res2[['bf']][1])) )
} 

PlotJZS <- function(t, # t value 
                    n1, # number of persons in first group
                    m1 = 0, # number of subjects in second group
                    sample = 1, # 1= one sample, 2 = two samples  
                    WB ="c:/Program Files/WinBUGS14", #WinBUGS location
                    iters = 50000, #Iterations in WinBUGS 
                    side = 0, #1 = One-sided Lower, 2= one-sided Higher
                    yhigh = 0 # If you need comparable plots of the same height
)
{
  
  
  ff <- posteriorJZS(t, n1, m2=m1,sample=sample, WB=WB, iters=iters) 
  JZSpost.mean <- ff$mean 
  JZSpost.sd <- ff$sd
  
  if ( side == 0) {
    
    if(yhigh == 0) {
      high <- dnorm(JZSpost.mean, JZSpost.mean, JZSpost.sd )
      add <- (dnorm(JZSpost.mean, JZSpost.mean, JZSpost.sd )/5)
      yhigh <- high + add
    }
    
    scale <- 3
    min.x <-  min(JZSpost.mean-scale*JZSpost.sd,0) 
    max.x <-  max(JZSpost.mean+scale*JZSpost.sd,0) 
    
    par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
        font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
    
    plot ( function( x ) dnorm( x, JZSpost.mean, JZSpost.sd ), min.x, max.x, 
           ylim=c(0,yhigh), xlim=c(min.x,max.x), lwd=2, lty=1, col= 1,
           ylab="Density", xlab=" ") 
    par(new=T)
    plot ( function( x ) dcauchy(x), ylim=c(0,yhigh), xlim=c(min.x,max.x), lwd=2, lty=3, col=1,
           ylab="Density", xlab=" ", main= "JZS BF") 
    axis(1)
    axis(2)
    mtext(expression(Effect ~ size ~ delta), side=1, line = 2.8, cex=2)
    
    #  par(new=T)
    #  plot(rep(0,5),c(0,1,2,3,4) ,type='l',ylim=c(0,yhigh), xlim=c(min.x,max.x),ylab = " ", xlab = " ")
    
    # Plot heights at delta = zero for Savage-Dickey density ratio:
    points(0, dnorm(0,JZSpost.mean,JZSpost.sd )  , pch=21, cex=2, bg="grey")
    points(0, dcauchy(0) , pch=21, cex=2, bg="grey")
    
    legend(min.x, yhigh + .5 * add, c("Prior","Posterior"), lwd= 2, lty=c(3,1) ,bty = 'n' , cex= 1.5)  
    
    # plot BF in graph:
    #text(JZSpost.mean+.05, dnorm(JZSpost.mean,JZSpost.mean,JZSpost.sd)+.2, labels = substitute(paste(BFJZS[r0], " = ", v), list(v=round(JZSBF, digits=2))), cex = 1.5, pos=4)
    par(new = F)
    BF <- dcauchy(0)/dnorm(0,JZSpost.mean,JZSpost.sd )
  }
  
  if ( side == 1) {
    
    #side lower:  
    scale <- 3
    min.x <-  min( -.1,JZSpost.mean-scale*JZSpost.sd)
    
    #Correct the posterior for cutting off at zero
    leftarea <- pnorm(0,JZSpost.mean,JZSpost.sd) 
    
    max.y = yhigh
    add <- yhigh/5
    
    if(yhigh == 0) {
      high <- max(dnorm(seq(-10,0, by = .1),JZSpost.mean,JZSpost.sd)*(1/leftarea) ) 
      add <- high/5
      max.y <- high + add
    }
    
    
    par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
        font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
    
    plot(function(c) dnorm(c,JZSpost.mean,JZSpost.sd)*(1/leftarea),min.x,0,lwd=2,lty=1, xlim=c(min.x,0),ylim=c(0,max.y),ylab='Density',
         xlab= " ", main= "JZS BF one-sided: Lower")
    plot(function(c) dcauchy(c,0,1)*2,-5,0,add=T,lwd=2,lty=3)    
    axis(1)
    axis(2)
    mtext(expression(Effect ~ size ~ delta), side=1, line = 2.8, cex=2)
    
    # Lines at zero to the ordinate
    lines(c(0,0),c(0,dnorm(0,JZSpost.mean,JZSpost.sd) *(1/leftarea)),lwd=2,lty=1) 
    lines(c(0,0),c(0,dcauchy(0)*2),lwd=2,lty=3)
    
    # Density at zero for prior (* 2 because symmetric around 0) and posterior
    points(0, dcauchy(0)*2 , pch=21, cex=2, bg="grey")
    points(0,dnorm(0,JZSpost.mean,JZSpost.sd)*(1/leftarea), pch=21, cex=2, bg="grey")
    
    legend(min.x, max.y +.5*add, c("Prior","Posterior"), lwd= 2, lty=c(3,1) ,bty = 'n' , cex= 1.5)  
    BF <- (dcauchy(0)*2)/dnorm(0,JZSpost.mean,JZSpost.sd)*(1/leftarea)
  }
  
  if ( side == 2) {
    
    #side upper:  
    scale <- 3
    max.x <-  max(JZSpost.mean+scale*JZSpost.sd, .1) 
    
    rightarea <- 1-pnorm(0,JZSpost.mean,JZSpost.sd) 
    
    max.y = yhigh
    add <- yhigh/5
    
    if(yhigh == 0) {
      high <- max(dnorm(seq(0,10, by = .01),JZSpost.mean,JZSpost.sd)*(1/rightarea) ) 
      add <- high/5
      max.y <- high + add
    }
    
    par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
        font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
    
    plot(function(c) dnorm(c,JZSpost.mean,JZSpost.sd)*(1/rightarea),0,max.x,lwd=2,lty=1,
         xlim=c(0,max.x),ylim=c(0,max.y),ylab='Density',
         xlab=" ", main= "JZS BF one-sided: Upper")
    axis(1)
    axis(2)
    mtext(expression(Effect ~ size ~ delta), side=1, line = 2.8, cex=2)
    
    plot(function(c) dcauchy(c,0,1)*2,0,max.x,add=T,lwd=2,lty=3)    
    
    # Lines at zero to the ordinate
    lines(c(0,0),c(0,dnorm(0,JZSpost.mean,JZSpost.sd)*(1/rightarea)),lwd=2,lty=1)
    lines(c(0,0),c(0,dcauchy(0)*2),lwd=2,lty=3)
    
    # Density at zero for prior (* 2 because symmetric around 0) and posterior
    points(0, dcauchy(0)*2 , pch=21, cex=2, bg="grey") 
    points(0,dnorm(0,JZSpost.mean,JZSpost.sd)*(1/rightarea), pch=21, cex=2, bg="grey")
    
    legend(0 , max.y+.5*add, c("Prior","Posterior"), lwd= 2, lty=c(3,1) ,bty = 'n' , cex= 1.5)  
    BF <- (dcauchy(0)*2)/dnorm(0,JZSpost.mean,JZSpost.sd)*(1/rightarea)  
  } 
  names(BF) <- "Savage Dickey JZS Bayes factor" 
  return(BF)
}




PlotJZSspline <- function(t, # t value 
                          n1, # number of persons in first group
                          m1 = 0, # number of subjects in second group
                          sample = 1, # 1= one sample, 2 = two samples  
                          WB ="c:/Program Files/WinBUGS14", #WinBUGS location
                          iters = 10000, #Iterations in WinBUGS 
                          side = 0, #1 = One-sided Lower, 2= one-sided Higher
                          yhigh = 0 # If you need comparable plots of the same height
)
{
  #But this is two-sided 
  
  ff <- posteriorJZS(t, n1, m1 , sample,WB,iters) 
  JZSpost.mean <- ff$mean 
  JZSpost.sd <- ff$sd
  
  
  if ( side == 0) {
    
    if(yhigh == 0) {
      yhigh <- dnorm(JZSpost.mean, JZSpost.mean, JZSpost.sd ) +.5
    }
    
    scale <- 3
    min.x <-  JZSpost.mean-scale*JZSpost.sd 
    max.x <-  JZSpost.mean+scale*JZSpost.sd 
    
    plot ( function( x ) dnorm( x, JZSpost.mean, JZSpost.sd ), min.x, max.x, 
           ylim=c(0,yhigh), xlim=c(min.x,max.x), lwd=2, lty=1, col= 1,
           ylab="Density", xlab=" ") 
    par(new=T)
    plot ( function( x ) dcauchy(x), ylim=c(0,yhigh), xlim=c(min.x,max.x), lwd=2, lty=3, col=1,
           ylab="Density", xlab=" ") 
    #  par(new=T)
    #  plot(rep(0,5),c(0,1,2,3,4) ,type='l',ylim=c(0,yhigh), xlim=c(min.x,max.x),ylab = " ", xlab = " ")
    
    # Plot heights at delta = zero for Savage-Dickey density ratio:
    points(0, dnorm(0,JZSpost.mean,JZSpost.sd )  , pch=21, cex=2, bg="grey")
    points(0, dcauchy(0) , pch=21, cex=2, bg="grey")
    
    legend(min.x,yhigh, c("Prior","Posterior"), lwd= 2, lty=c(3,1) ,bty = 'n' , cex= 1.5)  
    
    # plot BF in graph:
    #  text(JZSpost.mean+.05, dnorm(JZSpost.mean,JZSpost.mean,JZSpost.sd)+.2, labels = substitute(paste(BFJZS[r0], " = ", v), list(v=round(JZSBF, digits=2))), cex = 1.5, pos=4)
    par(new = F)
    BF <- dcauchy(0)/dnorm(0,JZSpost.mean,JZSpost.sd )
  }
  
  if ( side == 1) {
    
    d=density(ff$postchain)
    fpost =splinefun(d)
    fpost = dnorm(mean(ff$postchain),var(ff$postchain))  
    d0_post=fpost(0)
    
    #side lower:  
    scale <- 3
    min.x <-  JZSpost.mean-scale*JZSpost.sd 
    
    leftarea <- integrate(fpost,min(d$x),0)$value
    
    max.y <- max(spline(d)$y)*(1/leftarea) + .5
    
    #Correct the posterior for cutting off at zero
    plot(function(c) fpost(c)*(1/leftarea),min.x,0,lwd=2,lty=1, xlim=c(min.x,0),ylim=c(0,max(spline(d)$y)*(1/leftarea)),ylab='Density',
         xlab=expression(paste('effect size ',delta)), main= "One-sided: Lower")
    plot(function(c) dcauchy(c,0,1)*2,-5,0,add=T,lwd=2,lty=3)    
    
    # Lines at zero to the ordinate
    lines(c(0,0),c(0,fpost(0)*(1/leftarea)),lwd=2,lty=1)
    lines(c(0,0),c(0,dcauchy(0)*2),lwd=2,lty=3)
    
    # Density at zero for prior (* 2 because symmetric around 0) and posterior
    points(0, dcauchy(0)*2 , pch=21, cex=2, bg="grey")
    points(0,d0_post*(1/leftarea), pch=21, cex=2, bg="grey")
    
    legend(min.x, max.y, c("Prior","Posterior"), lwd= 2, lty=c(3,1) ,bty = 'n' , cex= 1.5)  
    BF <- (dcauchy(0)*2)/d0_post*(1/leftarea)
  }
  
  if ( side == 2) {
    
    d=density(ff$postchain)
    fpost =splinefun(d)
    d0_post=fpost(0)
    
    #side upper:  
    scale <- 3
    max.x <-  JZSpost.mean+scale*JZSpost.sd 
    rightarea <- integrate(fpost,0,max(d$x))$value
    
    plot(function(c) fpost(c)*(1/rightarea),0,max.x,lwd=2,lty=1,
         xlim=c(0,max.x),ylim=c(0,max(spline(d)$y)*(1/rightarea)),ylab='Density',
         xlab=expression(paste('effect size ',delta)), main= "One-sided: Upper")
    
    plot(function(c) dcauchy(c,0,1)*2,0,max.x,add=T,lwd=2,lty=3)    
    
    # Lines at zero to the ordinate
    lines(c(0,0),c(0,fpost(0)*(1/rightarea)),lwd=2,lty=1)
    lines(c(0,0),c(0,dcauchy(0)*2),lwd=2,lty=3)
    
    # Density at zero for prior (* 2 because symmetric around 0) and posterior
    points(0, dcauchy(0)*2 , pch=21, cex=2, bg="grey") 
    points(0,d0_post*(1/rightarea), pch=21, cex=2, bg="grey")
    
    legend(max.x- (max.x/2) , max(spline(d)$y), c("Prior","Posterior"), lwd= 2, lty=c(3,1) ,bty = 'n' , cex= 1.5)  
    
    BF <- (dcauchy(0)*2)/d0_post*(1/rightarea)  
    
  } 
  return(BF)
}



##Basic functions 

DefaultBF <- function(
  t,                  # t value 
  n1,                    # n in group 1 or total n  
  m1       = 1,          # n in group 2 
  sample   = 1        # 1 = one sample t-test (or within), 2 = two sample t-test  
)	
{
  library(MCMCpack)
  library(BayesFactor)
  
  if (sample==1) { 
    sqrt.n  <-  sqrt(n1)
    df <- n1 -1 }
  
  if (sample==2) {
    sqrt.n <- sqrt( 1/(1/n1+1/m1)) # two sample alternative for sqrt(n)
    df <- n1 + m1 -2   #degrees of freedom
  }
  
  ######## Definition of Functions  ###################################
  
  ## Probability of H0|Y for JZS and symmetric Normal prior BF  
  bf0 = function(t,nu) (1+t^2/(nu))^(-(nu+1)/2)
  ## Auxiliary functions
  prior=function(g,n) dinvgamma(g,.5,n/2)
  bf1a=function(g,t,n,nu) (1+n*g)^(-.5)*(1+t^2/((1+n*g)*(nu)))^(-(nu+1)/2)
  joint=function(g,t,n,nu,r2) prior(g,1)*bf1a((g*r2),t,n,nu)
  bf1= function(t,n,nu,r2)
    integrate(joint,lower=0,upper=Inf,t=t,n=n,nu=nu,r2=r2)
  ## Bayes Factor JZS
  bf=function(t,n,nu,r2) bf0(t,nu)/bf1(t,n,nu,r2)$value
  ## Bayes factor Unit information prior
  unit=function(t,n,nu,r)  bf0(t,nu)/bf1a(r,t,n,nu)
  ##one-sided Bayes factor JZS
  bf1s = function(t,n1, m1=0) { 
    res1 <- ttest.tstat(t, n1, m1, nullInterval = c(0,Inf) , rscale = 1)
    res2 <- ttest.tstat(t, n1, m1, nullInterval = c(-Inf,0) , rscale = 1)
    return( c(exp(res1[['bf']]),exp(res2[['bf']])) )
  } 
  
  ######## Calculation of BFs ########################################
  
  JZSBF <- 1/bf(t,(sqrt.n)^2,df,1)
  if ( sample== 1) {JZSBF1s  <- bf1s(t,n1)}
  if ( sample== 2) {JZSBF1s  <- bf1s(t,n1,m1)}
  UnitBF    <- 1/unit(t,(sqrt.n)^2,df,1)
  MaxBF     <- dt(0,df)/dt(t,df)
  
  sds = seq(0,3,length = 100)
  BF_seq = rep(NA,length(sds))
  for(i in 1:length(sds)){
    BF_seq[i] = unit(t,(sqrt.n)^2, df, sds[i]^2)
  }    
  
  MaxvarBF <- 1/min(BF_seq)
  BFs <- matrix(c(JZSBF,JZSBF1s,UnitBF,MaxvarBF,MaxBF),,1)
  rownames(BFs) <- c("JZSBF","JZSBF1sUpper","JZSBF1sLower","UnitBF","MaxvarBF","MaxBF")
  return(BFs)
}



##################################################################################
#Replication Bayes factor 
#################################################################################
#Change this function to only producing the new replication Bayes factor. 

ReplicationBayesfactorNCT <- function(
  tobs,                  # t value in first experiment
  trep,                  # t value in replicated experiment
  n1,                    # first experiment: n in group 1 or total n  
  n2,                    # second experiment: n in group 1 or total n 
  m1       = 1,          # first experiment: n in group 2 or total n
  m2       = 1,          # second experiment: n in group 2 or total n
  sample   = 1,        # 1 = one sample t-test (or within), 2 = two sample t-test	
  wod      = dir,	      # working directory 
  plot = 0,  # 0 = no plot 1 = replication 2 = replication + JZS  
  post = 0,  # 0 = no posterior, 1 = estimate posterior
  M = 500000, 
  yhigh = 0
)	
{
  library(MCMCpack)
  
  ##################################################################
  #STEP 1: compute the prior for delta based on the first experiment
  ################################### ###############################
  
  D    <- tobs 
  
  if (sample==1) {
    sqrt.n.orig  <-  sqrt(n1)
    df.orig <- n1 -1  
  }
  
  if (sample==2) {
    sqrt.n.orig  <- sqrt( 1/(1/n1+1/m1)) # two sample alternative for sqrt(n)
    df.orig <- n1 + m1 -2   #degrees of freedom
  }
  
  #To find out quickly the lower and upper bound areas, the .025 and .975 quantiles of tobs at a range of values for D are computed. To make the algorithm faster, only values in a reasonable area around D are computed.    
  # determination of area, larger with large D and small N
  range.D <- 4+ abs(D/(2*sqrt.n.orig)) 
  sequence.D <- seq(D,D + range.D,.01) #make sequence with range 
  # determine which D gives a quantile closest to tobs with an accuracy of .01
  options(warn=-1)
  approximatelow.D <- sequence.D[which( abs(qt(.025,df.orig,sequence.D)-tobs)==min(abs(qt(.025,df.orig,sequence.D)-tobs)) )]  
  options(warn=0)
  # Then a more accurate interval is computed within this area
  # Make sequence within .01 from value found before 
  sequenceappr.D <- seq((approximatelow.D-.01),(approximatelow.D+.01),.00001) 
  # determine which D gives a quantile closest to tobs with an accuracy of .00001
  low.D <- sequenceappr.D[which( abs(qt(.025,df.orig,sequenceappr.D)-tobs)==min(abs(qt(.025,df.orig,sequenceappr.D)-tobs)) )]  
  
  # Compute standard deviation for the corresponding normal distribution.
  sdlow.D <- (D-low.D)/qnorm(.025) 
  
  # compute prior mean and as for delta 
  prior.mudelta <- D/sqrt.n.orig
  prior.sdelta <-  sdlow.D/sqrt.n.orig
  
  ##################################################################
  #STEP 2: Compute Replication Bayes Factor
  ##################################################################
  # For one sample t-test: within (group2==1) or between (group2==vector)
  
  if (sample==1)
  {  
    df.rep <- n2-1 
    sqrt.n.rep <- sqrt(n2)
    Likelihood.Y.H0 <- dt(trep,df.rep)
    
    sample.prior <- rnorm(M,prior.mudelta,prior.sdelta)
    options(warn=-1)
    average.Likelihood.H1.delta <- mean(dt(trep,df.rep,sample.prior*sqrt.n.rep))
    options(warn=0)
    BF <- average.Likelihood.H1.delta/Likelihood.Y.H0
  }
  
  # For two sample t-test: 
  
  if (sample==2)  {
    df.rep <- n2 + m2 -2 
    sqrt.n.rep <- sqrt(1/(1/n2+1/m2))
    Likelihood.Y.H0 <- dt(trep,df.rep)
    
    sample.prior <- rnorm(M,prior.mudelta,prior.sdelta)
    options(warn=-1)
    average.Likelihood.H1.delta <- mean(dt(trep,df.rep,sample.prior*sqrt.n.rep))
    options(warn=0)
    BF <- average.Likelihood.H1.delta/Likelihood.Y.H0
  }
  
  #################################################################
  #STEP 3: Posterior distribution 
  #################################################################
  
  
  if( post == 1) { 
    
    options(warn=-1)
    likelihood <- dt(trep,df.rep,sample.prior*sqrt.n.rep)
    prior.density <- dnorm(sample.prior,prior.mudelta,prior.sdelta)
    likelihood.x.prior <- likelihood * prior.density
    
    LikelihoodXPrior <- function(x) {dnorm(x,prior.mudelta,prior.sdelta) * dt(trep,df.rep,x*sqrt.n.rep) }
    fact  <- integrate(LikelihoodXPrior,-Inf,Inf) 
    posterior.density <- likelihood.x.prior/fact$value
    PosteriorDensityFunction <- function(x) {(dnorm(x,prior.mudelta,prior.sdelta) * dt(trep,df.rep,x*sqrt.n.rep))/fact$value }
    options(warn=0)
    
    mean <- prior.mudelta
    sdh  <- prior.mudelta + .5*(max(sample.prior)[1] - prior.mudelta)
    sdl  <- prior.mudelta - .5*(prior.mudelta - min(sample.prior)[1])
    dev  <- 2
    
    
    for ( j in 1:10) {
      rangem <- seq((mean-dev)[1],(mean+dev)[1],dev/10)
      rangesdh <- seq((sdh-dev)[1],(sdh+dev)[1],dev/10)
      rangesdl <- seq((sdl-dev)[1],(sdl+dev)[1],dev/10)
      perc <- matrix(0,length(rangem),3)
      
      
      I<-min(length(rangem), length(rangesdh),length(rangesdl) )
      for ( i in 1:I) { 
        options(warn=-1)
        vpercm <-  integrate(PosteriorDensityFunction, -Inf,  rangem[i])
        perc[i,1]<- vpercm$value
        vpercsh <-  integrate(PosteriorDensityFunction, -Inf,  rangesdh[i])
        perc[i,2]<- vpercsh$value
        vpercsl <-  integrate(PosteriorDensityFunction, -Inf,  rangesdl[i])
        perc[i,3]<- vpercsl$value
        options(warn=0)    
      }
      mean <- rangem[which(abs(perc[,1]-.5)== min(abs(perc[,1]-.5)))]
      sdh <-  rangesdh[which(abs(perc[,2]-pnorm(1))== min(abs(perc[,2]-pnorm(1))))]
      sdl <-  rangesdl[which(abs(perc[,3]-pnorm(-1))== min(abs(perc[,3]-pnorm(-1))))]
      dev <- dev/10
    }
    
    posterior.mean <- mean
    posterior.sd <- mean(c(abs(sdh- mean),abs(sdl - mean)))
  }
  
  if (post != 1) {
    posterior.mean <- 0
    posterior.sd <- 0
  } 
  
  ###########OUT 
  
  dat.SD=new.env()
  dat.SD$BF      <- BF
  dat.SD$prior.mean= round(prior.mudelta,2)
  dat.SD$prior.sd= round(prior.sdelta,2)
  dat.SD$post.mean= round(posterior.mean,2)
  dat.SD$post.sd= round(posterior.sd,2)
  dat.SD=as.list(dat.SD)
  
  
  ###########################################  
  #PLOT
  ########################################### 
  if (plot==1)
  {
    
    if (post != 1) {
      options(warn =-1)
      rp <- ReplicationPosterior(trep,prior.mudelta,prior.sdelta,n2,m2=1,sample=sample)
      options(warn =0)
      
      posterior.mean <- rp[[1]] 
      posterior.sd  <- rp[[2]]
    }
    
    par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
        font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
    
    high <- dnorm(posterior.mean,posterior.mean,posterior.sd) 
    add <- high/5
    
    if(yhigh==0) { 
      yhigh <- high + add  
    }
    
    scale <- 3
    min.x <-  min((posterior.mean - scale*posterior.sd),(prior.mudelta - scale*prior.sdelta))
    max.x <-  max((posterior.mean + scale*posterior.sd),(prior.mudelta + scale*prior.sdelta))
    
    #Plot the posterior:
    plot ( function(x) dnorm(x, prior.mudelta, prior.sdelta ), min.x, max.x, ylim=c(0,yhigh), xlim=c(min.x,max.x), lwd=2, lty=3, 
           ylab="Density", xlab=" ") 
    par(new=T)
    
    #Plot the posterior:
    plot ( function(x) dnorm(x, posterior.mean, posterior.sd ), min.x, max.x, ylim=c(0,yhigh), xlim=c(min.x,max.x), lwd=2, lty=1, 
           ylab="Density", xlab=" ") 
    axis(1)
    axis(2)
    mtext(expression(Effect ~ size ~ delta), side=1, line = 2.8, cex=2)
    
    # Plot heights at delta = zero for Savage-Dickey density ratio:
    points(0, dnorm(0,prior.mudelta,prior.sdelta)  , pch=21, cex=2, bg="grey")
    points(0, dnorm(0,posterior.mean,posterior.sd) , pch=21, cex=2, bg="grey")
    
    # plot BF in graph:
    text(0, high + .5*add , labels = substitute(paste(BF[r0], " = ", v), list(v=round(BF, digits=2))), cex = 1.5, pos=4)
    #    title <-  paste ("tobs=", tobs, "trep=", trep , "nobs =", n1,"nrep =", n2 )
    
    #  dev.copy(device=jpeg, file= paste(dir,title,".jpg", sep=""))
    #  dev.off()
  }
  
  
  if (plot==2)
  {
    
    if (post != 1) {
      options(warn =-1)
      rp <- ReplicationPosterior(trep,prior.mudelta,prior.sdelta,n2,m2=1,sample=sample)
      options(warn =0)
      
      posterior.mean <- rp[[1]] 
      posterior.sd  <- rp[[2]]
    }
    
    pj <- posteriorJZS(trep,n2) 
    
    
    JZSpost.mean <- pj$mean
    JZSpost.sd <- pj$sd
    checkBF <- 1/pj$BF
    
    #step 7: plot both  
    
    ReplicationPlot(trep, tobs, n1, n2, posterior.mean, posterior.sd,
                    prior.mudelta, prior.sdelta, REPBF= BF,JZS = 1, 
                    JZSpost.mean = JZSpost.mean, JZSpost.sd = JZSpost.sd,
                    JZSBF = JZSBF, yhigh = yhigh)
    
    
  }
  
  return(dat.SD)
}

###############################

PosteriorDelta <- function(
  tobs,          # t value in first experiment
  n1,            # first experiment: n in group 1 or total n  
  m1       = 1,  # first experiment: n in group 2 or total n
  sample   = 1   # 1 = one sample t-test (or within), 2 = two sample t-test  
)
{
  D    <- tobs 
  if (sample==1) {
    sqrt.n.orig  <-  sqrt(n1)
    df.orig <- n1 -1  
  }
  if (sample==2) {
    sqrt.n.orig  <- sqrt( 1/(1/n1+1/m1)) # two sample alternative for sqrt(n)
    df.orig <- n1 + m1 -2   #degrees of freedom
  }
  
  #To find out quickly the lower and upper bound areas, the .025 and .975 quantiles of tobs at a range of values for D are computed. To make the algorithm faster, only values in a reasonable area around D are computed.    
  # determination of area, larger with large D and small N
  range.D <- 4+ abs(D/(2*sqrt.n.orig)) 
  sequence.D <- seq(D,D + range.D,.01) #make sequence with range 
  # determine which D gives a quantile closest to tobs with an accuracy of .01
  options(warn=-1)
  approximatelow.D <- sequence.D[which( abs(qt(.025,df.orig,sequence.D)-tobs)==min(abs(qt(.025,df.orig,sequence.D)-tobs)) )]  
  options(warn=0)
  
  # Then a more accurate interval is computed within this area
  # Make sequence within .01 from value found before 
  sequenceappr.D <- seq((approximatelow.D-.01),(approximatelow.D+.01),.00001) 
  # determine which D gives a quantile closest to tobs with an accuracy of .00001
  low.D <- sequenceappr.D[which( abs(qt(.025,df.orig,sequenceappr.D)-tobs)==min(abs(qt(.025,df.orig,sequenceappr.D)-tobs)) )]  
  
  # Compute standard deviation for the corresponding normal distribution.
  sdlow.D <- (D-low.D)/qnorm(.025) 
  
  # compute prior mean and as for delta 
  prior.mudelta <- D/sqrt.n.orig
  prior.sdelta <-  sdlow.D/sqrt.n.orig
  
  return(list(prior.mudelta, prior.sdelta))
}


# Compute Replication Bayes Factor
RepBF <- function(trep,sample=1,n2,m2=1,prior.mudelta,prior.sdelta,M=500000)
{
  if (sample==1)
  {  
    df.rep <- n2-1 
    sqrt.n.rep <- sqrt(n2)
    Likelihood.Y.H0 <- dt(trep,df.rep)
    
    sample.prior <- rnorm(M,prior.mudelta,prior.sdelta)
    options(warn=-1)
    average.Likelihood.H1.delta <- mean(dt(trep,df.rep,sample.prior*sqrt.n.rep))
    options(warn=0)
    BF <- average.Likelihood.H1.delta/Likelihood.Y.H0
  }
  
  # For two sample t-test: 
  
  if (sample==2)  {
    df.rep <- n2 + m2 -2 
    sqrt.n.rep <- sqrt(1/(1/n2+1/m2))
    Likelihood.Y.H0 <- dt(trep,df.rep)
    
    sample.prior <- rnorm(M,prior.mudelta,prior.sdelta)
    options(warn=-1)
    average.Likelihood.H1.delta <- mean(dt(trep,df.rep,sample.prior*sqrt.n.rep))
    options(warn=0)
    BF <- average.Likelihood.H1.delta/Likelihood.Y.H0
  }
  
  return(BF)
}

################################

BFttest <- function(
  trep,  # value in replicated study
  n2,  # n in replication study group 1 
  m2 = 1,  # n in replication study group 2
  sample = 1,  # 1 = one sample t-test (or within), 2 = two sample t-test  
  prior.mudelta = 1,  # mean of prior distribution (for BUJ prior)
  scale = 1  # scale for the BUJ prior
)
  
{
  
  ###################
  #Generates Bayes Factors from t values and n for one and two sample t-tests
  #Bayes factors included are: 
  #JZS (adapted from Ruud Wetzels)
  #Unit information (adapted from Ruud Wetzels)
  #BUJ include prior information
  #Crystal Ball maximum information
  #Crystal Ball maximum variance
  ###################
  
  library(MCMCpack)
  
  # Define functions (Adapted from Ruud Wetzels): 
  # Numerator Bayes factor component (H0) 
  bf0 <- function(t, nu) {
    (1+t^2/(nu))^(-(nu+1)/2)
  }
  # Denominator Bayes factor component Unit information prior
  bf1a <- function(g, t, n, nu){
    (1+n*g)^(-.5)*(1+t^2/((1+n*g)*(nu)))^(-(nu+1)/2)
  }
  # Construction Bayes factor JZS: 
  # Inverse gamma prior 
  prior <- function(g, n){
    dinvgamma(g, .5, n/2)
  }
  # Prior times likelihood
  joint <- function(g, t, n, nu, r2){
    prior(g, 1)*bf1a((g*r2), t, n, nu)
  }
  # Denominator Bayes factor component JZS prior
  bf1 <-  function(t, n, nu, r2){
    integrate(joint, lower=0, upper=Inf, t=t, n=n, nu=nu, r2=r2)
  }
  # Bayes Factor JZS: 
  bf <- function(t, n, nu, r2){
    bf0(t, nu)/ bf1(t, n, nu, r2)$value
  }
  # Bayes factor unit information prior: 
  unit <- function(t, n, nu, r){
    bf0(t, nu)/ bf1a(r, t, n, nu)
  }
  
  if (sample == 2) {
    N <- n2*m2/(n2+m2)
    df.rep <- n2+m2-2  
  }
  if (sample == 1) {
    N <- n2
    df.rep <- n2-1  
  }
  nu <- df.rep
  
  # Compute Unit prior Bayes Factor 10
  UnitBF <- 1/unit(trep, N, nu, 1)
  # Compute JZS prior Bayes Factor 10
  JZSBF <- 1/bf(trep, N, nu, 1)
  
  # Bem, Utts, Johnson (??) prior Bayes factor 10 
  # include posterior mean from the previous experiment as (scaled) sd for normal prior
  r = (scale*prior.mudelta)^2
  BUJBF <- 1/unit(trep, N, nu, r)
  
  # Edwards, Savage, Lindley (1963) higher bound Bayes factor 10
  MaxBF <- dt(0, nu)/dt(trep, nu)
  
  # Normal prior with variance leading to maximum BF, Bayes factor 10
  sds = seq(0, 3, length = 100)
  BF_seq = rep(NA, length(sds))
  for(i in 2:length(sds)){
    BF_seq[i] = unit(trep, N, nu, (sds[i]^2))
  }    
  MaxvarBF <- 1/min(BF_seq[2:100])
  
  return(list(JZSBF, UnitBF, BUJBF, MaxBF, MaxvarBF))
}

################




#############

ReplicationPosterior <- function(trep,prior.mudelta,prior.sdelta,n2,m2=1,sample=1,M = 500000)
  
{
  
  if (sample==1)
  {  
    df.rep <- n2-1 
    sqrt.n.rep <- sqrt(n2)
    sample.prior <- rnorm(M,prior.mudelta,prior.sdelta)
  }
  
  # For two sample t-test: 
  
  if (sample==2)  {
    df.rep <- n2 + m2 -2 
    sqrt.n.rep <- sqrt(1/(1/n2+1/m2))
    sample.prior <- rnorm(M,prior.mudelta,prior.sdelta)
  }
  
  
  likelihood <- dt(trep,df.rep,sample.prior*sqrt.n.rep)
  prior.density <- dnorm(sample.prior,prior.mudelta,prior.sdelta)
  likelihood.x.prior <- likelihood * prior.density
  
  options(warn=-1)
  LikelihoodXPrior <- function(x) {dnorm(x,prior.mudelta,prior.sdelta) * dt(trep,df.rep,x*sqrt.n.rep) }
  fact  <- integrate(LikelihoodXPrior,-Inf,Inf) 
  posterior.density <- likelihood.x.prior/fact$value
  PosteriorDensityFunction <- function(x) {(dnorm(x,prior.mudelta,prior.sdelta) * dt(trep,df.rep,x*sqrt.n.rep))/fact$value }
  options(warn=0)
  
  mean <- prior.mudelta
  sdh  <- prior.mudelta + .5*(max(sample.prior)[1] - prior.mudelta)
  sdl  <- prior.mudelta - .5*(prior.mudelta - min(sample.prior)[1])
  dev  <- 2
  
  
  for ( j in 1:10) {
    rangem <- seq((mean-dev)[1],(mean+dev)[1],dev/10)
    rangesdh <- seq((sdh-dev)[1],(sdh+dev)[1],dev/10)
    rangesdl <- seq((sdl-dev)[1],(sdl+dev)[1],dev/10)
    perc <- matrix(0,length(rangem),3)
    
    
    I<-min(length(rangem), length(rangesdh),length(rangesdl) )
    for ( i in 1:I) { 
      options(warn=-1)
      vpercm <-  integrate(PosteriorDensityFunction, -Inf,  rangem[i])
      perc[i,1]<- vpercm$value
      vpercsh <-  integrate(PosteriorDensityFunction, -Inf,  rangesdh[i])
      perc[i,2]<- vpercsh$value
      vpercsl <-  integrate(PosteriorDensityFunction, -Inf,  rangesdl[i])
      perc[i,3]<- vpercsl$value
      options(warn=0)
    }
    mean <- rangem[which(abs(perc[,1]-.5)== min(abs(perc[,1]-.5)))]
    sdh <-  rangesdh[which(abs(perc[,2]-pnorm(1))== min(abs(perc[,2]-pnorm(1))))]
    sdl <-  rangesdl[which(abs(perc[,3]-pnorm(-1))== min(abs(perc[,3]-pnorm(-1))))]
    dev <- dev/10
  }
  
  posterior.mean <- mean
  posterior.sd <- mean(c(abs(sdh- mean),abs(sdl - mean)))
  
  return(list(posterior.mean,posterior.sd))  
}

######################



#############################################
#cOMPUTE POSTERIOR DISTRIBUTION FOR JZS PRIOR
#############################################

posteriorJZS <- function(trep,n2,m2=1,sample=1, WB ="c:/Program Files/WinBUGS14", iters = 50000) {
  
  ### ONE sample 
  
  if( sample ==1){
    
    np2  <-  sqrt(n2)
    nugr <- n2 -1  
    s      <- 1
    mudif  <- trep * (s/sqrt(n2))
    group1 <- rnorm(n2, 0, 1)
    group1 <- (group1-mean(group1))/sqrt(var(group1))
    group1 <- group1 + mudif
    
    data=list('group1','n2')
    
    modelString=
      "
    model{delta~dnorm(0,lambdaDelta)
    lambdaDelta ~ dchisqr(1)
    sigma.~dnorm(0,sigmaL)
    sigmaL~dchisqr(1)
    sigma<-abs(sigma.)
    lambdaData<-pow(sigma,-2)
    mu<-delta*sigma
    for (i in 1:n2){group1[i] ~ dnorm(mu,lambdaData)}
    prdelt ~ dnorm(0,lambdaDelta)
    }
    "
    
    write(modelString, file='modelfile.bug')
    
    inits=function()
    {
      list(delta=rnorm(1,0,1),sigma.=runif(1,0,5))
    }
    parameters=c('delta','mu','sigma','prdelt')
  }
  
  ####TWO sample 
  
  if ( sample ==2){
    
    np2  <- sqrt( 1/(1/n2+1/m2)) # two sample alternative for sqrt(n)
    mudif  <- trep * (1/np2)
    group1 <- rnorm(n2,0,1)
    group1 <- (group1-mean(group1))/sqrt(var(group1))
    group2 <- rnorm(m2,0,1)
    group2 <- (group2-mean(group2))/sqrt(var(group2))
    group2 <- group2 - mudif
    
    
    data=list('group1','group2','n2','m2')
    
    modelString=
      "
    model{delta~dnorm(0,lambdaDelta)
    lambdaDelta ~ dchisqr(1)
    sigma.~dnorm(0,sigmaL)
    sigmaL~dchisqr(1)
    sigma<-abs(sigma.)
    var<-pow(sigma,2)
    mu~dnorm(0,muL)
    muL~dchisqr(1)
    lambdaData<- 1/var
    alpha<-delta*sigma	
    muData1<-mu+alpha*0.5	
    muData2<-mu-alpha*0.5	
    for (i in 1:n2){group1[i] ~ dnorm(muData1,lambdaData)}
    for (i in 1:m2){group2[i] ~ dnorm(muData2,lambdaData)}
    prdelt ~ dnorm(0,lambdaDelta)
    }
    "
    
    write(modelString, file='modelfile.bug')
    
    inits=function()
    {
      list(delta=rnorm(1,0,1),mu=rnorm(1,0,1),sigma.=runif(1,0,5))
    }
    parameters=c('delta','mu','sigma','alpha','prdelt')
  }
  
  #### run WinBugs
  
  chains <- 1
  #iters <- 50000
  burns <- iters/10
  thins <-1
  
  library(R2WinBUGS)
  
  
  OST = bugs(data, inits, parameters,  model.file ="modelfile.bug",
             n.chains=chains, n.iter=iters, n.burnin=burns, n.thin=thins,
             bugs.directory=WB,  codaPkg=F,debug=F)
  
  post.dat=OST$sims.array[,,1]
  m=mean(post.dat)
  sd=sd(post.dat)
  
  d0_prior1=dcauchy(0)
  d0_post=dnorm(0,m,sd)
  BF <- d0_post/d0_prior1
  
  d2=density(OST$sims.array[,,1])
  f2=splinefun(d2)
  d0_ps=f2(0)
  BF2 <- d0_post/d0_ps
  
  priorchain <- OST$sims.array[,,4]
  postchain <- OST$sims.array[,,1]
  
  
  dat.postJZS <- new.env()
  dat.postJZS$mean <- m
  dat.postJZS$sd <- sd
  dat.postJZS$BF <- BF
  dat.postJZS$BF2 <- BF2
  dat.postJZS$priorchain <- priorchain
  dat.postJZS$postchain  <- postchain
  
  return(dat.postJZS)
  
  #  return(list(m,sd,BF,BF2,priorchain,postchain))
  
  }

posteriorJZS1sided <- function(trep,n2,m2=1,sample=1, WB ="c:/Program Files/WinBUGS14", iters = 50000) {
  
  ### ONE sample 
  
  if( sample ==1){
    
    np2  <-  sqrt(n2)
    nugr <- n2 -1  
    s      <- 1
    mudif  <- trep * (s/sqrt(n2))
    group1 <- rnorm(n2, 0, 1)
    group1 <- (group1-mean(group1))/sqrt(var(group1))
    group1 <- group1 + mudif
    
    data=list('group1','n2')
    
    modelString=
      "
    model{delta~dnorm(0,lambdaDelta)I(0,)
    lambdaDelta ~ dchisqr(1)
    sigma.~dnorm(0,sigmaL)
    sigmaL~dchisqr(1)
    sigma<-abs(sigma.)
    lambdaData<-pow(sigma,-2)
    mu<-delta*sigma
    for (i in 1:n2){group1[i] ~ dnorm(mu,lambdaData)}
    prdelt ~ dnorm(0,lambdaDelta)I(0,)
    }
    "
    
    write(modelString, file='modelfile.bug')
    
    inits=function()
    {
      list(delta=rnorm(1,0,1),sigma.=runif(1,0,5))
    }
    parameters=c('delta','mu','sigma','prdelt')
  }
  
  ####TWO sample 
  
  if ( sample ==2){
    
    np2  <- sqrt( 1/(1/n2+1/m2)) # two sample alternative for sqrt(n)
    mudif  <- trep * (1/np2)
    group1 <- rnorm(n2,0,1)
    group1 <- (group1-mean(group1))/sqrt(var(group1))
    group2 <- rnorm(m2,0,1)
    group2 <- (group2-mean(group2))/sqrt(var(group2))
    group2 <- group2 - mudif
    
    
    data=list('group1','group2','n2','m2')
    
    modelString=
      "
    model{delta~dnorm(0,lambdaDelta)I(0,)
    lambdaDelta ~ dchisqr(1)
    sigma.~dnorm(0,sigmaL)
    sigmaL~dchisqr(1)
    sigma<-abs(sigma.)
    var<-pow(sigma,2)
    mu~dnorm(0,muL)
    muL~dchisqr(1)
    lambdaData<- 1/var
    alpha<-delta*sigma  
    muData1<-mu+alpha*0.5	
    muData2<-mu-alpha*0.5	
    for (i in 1:n1){group1[i] ~ dnorm(muData1,lambdaData)}
    for (i in 1:n2){group2[i] ~ dnorm(muData2,lambdaData)}
    prdelt ~ dnorm(0,lambdaDelta)I(0,)
    }
    "
    
    write(modelString, file='modelfile.bug')
    
    inits=function()
    {
      list(delta=rnorm(1,0,1),mu=rnorm(1,0,1),sigma.=runif(1,0,5))
    }
    parameters=c('delta','mu','sigma','alpha','prdelt')
  }
  
  #### run WinBugs
  
  chains <- 1
  #iters <- 50000
  burns <- iters/10
  thins <-1
  
  library(R2WinBUGS)
  
  
  OST = bugs(data, inits, parameters,  model.file ="modelfile.bug",
             n.chains=chains, n.iter=iters, n.burnin=burns, n.thin=thins,
             bugs.directory=WB,  codaPkg=F,debug=F)
  
  post.dat=OST$sims.array[,,1]
  m=mean(post.dat)
  sd=sd(post.dat)
  
  d0_prior1=dcauchy(0)
  d0_post=dnorm(0,m,sd)
  BF <- d0_post/d0_prior1
  
  d2=density(OST$sims.array[,,1])
  f2=splinefun(d2)
  d0_ps=f2(0)
  BF2 <- d0_post/d0_ps
  
  priorchain <- OST$sims.array[,,4]
  postchain <- OST$sims.array[,,1]
  
  
  dat.postJZS <- new.env()
  dat.postJZS$mean <- m
  dat.postJZS$sd <- sd
  dat.postJZS$BF <- BF
  dat.postJZS$BF2 <- BF2
  dat.postJZS$priorchain <- priorchain
  dat.postJZS$postchain  <- postchain
  
  return(dat.postJZS)
  
  #  return(list(m,sd,BF,BF2,priorchain,postchain))
  
  }


#####################



ReplicationPlot <- function(trep, tobs, n1, n2, posterior.mean, posterior.sd,
                            prior.mudelta, prior.sdelta,REPBF,JZS = 0, 
                            JZSpost.mean = 1, JZSpost.sd =1,JZSBF=0, saveplot = 0, yhigh =  0 )
  
{
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
      font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
  
  if(JZS ==0){
    if(yhigh==0) {yhigh <- dnorm(posterior.mean,posterior.mean,posterior.sd) + .5 }
    scale <- 3
    min.x <-  min((posterior.mean - scale*posterior.sd),(prior.mudelta - scale*prior.sdelta))
    max.x <-  max((posterior.mean + scale*posterior.sd),(prior.mudelta + scale*prior.sdelta))
  }
  
  if(JZS ==1){
    if(yhigh==0) {yhigh <- max(dnorm(prior.mudelta, prior.mudelta, prior.sdelta ),dnorm( posterior.mean, posterior.mean, posterior.sd ),dnorm( JZSpost.mean, JZSpost.mean, JZSpost.sd )) +.5}
    scale <- 3
    min.x <-  min((posterior.mean-scale*posterior.sd), (prior.mudelta-scale*prior.sdelta) , (JZSpost.mean-scale*JZSpost.sd) )
    max.x <-  max((posterior.mean+scale*posterior.sd), (prior.mudelta+scale*prior.sdelta),  (JZSpost.mean+scale*JZSpost.sd) )
  }
  
  #Plot the posterior:
  plot ( function(x) dnorm(x, prior.mudelta, prior.sdelta ), min.x, max.x, ylim=c(0,yhigh), xlim=c(min.x,max.x), lwd=2, lty=3, 
         ylab="Density", xlab=" ") 
  par(new=T)
  
  #Plot the posterior:
  plot ( function(x) dnorm(x, posterior.mean, posterior.sd ), min.x, max.x, ylim=c(0,yhigh), xlim=c(min.x,max.x), lwd=2, lty=1, 
         ylab="Density", xlab=" ") 
  axis(1)
  axis(2)
  mtext(expression(Effect ~ size ~ delta), side=1, line = 2.8, cex=2)
  
  # Plot heights at delta = zero for Savage-Dickey density ratio:
  points(0, dnorm(0,prior.mudelta,prior.sdelta)  , pch=21, cex=2, bg="grey")
  points(0, dnorm(0,posterior.mean,posterior.sd) , pch=21, cex=2, bg="grey")
  
  # plot BF in graph:
  text(posterior.mean, dnorm(posterior.mean,posterior.mean,posterior.sd)+.2, labels = substitute(paste(BFREP[r0], " = ", v), list(v=round(REPBF, digits=2))), cex = 1.5, pos=4)
  
  if ( JZS == 1) {
    par(new=T)
    plot ( function( x ) dnorm( x, JZSpost.mean, JZSpost.sd ), min.x, max.x, ylim=c(0,yhigh), xlim=c(min.x,max.x), lwd=2, lty=1, col= 'gray',
           ylab="Density", xlab=" ") 
    par(new=T)
    plot ( function( x ) dcauchy(x), ylim=c(0,yhigh), xlim=c(min.x,max.x), lwd=2, lty=3, col='gray',
           ylab="Density", xlab=" ") 
    par(new=T)
    plot(rep(0,5),c(0,1,2,3,4) ,type='l',ylim=c(0,yhigh), xlim=c(min.x,max.x),ylab = " ", xlab = " ")
    
    # Plot heights at delta = zero for Savage-Dickey density ratio:
    points(0, dnorm(0,JZSpost.mean,JZSpost.sd )  , pch=21, cex=2, bg="grey")
    points(0, dcauchy(0) , pch=21, cex=2, bg="grey")
    
    # plot BF in graph:
    text(JZSpost.mean+.05, dnorm(JZSpost.mean,JZSpost.mean,JZSpost.sd)+.2, labels = substitute(paste(BFJZS[r0], " = ", v), list(v=round(JZSBF, digits=2))), cex = 1.5, pos=4)
  }
  par(new=F)
  
  #legend(min.x,yhigh, c("REPprior","REPposterior"), lty=c(3,1) ,bty = 'n' )  
  
  if (saveplot == 1 )
  {
    title <-  paste ("torig=", tobs, "trep=", trep , "nobs =", n1,"nrep =", n2 )
    dev.copy(device=jpeg, file= paste(dir,title,".jpg", sep=""))
    dev.off()
  }
  
}


PosteriorDeltaLC <- function(
  tobs,          # t value in first experiment
  n1,            # first experiment: n in group 1 or total n  
  m1       = 1,  # first experiment: n in group 2 or total n
  sample   = 1   # 1 = one sample t-test (or within), 2 = two sample t-test  
)
{
  
  #Function used by Lecoutre to do normal approximations
  
  D    <- tobs 
  if (sample==1) {
    sqrt.n.orig  <-  sqrt(n1)
    df.orig <- n1 -1  
  }
  if (sample==2) {
    sqrt.n.orig  <- sqrt( 1/(1/n1+1/m1)) # two sample alternative for sqrt(n)
    df.orig <- n1 + m1 -2   #degrees of freedom
  }
  
  logk <- (log(2) - log(df.orig))/2 +  log ( gamma ((df.orig+1 )/2)) - log ( gamma ((df.orig)/2))
  k <- exp(logk)
  mean.D <- D * k
  
  # Compute standard deviation for the corresponding normal distribution.
  sdlow.D <- 1+ D^2 - mean.D^2 
  
  # compute prior mean and as for delta 
  prior.mudelta <- mean.D/sqrt.n.orig
  prior.sdelta <-  sdlow.D/sqrt.n.orig
  
  return(list(prior.mudelta, prior.sdelta))
}


BFmeta  <- function(
  tobs,                  # t value in first experiment
  trep,                  # t values in replicated experiment-> now m long
  nreps,        # number of replications
  n1,                    # first experiment: n in group 1 or total n  
  n2,                    # second experiment: ns in group 1 or total n 
  m1       = 1,          # first experiment: n in group 2 or total n -> now m long
  m2       = 1,          # second experiment: ns in group 2 or total n -> now m long
  sample   = 1,        # 1 = one sample t-test (or within), 2 = two sample t-test	
  r=1
)	
  
{
  
  if (sample==1) {
    sqrt.n.orig  <-  sqrt(n1)
    df.orig <- n1 -1  
  }
  
  if (sample==2) {
    sqrt.n.orig  <- sqrt( 1/(1/n1+1/m1)) # two sample alternative for sqrt(n)
    df.orig <- n1 + m1 -2   #degrees of freedom
  }
  
  df.rep <- rep(0,nreps)    
  sqrt.n.rep<- rep(0,nreps)
  
  for (m in 1:nreps){ 
    if (sample==1) {
      df.rep[m] <- n2[m]-1 
      sqrt.n.rep[m] <- sqrt(n2[m])
    }
    
    if (sample==2) {
      df.rep[m] <- n2[m] + m2[m] -2
      sqrt.n.rep[m] <-  sqrt( 1/(1/n2[m]+1/m2[m]))
    }
  }
  
  
  
  BFp0O <- dt(tobs,df.orig)
  BFp0R <- dt(trep,df.rep)
  
  # For Meta-analytic Bayes factor
  priorC=function(delta) dcauchy(delta,0,r)
  metaC=function(delta,t,df,sqrtn) {
    sapply(delta, function(delta) (priorC(delta)*prod(dt(t,df,delta*sqrtn) )) )
  }
  metabf1C= function(t,df,sqrtn)
    integrate(metaC,lower=0,upper=Inf,t=t,df=df,sqrtn=sqrtn)
  
  tM<- c(tobs,trep)
  dfM<- c(df.orig,df.rep)
  sqrtnM<- c(sqrt.n.orig,sqrt.n.rep)
  MB1 <- metabf1C(tM,dfM,sqrtnM)
  
  BFmeta <- MB1$value/prod(c(BFp0R, BFp0O))
  BFmeta
}

BayMayBF <- function(
  tobs,                  # t value in first experiment
  trep,                  # t values in replicated experiment-> now m long
  n1,                    # first experiment: n in group 1 or total n  
  n2,                    # second experiment: ns in group 1 or total n 
  m1       = 1,          # first experiment: n in group 2 or total n -> now m long
  m2       = 1,          # second experiment: ns in group 2 or total n -> now m long
  sample   = 1,        # 1 = one sample t-test (or within), 2 = two sample t-test	
  a=2,
  k=2,
  M= 10000)
{
  library(MCMCpack)
  library(polspline)
  
  # MCMC sampler, Savage Dickey BF
  
  T <- c(tobs, trep)
  
  if (sample==1) {
    n<- c(n1,n2)
    nu <- c(n1-1,n2-1)
  }
  
  if (sample==2) {
    n<- c(1/(1/n1 + 1/m1), 1/(1/n2 + 1/m2))
    nu <- c((n1+m1-2), (n2+m2-2)) 
  }
  
  # for the Bayes factor -> integrate (MC) over conditional distributions 
  # prior functions: 
  priorsig1 = function(s,nu) dinvgamma(s,((nu)/2),((nu+1)/2))
  priorsig2 = function(s,nu) dinvgamma(s,(nu/2),(nu/2))
  priortau  =  function(t,a,k) dinvgamma(t,(a*k),(a+1))
  
  #vnullBF = function(nu,sig2t,n,T) 
  #priorsig1(sig2t[1],nu[1])*priorsig2(sig2t[2],nu[2])* 
  #dnorm(T[2],T[1]*sqrt((n[2]*sig2t[2])/(n[1]*sig2t[1])), sqrt(n[2]*sig2t[2]*(1/n[1]+1/n[2]))) 
  
  vnullBF = function(nu,sig2t,n,T) 
    priorsig1(sig2t[1],nu[1])*priorsig2(sig2t[2],nu[2])* 
    dnorm(T[2],T[1]*sqrt((n[2]*sig2t[2])/(n[1]*sig2t[1])), sqrt(n[2]*sig2t[2]*(1/n[1]+1/n[2]))) 
  
  #valtBF = function(nu,sig2t,n,T,tau,a,k) 
  #priortau(tau,a,k)*priorsig1(sig2t[1],nu[1])* priorsig2(sig2t[2],nu[2])* 
  #dnorm(T[2],T[1]*sqrt((n[2]*sig2t[2])/(n[1]*sig2t[1])), sqrt(n[2]*sig2t[2]*(1/n[1]+1/n[2]+2*tau)) ) 
  
  valtBF = function(nu,sig2t,n,T,tau,a,k) 
    priortau(tau,a,k)*priorsig1(sig2t[1],nu[1])* priorsig2(sig2t[2],nu[2])* 
    dnorm(T[2],T[1]*sqrt((n[2]*sig2t[2])/(n[1]*sig2t[1])), sqrt(n[2]*sig2t[2]*(1/n[1]+1/n[2]+2*tau)) ) 
  
  
  sig2t1 <- rinvgamma(M,((nu[1])/2), ((nu[1]+1)/2))
  sig2t2 <- rinvgamma(M,nu[2]/2,nu[2]/2)
  sig2t <- cbind(sig2t1,sig2t2)
  taut <- rinvgamma(M,(a*k),(a+1))
  
  nullBF <- matrix(0,M,1)
  altBF <- matrix(0,M,1)
  ptm <- matrix(0,M,1)
  
  for (m in 1:M){
    nullBF[m] <- vnullBF(nu,sig2t[m,1:2],n,T)
    altBF[m] <- valtBF(nu,sig2t[m,1:2],n,T,taut[m],a,k)
    naltBF <- valtBF(nu,sig2t[m,1:2],n,T,0,a,k)
  }
  
  
  nul <- mean(nullBF)
  alt <- mean(altBF) 
  nalt <- mean(naltBF) 
  thebf <- mean(nullBF)/ mean(altBF)
  return(c(thebf))
}



JZSBF  <- function(
  tobs,                  # t value in first experiment
  trep,                  # t values in replicated experiment-> now m long
  n1,                    # first experiment: n in group 1 or total n  
  n2,                    # second experiment: ns in group 1 or total n 
  m1       = 1,          # first experiment: n in group 2 or total n -> now m long
  m2       = 1,          # second experiment: ns in group 2 or total n -> now m long
  sample   = 1        # 1 = one sample t-test (or within), 2 = two sample t-test	
)	
  
{
  
  # Define functions (Adapted from Ruud Wetzels): 
  # Numerator Bayes factor component (H0) 
  bf0 <- function(t, nu) {
    (1+t^2/(nu))^(-(nu+1)/2)
  }
  # Construction Bayes factor JZS: 
  # Inverse gamma prior 
  prior <- function(g, n){
    dinvgamma(g, .5, n/2)
  }
  # Denominator Bayes factor component
  bf1a <- function(g, t, n, nu){
    (1+n*g)^(-.5)*(1+t^2/((1+n*g)*(nu)))^(-(nu+1)/2)
  }
  # Prior times likelihood
  joint <- function(g, t, n, nu, r2){
    prior(g, 1)*bf1a((g*r2), t, n, nu)
  }
  # Denominator Bayes factor component JZS prior
  bf1 <-  function(t, n, nu, r2){
    integrate(joint, lower=0, upper=Inf, t=t, n=n, nu=nu, r2=r2)
  }
  # Bayes Factor JZS: 
  bf <- function(t, n, nu, r2){
    bf0(t, nu)/ bf1(t, n, nu, r2)$value
  }
  
  if (sample == 2) {
    N.orig <- n1*m1/(n1+m1)
    df.orig <- n1+m1-2  
    N.rep <- n2*m2/(n2+m2)
    df.rep <- n2+m2-2  
  }
  if (sample == 1) {
    N.orig <- n1
    df.orig <- n1-1  
    N.rep <- n2
    df.rep <- n2-1  
  }
  
  # Compute JZS prior Bayes Factor 10
  JZSBFR <- 1/bf(trep, N.rep, df.rep, 1)
  JZSBFO <- 1/bf(tobs, N.orig, df.orig , 1)
  
  return(c(JZSBFO,JZSBFR))
}

