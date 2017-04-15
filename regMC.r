#Uses a montecarlo simulation to estimate parameters
regMC=function(ns = 20, n=1000, b1=1, b2=1, b3=0, a1=1, a2=1, a3=0, sigma=1,dist="norm")
{ 
  b1list = vector(mode="numeric",length=0)
  inInterval=vector(mode="logical",length=0)#so that append method works
  for(j in 1:n)
  {
    #__init__
    Z1 = rnorm(n=ns,mean=0,sd=1)
    X2 = rnorm(n=ns,mean=0,sd=1)
    X3 = rnorm(n=ns,mean=0,sd=1)
    V = rnorm(n=ns,mean=0,sd=1)
    
    if (dist == "norm")
    {
      U = rnorm(n=ns,mean=0,sd=1) 
    }
    else
    {
      U = rlnorm(n=ns,mean=0,sd=1)
    }
    #Generate X1 and Y
    X1 = a1*Z1 + a2*X2 + a3*X3 +V
    Y = b1*X1 + b2*X2 +b3*X3 + sigma*U
    #Estimates
    hat = lm(Y ~ X1+X2)
    b1list=append(b1list,hat$coefficients["X1"])
    #In confidence interval?
    interval = confint(hat,parm = "X1",interval="confidence")
    inInterval = append(inInterval,b1<interval[2]&b1>interval[1])
    intervalrange = interval[2]-interval[1]
  }
  cat("\n","bias=",(mean(b1list)-1),"sd=",(sd(b1list)),"CoverageRate=",(mean(inInterval)),"IntervalRange=",(mean(intervalrange)))
}