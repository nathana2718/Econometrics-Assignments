library(R6) #using R6 oop system
regMCloop = R6Class(
  public = list(
    nrange=NULL,
    sigmarange=NULL,
    Urange=NULL,
    a2range=NULL,
    results = NULL,
    
    initialize = function(nrange,sigmarange,Urange,a2range=0)
    {
      self$nrange = nrange
      self$sigmarange = sigmarange
      self$Urange = Urange
      self$a2range = a2range
      self$results = vector("list",length(nrange)*length(sigmarange)*length(Urange)*length(a2range))
    },
    
    loop = function(a1=1, a3=1, b1=1, b2=1, b3=0)
    {
      i=1#elements of result vector
      for(n in self$nrange)
      {
        for(sigma in self$sigmarange)
        {
          for(dist in self$Urange)
          {
            for(a2 in self$a2range)
            {
              self$results[[i]] = regMClass$new(ns=n,sigma=sigma,dist=dist, a2=a2, a3=a3, b1=b1, b2=b2, b3=b3, a1=a1)
              i=i+1#next result
            }
          }
        }
      }
    }
    
    
  )
)

regMClass = R6Class(
  public = list(
    bias = NULL,
    sd = NULL,
    CovRate=NULL,
    CovLength=NULL,
    ns = NULL,
    sigma= NULL,
    dist= NULL,
    a2 =NULL,
    
    
    initialize = function(ns = 20, n=1000, b1=1, b2=1, b3=0, a1=1, a2=0, a3=1, sigma=1,dist="norm")
    { 
      intervalrange =vector(mode="numeric",length=0)
      b1list = vector(mode="numeric",length=0)
      inInterval=vector(mode="logical",length=0)#declare vectors so that append method works
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
        intervalrange = append(intervalrange,interval[2]-interval[1])
      }
      self$ns = ns # so we can sort by values used
      self$sigma = sigma
      self$dist = dist
      self$a2 = a2
      self$bias = mean(b1list)-1
      self$sd = sd(b1list)
      self$CovRate = mean(inInterval)
      self$CovLength = mean(intervalrange)
    }
  )
)
