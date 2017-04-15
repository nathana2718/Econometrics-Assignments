#requires regMC
for(n in c(20,200,2000))
  {
    for(sigma in c(1,2))
    {
      for(dist in c("norm","lnorm"))
      {
        cat("\n",n,"samples","sigma=",sigma,"dist=",dist,":")
        regMC(ns=n,sigma=sigma,dist=dist)
        
      }
    }
  }