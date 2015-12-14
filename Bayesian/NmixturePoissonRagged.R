
sink("Bayesian/NmixturePoissonRagged.jags")

cat("
    model {
    for (x in 1:Nobs){
    
    # Covariates for true state   
    log(lambda[Bird[x],Plant[x],Time[x]]) <- alpha[Bird[x]] + beta1[Bird[x]] * traitmatch[x] + beta2[Bird[x]] * resources[x] + beta3[Bird[x]] * resources[x] * traitmatch[x]      
    
    #True State
    N[x] ~ dpois(lambda[Bird[x],Plant[x],Time[x]] )    
    
    #Observation Process
    Yobs[x] ~ dbin(detect[Bird[x]],N[x])    

    #Assess Model Fit
      
    #Fit discrepancy statistics
    eval[x]<-detect[Bird[x]]*N[x]
    E[x]<-pow((Yobs[x]-eval[x]),2)/(eval[x]+0.5)

    ynew[x]~dbin(detect[Bird[x]],N[x])
    E.new[x]<-pow((ynew[x]-eval[x]),2)/(eval[x]+0.5)

    }
    
    for (i in 1:Birds){
    logit(detect[i]) <- dtrans[i]
    dtrans[i] ~ dnorm(dprior,tau_detect)
    alpha[i] ~ dnorm(intercept,tau_alpha)
    beta1[i] ~ dnorm(gamma1,tau_beta1)    
    beta2[i] ~ dnorm(gamma2,tau_beta2)    
    beta3[i] ~ dnorm(gamma3,tau_beta3)    
    }
  

    #Hyperpriors
    #Slope grouping
    gamma1~dnorm(0,0.0001)
    gamma2~dnorm(0,0.0001)
    gamma3~dnorm(0,0.0001)
    
    #Intercept grouping
    intercept~dnorm(0,0.0001)
  
    #detect grouping
    dprior~dnorm(0,1)
    
    # Group intercept variance
    tau_alpha ~ dgamma(0.0001,0.0001)
    sigma_int<-pow(1/tau_alpha,0.5) #Derived Quantity
    
    #Group Slope Variance
    tau_beta1 ~ dgamma(0.0001,0.0001)
    tau_beta2 ~ dgamma(0.0001,0.0001)
    tau_beta3 ~ dgamma(0.0001,0.0001)
    
    sigma_slope1<-pow(1/tau_beta1,0.5)
    sigma_slope2<-pow(1/tau_beta2,0.5)
    sigma_slope3<-pow(1/tau_beta3,0.5)
   
    #Group Detection variance
    tau_detect ~ dgamma(0.0001,0.0001)
    sigma_detect<-pow(1/tau_detect,0.5)

    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])
    
    }
    ",fill=TRUE)

sink()
