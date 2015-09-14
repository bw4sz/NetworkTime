
sink("Bayesian/NMixture.jags")

cat("
    model {
    
    for (i in 1:Birds){
    for (j in 1:Plants){
    
    #Process Model
    log(lambda[i,j]) <- alpha[i] + beta1[i] * traitmatch[x] + beta2[i] * Availability[x] + beta3[i] * Availability[x] * traitmatch[x]
    
    #True state model  
    N[i,j] ~ dpois(lambda[i,j])
    }
    }
    
    #Observation Model
    for (i in 1:Nobs){
    Y[i] ~ dbin(detect[Bird[i]],N[Bird[i],Plant[i]]) 
    
    #Fit discrepancy statistics
    eval[i]<-detect[Bird[i]]*N[Bird[i],Plant[i]]
    E[i]<-pow((Y[i]-eval[i]),2)/(eval[i]+0.5)
    
    y.new[i]~dbin(detect[Bird[i]],N[Bird[i],Plant[i]])
    E.new[i]<-pow((y.new[i]-eval[i]),2)/(eval[i]+0.5)
    }
    
    for (k in 1:Birds){
    detect[k] ~ dunif(0,1) # Detection for each bird species
    alpha[k] ~ dnorm(intercept,tau_alpha)
    beta1[k] ~ dnorm(gamma1,tau_beta1)    
    beta2[k] ~ dnorm(gamma2,tau_beta2)    
    beta3[k] ~ dnorm(gamma3,tau_beta3)    
    }
    
    #Hyperpriors
    #slope
    gamma1~dnorm(0.001,0.0001)
    gamma2~dnorm(0.001,0.0001)
    gamma3~dnorm(0.001,0.0001)

    #intercept
    intercept~dnorm(0.001,0.001)
    
    tau_alpha ~ dgamma(0.0001,0.0001)
    sigma_int<-pow(1/tau_alpha,0.5) #Derived Quantity
    
    #slope priors
    tau_beta1 ~ dgamma(0.0001,0.0001)
    sigma_slope1<-pow(1/tau_beta1,0.5)

    tau_beta2 ~ dgamma(0.0001,0.0001)
    sigma_slope2<-pow(1/tau_beta2,0.5)

    tau_beta3 ~ dgamma(0.0001,0.0001)
    sigma_slope3<-pow(1/tau_beta3,0.5)
    
    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])
    
    }
    ",fill=TRUE)

sink()
