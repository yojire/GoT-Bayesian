

setwd("/Users/sunyichi/Documents/GitHub/GoT-Bayesian/")
data=readRDS("data1204.rds")
require(rjags)
negative<-c(which(data$In_Book < 0),which(is.na(data$In_Book)))
data=data[-negative,]
data[is.na(data$gender),4]=c(1,1)
N= length(data$In_Book)

library(R2jags)
set.seed(1)
JAGS_RE_model=function(){
  #Likelihood
  for (i in 1:N){
    In_Book[i] ~ dweib(shape,lambda[i])
    lambda[i] <- exp(-mu[i] * shape)
    mu[i]<-beta0+beta1*popularity[i]+beta2*gender[i]+beta3*noble[i]
  }
  #Priors
  shape ~ dgamma(.001, .001)
  beta0~dnorm(1,0.001)  #most characters only love for one chapters
    beta1~dnorm(0,0.001)
    beta2~dnorm(0,0.001)
    beta3~dnorm(0,0.001)
  #Output
   # theta <- pow(-log(0.5)/theta,1/alpha)
   # lambda <- exp(-theta*pow(271,alpha))
  #for (i in 1:N){
  #  pred[i]<-beta0+beta1*popularity[i]+beta2*gender[i]+beta3*noble[i]
  #} 
}
fit_JAGS_RE_model = jags(data = list('N'= length(data$In_Book),'In_Book' = data$In_Book,'popularity'=data$popularity,'gender'=data$gender,'noble'=data$noble),
  parameters.to.save = c("shape","beta0","beta1","beta2","beta3"),
  n.chains = 3,model.file = JAGS_RE_model)

library(lattice)
densityplot(as.mcmc(fit_JAGS_RE_model))


gelman.diag(as.mcmc(fit_JAGS_RE_model))
gelman.plot(as.mcmc(fit_JAGS_RE_model))

