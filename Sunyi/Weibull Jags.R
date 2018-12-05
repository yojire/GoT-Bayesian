

setwd("/Users/sunyichi/Documents/GitHub/GoT-Bayesian/")
data=readRDS("data1204.rds")
require(rjags)
negative<-c(which(data$In_Book < 0),which(is.na(data$In_Book)))
data=data[-negative,]
prior_model_Weibull = "model{
for (i in 1:N){
In_Book[i] ~ dweib(alpha,beta)
}
alpha ~ dunif(0,100)
beta ~ dunif(0,100)

theta <- pow(-log(0.5)/beta,1/alpha)
lambda <- exp(-beta*pow(271,alpha))
}"

prior_model.con_W<-textConnection(prior_model_Weibull)

data$In_Book_15 = as.integer(data$In_Book)
N_15 = length(data$In_Book)
jags15 = jags.model(prior_model.con_W, n.chains = 3, n.adapt = 200, data = list('In_Book' = data$In_Book_15, 'N' = N_15))
sample15 = coda.samples(jags15, c("alpha","beta","theta", "lambda"), 10000)
#quartz()
plot(sample15, trace = FALSE)
summary(sample15)
gelman.diag(sample15) # Gelman and Rubin Convergence Diagnostic
#quartz()
gelman.plot(sample15)



prior_model_Weibull = "model{
for (i in 1:N){
In_Book[i] ~ dweib(alpha,beta)
}
alpha ~ dunif(0,100)
beta ~ dunif(0,100)

theta <- pow(-log(0.5)/beta,1/alpha)
lambda <- exp(-beta*pow(150,alpha))
}"

prior_model.con_W<-textConnection(prior_model_Weibull)

In_Book_14 = as.integer(data2$In_Book)
N_14 = length(data2$In_Book)
jags14 = jags.model(prior_model.con_W, n.chains = 3, n.adapt = 200, data = list('In_Book' = In_Book_14, 'N' = N_14))
sample14 = coda.samples(jags14, c("alpha","beta", "theta", "lambda"), 10000)
#quartz()
plot(sample14, trace = FALSE)
summary(sample14)
gelman.diag(sample14) # Gelman and Rubin Convergence Diagnostic
#quartz()
gelman.plot(sample14)