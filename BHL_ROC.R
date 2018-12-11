pacman::p_load(readr, data.table, tidyverse, R2jags, pROC)
reshaped = read_rds("reshaped.rds")
reshaped = na.omit(reshaped)
reshaped[, Westeros := north_region+middle_region+south_region]

set.seed(100)
train_set = sample(1:nrow(reshaped),500)
test_set = sample(setdiff(1:nrow(reshaped),train_set),(nrow(reshaped)-500))
GoT_train = reshaped[train_set,]
GoT_test = reshaped[test_set,]

Y_train = GoT_train$isAlive
X_train = GoT_train[,c(4,8,7,11,24,2)]
Y_test = GoT_test$isAlive
X_test = GoT_test[,c(4,8,7,11,24,2)]

n = length(Y_train)
n_pred = length(Y_test)


JAGS_GoT_model = function() {
  # Likelihood
  for (i in 1:n) {
    Y_train[i] ~ dbern(prob[i])
    logit(prob[i]) <-  beta_0 + inprod(X_train[i,1:5],beta) + X_train[i,6] * ifelse(X_train[i,2],alpha[1],-alpha[2])
  }
  #prior
  beta_0 ~ dnorm(0,0.001)
  
  meanbeta = c(-1, .5, 5e-3, 1, 0)
  for(j in 1:5){
    beta[j] ~ dnorm(meanbeta[j],0.001)
  }
  
  for(j in 1:2){
    alpha[j] ~ dgamma(2,1)
  }
  
  #prediction
  for(i in 1:n_pred){
    Y_pred[i] ~ dbern(p_pred[i])
    logit(p_pred[i]) <- beta_0 + inprod(X_test[i,1:5],beta) + X_test[i,6] * ifelse(X_test[i,2],alpha[1],-alpha[2])
  }
}

fit_JAGS_GoT_model = jags(
  data = list(Y_train = Y_train, n=n, X_train=as.matrix(X_train), X_test=as.matrix(X_test),n_pred=n_pred),
  inits = list(list(Y_pred=rep(0,length=n_pred),beta_0 = 0,beta=c(-1, .5, 5e-3, 1, 0),alpha=c(1,1))),
  parameters.to.save = c("Y_pred","alpha"),
  n.chains = 1,
  n.iter = 10000,
  n.burnin = 2000,
  model.file = JAGS_GoT_model
)


pred_prob <- summary(as.mcmc(fit_JAGS_GoT_model))$statistics[paste("Y_pred[",1:n_pred,"]",sep=""),1]

plot(roc(Y_test,pred_prob,direction = "<"), xlim=c(1,0))