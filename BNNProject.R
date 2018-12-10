require(readr)
require(data.table)
require(tidyverse)
install.packages("R2jags")
install.packages("lattice")
library(R2jags)
library(lattice)

merged <- read_rds("/Users/briannaking/Downloads/reshaped.rds")

merged <- na.omit(merged)
GoT_data <- list(Y=merged$isAlive,X=merged[,c("popularity","gender","FirstAppearanceChap","noble","Essos")],n=nrow(merged))

X <- merged[,c("FirstAppearanceChap","gender","Essos","popularity","noble")]
Y <- merged$isAlive
n = length(Y)

#split data into train and test
set.seed(100)
n_train = 500
idx_train = sample(1:n,n_train)
idx_test = setdiff(1:n, idx_train)
X_train = X[idx_train,]
Y_train = Y[idx_train]

X_test = X[idx_test,]
Y_test = Y[idx_test]

# Bayesian Neural Network
Bayes_1L_NN = function(Y_train,X_train,X_test,q=3L,
                       n.iter=9000,n.burnin=1000){
  JAGS.NN.model = function() {
    # Likelihood
    for (i in 1:n_train) {
      Y_train[i] ~ dbern(prob[i])
      logit(prob[i]) <-  beta00 + inprod(Z_train[i,],beta01)
      for(l in 1:q){
        logit(Z_train[i,l]) <-  beta10[l] + inprod(X_train[i,],beta11[l,])
      }
    }
    #prior
    beta00 ~ dnorm(0,0.001)
    
    for(l in 1:q){
      beta01[l] ~ dnorm(0,0.001)
    }
    
    for(l in 1:q){
      for(j in 1:p){
        beta11[l,j] ~ dnorm(0,0.001)
      }
      beta10[l] ~ dnorm(0,0.001)
    }
    
    #prediction
    for(i in 1:n_pred){
      Y_pred[i] ~ dbern(p_pred[i])
      logit(p_pred[i]) <- beta00 + inprod(Z_train_pred[i,],beta01)
      for(l in 1:q){
        logit(Z_train_pred[i,l]) <-  beta10[l] + inprod(X_test[i,],beta11[l,])
      }
    }
  }
  JAGS.NN.data = list(Y_train=Y_train,
                      X_train=X_train,
                      X_test=X_test,
                      q = q,
                      n_train = as.integer(nrow(X_train)),
                      n_pred = as.integer(nrow(X_test)),
                      p = as.integer(ncol(X_train)))
  #set parameters to simulate
  para.JAGS = c("Y_pred")
  fit.JAGS = jags(data=JAGS.NN.data,inits=NULL,
                  parameters.to.save = para.JAGS,
                  n.chains=1,n.iter=n.iter,n.burnin=n.burnin,
                  model.file=JAGS.NN.model)
  mcmc_fit = as.mcmc(fit.JAGS)
  Y_pred_sample = mcmc_fit[[1]][,paste("Y_pred[",1:nrow(X_test),"]",sep="")]
  Y_pred=apply(Y_pred_sample,2,mean)
  Y_pred_CI = apply(Y_pred_sample,2,quantile,prob=c(0.025,0.975))
  return(list(Y_pred=Y_pred,
              Y_pred_lcl = Y_pred_CI[1,],
              Y_pred_ucl = Y_pred_CI[2,],
              DIC = fit.JAGS$BUGSoutput$DIC,
              pD = fit.JAGS$BUGSoutput$pD,
              fit.JAGS=fit.JAGS))
}

summary_res = function(res,Y_test){
  PMSE = mean((res$Y_pred-Y_test)^2)
  coverage = mean((Y_test>res$Y_pred_lcl)&(Y_test<res$Y_pred_ucl))
  return(c(PMSE=PMSE,coverage=coverage))
}


res_q5 = Bayes_1L_NN(Y_train=Y_train,
                     X_train=cbind(X_train),
                     X_test=cbind(X_test),q = 5L) # examine res_q5 to see pD and DIC
res_q5

res_q10 = Bayes_1L_NN(Y_train=Y_train,
                      X_train=cbind(X_train),
                      X_test=cbind(X_test),q = 10L)

res_q25 = Bayes_1L_NN(Y_train=Y_train,
                      X_train=cbind(X_train),
                      X_test=cbind(X_test),q = 25L)

res_q30 = Bayes_1L_NN(Y_train=Y_train,
                      X_train=cbind(X_train),
                      X_test=cbind(X_test),q = 30L)


par(mfcol=c(2,2))
plot_res(res_q5,main=sprintf("K = 5 DIC = %.2f",res_q5$DIC))
plot_res(res_q10,main=sprintf("K = 10 DIC = %.2f",res_q10$DIC))
plot_res(res_q25,main=sprintf("K = 25 DIC = %.2f",res_q25$DIC))
plot_res(res_q25,main=sprintf("K = 30 DIC = %.2f",res_q30$DIC))

tab = NULL
#tab = rbind(tab,summary_res(res_q3,Y_test))
tab = rbind(tab,summary_res(res_q5,Y_test))
tab = rbind(tab,summary_res(res_q10,Y_test))
tab = rbind(tab,summary_res(res_q25,Y_test))
tab = rbind(tab,summary_res(res_q30,Y_test))
rownames(tab) = paste("K = ", c(5,10,25,30), sep="")
print(tab)

#Diagnostics 
install.packages(c("rjags","R2jags","runjags","MCMCpack"))
library(rjags)
library(R2jags)
library(runjags)
library(MCMCpack)
# Hit "esc" button to sotp "Hit <Return> to see next plot"
#summarizing the results
print(res_q5$fit.JAGS)
#plot
#as an rjags object
plot(res_q5$fit.JAGS) 
#as an mcmc object
fit.JAGS.mcmc = as.mcmc(res_q5$fit.JAGS)
fit.JAGS.mcmc = as.mcmc(res_q5$fit.JAGS)
a <- geweke.diag(fit.JAGS.mcmc) # ask Jian if this can be used for logistic model convergence (all of the Z values have an absolute value smaller than 2)
plot(fit.JAGS.mcmc,ask=TRUE)
#directly traceplot
traceplot(res_q5$fit.JAGS)
traceplot(res_q5$fit.JAGS,mfrow=c(2,3),ask=FALSE)

# trace plot below isnt making sense
summary(fit.JAGS.mcmc)
#traceplot as an mcmc object
# xyplot(fit.JAGS.mcmc)
# xyplot(fit.JAGS.mcmc,layout=c(2,3))
#densityplot as an mcmc object
densityplot(fit.JAGS.mcmc)
densityplot(fit.JAGS.mcmc,layout=c(3,2))
#Autocorrelation plot
autocorr.plot(fit.JAGS.mcmc,auto.layout = FALSE)

# checking accuracy
thresh <- 0.5
predFac <- cut(res_q5$Y_pred, breaks=c(-Inf, thresh, Inf), labels=c("Dead","Alive"))
cTab <- table(Y_test, predFac, dnn=c("actual","predicted"))
cTab # prediction accuracy with everything in model except south_region, north_region, and middle_region is 0.63
# pred accuracy with gender and FirstChapAppearance only in model is 

# Model Summaries:
# Model with "popularity", "gender", "FirstAppearanceChap","noble","Essos","south_region","north_region","middle_region"
# pD = 28.6
# DIC = 554.1
# prediction accuracy = 72.3%
# this predicted 10 characters to die who did not actually die, but it predicted 5 dead correctly
# table: 5, 83, 10, 238 (a,b,c,d) (5 = characters predicted dead and actually dead, 83 characters predicted Alive but actually dead,
# 10 characters predicted dead but actually alive, 238 predicted alive and actually alive)

# Model with "popularity","gender","FirstAppearanceChap","noble","Essos"
# pD = 20.7
# DIC = 573.0
# prediction accuracy = 73.2% (it predicted only 4 people out of 336 to be dead)
# about 26% of characters actually die, yet we're predicting 1.2% to die
# characters BNN thought would die but did not: Meryn Trant (row 498 of merged data), Hallis Mollen (row 296), Stonesnake (row 708)
# Our model may be mostly using FirstAppearanceChap to determine dead/alive. Early first appearance --> more likely to die
# table: 1, 87, 3, 245 (a,b,c,d)

# Model with only gender, FirstAppearanceChap, Essos (these were most significant in glm I believe)
# pD = 11.4
# DIC = 572.6
# prediction accuracy = 74.4%
# although this is more accurate, it predicted everyone to be alive except for 2 people
# table: 2, 86, 0, 248

# Comparison: if our model predicted everyone to be alive, we'd have 73.8% accuracy
# our models aren't that great because they're not much better than predicting everyone to be alive
# but the Bayesian neural network model is doing better than glm

# Attempting cross validation
##### Cross Validation #####

cv_index_split = function(n, fold = 5, random = TRUE) {
  all_indices = 1:n
  cv_idx = list()
  n_test = floor(n / fold)
  test_idx_pool = all_indices
  for (k in 1:fold) {
    if (length(test_idx_pool) > n_test) {
      if (random == TRUE)
        test_idx = sample(test_idx_pool, n_test)
      else
        test_idx = (k - 1) * n_test + 1:n_test
      test_idx_pool = setdiff(test_idx_pool, test_idx)
    }
    else{
      test_idx = test_idx_pool
    }
    cv_idx[[k]] = list(test_idx = test_idx,
                       train_idx = setdiff(all_indices, test_idx))
  }
  return(cv_idx)
}

JAGS_GOT_pred = function() {
  # Likelihood
  for (i in 1:n_train) {
    Y_train[i] ~ dbern(prob[i])
    logit(prob[i]) <-  beta00 + inprod(Z_train[i,],beta01)
    for(l in 1:q){
      logit(Z_train[i,l]) <-  beta10[l] + inprod(X_train[i,],beta11[l,])
    }
  }
  #prior
  beta00 ~ dnorm(0,0.001)
  
  for(l in 1:q){
    beta01[l] ~ dnorm(0,0.001)
  }
  
  for(l in 1:q){
    for(j in 1:p){
      beta11[l,j] ~ dnorm(0,0.001)
    }
    beta10[l] ~ dnorm(0,0.001)
  }
  
  #prediction
  for(i in 1:n_pred){
    Y_pred[i] ~ dbern(p_pred[i])
    logit(p_pred[i]) <- beta00 + inprod(Z_train_pred[i,],beta01)
    for(l in 1:q){
      logit(Z_train_pred[i,l]) <-  beta10[l] + inprod(X_test[i,],beta11[l,])
    }
  }
}

##### directly implement cross validation
fold = 5
q = 5L
set.seed(27)
cv_idx = cv_index_split(GoT_data$n, fold = 5)
CV_MSE = list()
CV_MSE$BNN = rep(NA, length = fold)

for (k in 1:fold) {
  train_idx  = cv_idx[[k]]$train_idx
  n_train = length(train_idx)
  test_idx  = cv_idx[[k]]$test_idx
  n_test = length(test_idx)
  
  fit_JAGS = jags(
    data = list(
      Y = GoT_data$Y[train_idx],
      X = GoT_data$X[train_idx],
      X_pred = GoT_data$X[test_idx],
      n = n_train,
      n_pred = n_test
    ),
    parameters.to.save = c("Y_pred"),
    n.chains = 1,
    n.iter = 10000,
    n.burnin = 1000,
    model.file = Bayes_1L_NN
  )
  
  Y_pred = apply(as.mcmc(fit_JAGS)[[1]][, paste("Y_pred[", 1:n_test, "]", sep =
                                                  "")], 2, mean)
  CV_MSE$BNN[k] =  mean((Y_pred - GoT_data$Y[test_idx]) ^ 2)
}
apply(as.data.frame(CV_MSE), 2, mean)


# attempting ROC curve
library(pROC)
plot(roc(res_q5$Y_pred,Y_test,direction=">"))
 