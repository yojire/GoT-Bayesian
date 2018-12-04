require(rjags)

# Model： random effects logit model

RE_logit_model = "model {
for (i in 1:N) {
y[i] ~ dbin(p[i],n[i])
mu[i] <- alpha1 + alpha2*x1[i] + alpha3*x2[i] + alpha4*x1[i]*x2[i]
beta[i] ~ dnorm(mu[i],tau)
logit(p[i]) <- beta[i]
}
alpha1 ~ dnorm(0.0,1.0E-6)
alpha2 ~ dnorm(0.0,1.0E-6)
alpha3 ~ dnorm(0.0,1.0E-6)
alpha4 ~ dnorm(0.0,1.0E-6)
tau ~ dgamma(0.001,0.001)
sd <- sqrt(1.0/tau)
}"

RE_logit_model.con <- textConnection(RE_logit_model)

# Data from Book 1-5
y.15 = c(20,20,21,18,18,24,17,21,26,23,21,16,20,31,23,29,16,15,19,20,25,32,25,20,17,33,18,25,20,20,33,24,27,22,23,25,19,21,26,31,21,23,15,20,21,15,13,15,13,15,18,19,11,19,11,18,13,16,11,22,22,26,18,22,29,24,18,26,22,18,28,26,22,23,11,17,21,18,17,19)
n.15 = c(33,31,32,27,24,34,23,30,34,36,31,25,30,38,32,37,24,27,31,26,28,36,26,24,21,35,21,27,23,22,36,26,31,26,27,28,21,27,29,35,34,39,27,34,39,32,23,21,27,24,33,37,21,31,22,33,26,27,23,28,36,35,25,31,36,36,27,33,35,29,40,34,37,35,20,28,33,21,27,27)
x1.15 = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
x2.15 = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
N.15 = length(y.15)

jags.RElogit.15 = jags.model(RE_logit_model.con, n.chains = 2, n.adapt = 200, data = list('y'= y.15, 'n'= n.15, 'x1'= x1.15, 'x2'= x2.15, 'N'= N.15 ))
sample.RElogit.15 = coda.samples(jags.RElogit.15, c("alpha1","alpha2","alpha3", "alpha4"), 10000)
#quartz()
plot(sample.RElogit.15, trace = FALSE)
summary(sample.RElogit.15)
gelman.diag(sample.RElogit.15) # Gelman and Rubin Cinvergence Diagnostic
#quartz()
gelman.plot(sample.RElogit.15) 

# Data from Book 1-4
RE_logit_model = "model {
for (i in 1:N) {
y[i] ~ dbin(p[i],n[i])
mu[i] <- alpha1 + alpha2*x1[i] + alpha3*x2[i] + alpha4*x1[i]*x2[i]
beta[i] ~ dnorm(mu[i],tau)
logit(p[i]) <- beta[i]
}
alpha1 ~ dnorm(0.0,1.0E-6)
alpha2 ~ dnorm(0.0,1.0E-6)
alpha3 ~ dnorm(0.0,1.0E-6)
alpha4 ~ dnorm(0.0,1.0E-6)
tau ~ dgamma(0.001,0.001)
sd <- sqrt(1.0/tau)
}"

RE_logit_model.con <- textConnection(RE_logit_model)

y.14 = c(31,20,20,20,23,18,23,24,26,22,17,27,26,15,23,15,21,23,25,19,24,33,21,21,29,27,23,31,22,32,24,29,29,23,27,32,25,24,28,25,24,18,17,21,13,24,12,24,23,18,16,22,20,15,10,17,20,15,16,14,22,27,17,25,17,24,26,15,24,27,24,22,22,19,19,21,27,25,22,28)
n.14 = c(38,29,28,30,28,23,33,31,36,36,27,39,39,23,34,23,27,37,39,28,31,35,24,26,35,34,27,39,25,38,30,36,33,28,31,36,31,26,31,29,34,37,29,38,27,40,21,37,35,34,25,33,33,26,25,25,30,27,25,25,30,36,23,36,24,31,37,21,35,34,32,26,33,30,27,27,33,29,32,34)
x1.14 = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
x2.14 = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
N.14 = length(y.14)

jags.RElogit.14 = jags.model(RE_logit_model.con, n.chains = 2, n.adapt = 200, data = list('y'= y.14, 'n'= n.14, 'x1'= x1.14, 'x2'= x2.14, 'N'= N.14))
sample.RElogit.14 = coda.samples(jags.RElogit.14, c("alpha1","alpha2","alpha3", "alpha4"), 10000)
#quartz()
plot(sample.RElogit.14, trace = FALSE)
summary(sample.RElogit.14)
gelman.diag(sample.RElogit.14) # Gelman and Rubin Cinvergence Diagnostic
#quartz()
gelman.plot(sample.RElogit.14) 

