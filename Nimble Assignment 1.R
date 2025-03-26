# Load packages
library(tidyverse)
library(nimble)
library(igraph)
library(coda)
par(mar = c(1,1,1,1))



#2b) Write BUGS code using the R package Nimble to fit the suggested model. 

model_code <- nimbleCode({
  
  # priors
  alpha ~ dnorm(mu_alpha, sd = sigma_alpha)
  beta ~ dnorm(mu_beta, sd = sigma_beta)
  gamma ~ dnorm(mu_gamma, sd = sigma_gamma)
  variance ~ dinvgamma(a, b)
  
  # likelihood
  for (i in seq_len(N)) { 
    predicted_y[i] <- alpha + beta * x[i] + gamma * z[i]
    y[i] ~ dnorm(predicted_y[i], sd = sqrt(variance))
  }
})

# vague priors with reasonable mean for the distributions, 
# but with a large standard deviation (sd) parameter
mu_alpha <- 0;
sigma_alpha <- 10;
mu_beta <- 4;
sigma_beta <- 10;
mu_gamma <- 3; 
sigma_gamma <- 10;
a <- 3;
b <- 40;

#Check of a and b
# inverse gamma with a = 3 and b = 40 => mean and sd around 20
x <- seq(0.01, 70, length.out = 2000);
y <- dinvgamma(x, a, b);
mean(rinvgamma(1000000, a, b))
sd(rinvgamma(1000000, a, b))
# plot(x, y)


# explained variable: time spent telephoning in a given day (minutes)
y <- c(19.9, 27.3, 21.0, 3.7, 48.4, 66.4, 13.1, 38.6, 22.7, 17.4, 40.7, 
       47.0, 53.7, 61.9, 80.8, 11.4, 16.2, 8.6, 32.6, 60.4)

# explanatory variable: number of telephone calls made by individual
x <- c(3, 6, 4, 1, 9, 15, 1, 7, 6, 2, 9, 12, 14, 17, 18, 3, 5, 2, 8, 12)

# explanatory variable: weekday / weekend
z <- c(1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)




# 2a) Plot the data, including the plot in your solutions.                   
    #By only looking at the plot, does the suggested model seem sensible?   

xyz = data.frame(x, y, z); # imput data to dataframe plot
xyz$z_label = ifelse(z == 1, "Weekend", "Weekday");

ggplot(xyz, mapping = aes(x = x, y = y, color = z_label)) + 
  geom_point(size = 1) + 
  stat_smooth(method = "lm", formula = "y ~ x") + 
  theme(legend.position = "top") +
  scale_color_discrete(name = "")  +
  labs(x = "number of phone calls", y = "length of time spent telephoning in a day (minutes)")



# run MCMC
constants <- list(N = length(x), 
                  mu_alpha = mu_alpha, sigma_alpha = sigma_alpha,
                  mu_beta = mu_beta, sigma_beta = sigma_beta,
                  mu_gamma = mu_gamma, sigma_gamma = sigma_gamma,
                  a = a, b = b)
data <- list(y = y, x = x, z = z)


# assessing convergence to the stationary distribution 
# via the assessment of multiple replications starting from overdispersed starting points.
inits <- function() list(alpha = rnorm(1, mu_alpha, 3 * sigma_alpha), 
                         beta = rnorm(1, mu_beta, 3 * sigma_beta),
                         gamma = rnorm(1, mu_gamma, 3 * sigma_gamma),
                         variance = rinvgamma(1, a, b))

# get MCMC samples (returned as coda mcmc object, ready to anlisys and plot)
niter <- 2000
nchains <- 3
mcmc <- nimbleMCMC(code = model_code, 
                   constants = constants, 
                   data = data,
                   inits = inits,
                   niter = niter,
                   nchains = nchains,
                   setSeed = T, 
                   samplesAsCodaMCMC = TRUE)


# 2c) Perform ALL MCMC checks learnt in this module,                         
#     with particular attention to: burnin, convergence and chain length.     
#     Results and plots should be tidily reported and commented.             


# plot runs and distributions
plot(mcmc)

#convergence
gelman.plot(mcmc, confidence = 0.6) # burn-in 500
gelman.diag(mcmc, confidence = 0.6)
burnin = 500
# window function: iteration window for runs
mcmc_afther_burnin = window(mcmc, start = burnin + 1, end = niter);
gelman.diag(mcmc_afther_burnin, confidence = 0.6)
gelman.plot(mcmc_afther_burnin, confidence = 0.6)
# plot runs and distributions afther burn-in
plot(mcmc_afther_burnin)


# Batch Monte Carlo Standard Error
batchSE(mcmc_afther_burnin, batchSize = 100)
lapply(mcmc_afther_burnin, batchSE, batchSize = 100)


#autocorrelation
acfplot(mcmc_afther_burnin, outer = T, aspect = "fill");


#effective sample size
effectiveSize(mcmc_afther_burnin)
# ratio mcmc sample / independent sample
effectiveSize(mcmc_afther_burnin) / ((niter - burnin) * nchains)

lapply(mcmc_afther_burnin, effectiveSize)




# 2d) For each parameter, report in a table the posterior mean               
#     together with its 95% Credible Intervals (CIs).                         
#     Using the results in this table, provide a thorough interpretation     
#     of the estimated effect of the explanatory variables x and z            
#     on the response variable y and their uncertainty.                      

summary(mcmc_afther_burnin)



# 2e)                                                                        


# CASE A
model_code <- nimbleCode({
  
  # priors
  alpha ~ dunif(min_alpha, max_alpha)
  beta ~ dunif(min_beta, max_beta)
  gamma ~ dunif(min_gamma, max_gamma)
  variance ~ dunif(a, b)
  
  # likelihood
  for (i in seq_len(N)) { 
    predicted_y[i] <- alpha + beta * x[i] + gamma * z[i]
    y[i] ~ dnorm(predicted_y[i], sd = sqrt(variance))
  }
})

# vague priors with reasonable mean for the distributions, 
# but with a large standard deviation (sd) parameter
min_alpha <- -20;
max_alpha <- 20;
min_beta <- -100;
max_beta <- 100;
min_gamma <- -100; 
max_gamma <- 100;
a <- 0;
b <- 100;

# explained variable: time spent telephoning in a given day (minutes)
y <- c(19.9, 27.3, 21.0, 3.7, 48.4, 66.4, 13.1, 38.6, 22.7, 17.4, 40.7, 
       47.0, 53.7, 61.9, 80.8, 11.4, 16.2, 8.6, 32.6, 60.4)

# explanatory variable: number of telephone calls made by individual
x <- c(3, 6, 4, 1, 9, 15, 1, 7, 6, 2, 9, 12, 14, 17, 18, 3, 5, 2, 8, 12)

# explanatory variable: weekday / weekend
z <- c(1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)

# run MCMC
constants <- list(N = length(x), 
                  min_alpha = min_alpha, max_alpha = max_alpha,
                  min_beta = min_beta, max_beta = max_beta,
                  min_gamma = min_gamma, max_gamma = max_gamma,
                  a = a, b = b)

data <- list(y = y, x = x, z = z)

# assessing convergence to the stationary distribution 
# via the assessment of multiple replications starting from overdispersed starting points.
inits <- function() list(alpha = runif(1, min_alpha, max_alpha), 
                         beta = runif(1, min_beta, max_beta),
                         gamma = runif(1, min_gamma, max_gamma),
                         variance = runif(1, a, b))

# get MCMC samples (returned as coda mcmc object, ready to anlisys and plot
niter <- 2000
nchains <- 3
mcmc <- nimbleMCMC(code = model_code, 
                   constants = constants, 
                   data = data,
                   inits = inits,
                   niter = niter,
                   nchains = nchains,
                   setSeed = T, 
                   samplesAsCodaMCMC = TRUE)

# CASE B Bimodal normal mixture vagues priors
model_code <- nimbleCode({
  
  # hyperpriors
  n1_alpha ~ dnorm(0, sd = 50)
  n2_alpha ~ dnorm(100, sd = 50)
  n1_beta ~ dnorm(0, sd = 50)
  n2_beta ~ dnorm(100, sd = 50)
  n1_gamma ~ dnorm(0, sd = 50)
  n2_gamma ~ dnorm(100, sd = 50)
  
  # priors (mixture normal bimodal)
  alpha <- 0.5 * n1_alpha + 0.5 * n2_alpha
  beta <- 0.5 * n1_beta + 0.5 * n2_beta
  gamma <- 0.5 * n1_gamma + 0.5 * n2_gamma
  variance ~ dunif(0, 100)
  
  # likelihood
  for (i in seq_len(N)) { 
    predicted_y[i] <- alpha + beta * x[i] + gamma * z[i]
    y[i] ~ dnorm(predicted_y[i], sd = sqrt(variance))
  }
})

constants <- list(N = length(x))


# assessing convergence to the stationary distribution 
# via the assessment of multiple replications starting from overdispersed starting points.
inits <- function() list(n1_alpha = runif(1, 0, 100),
                         n1_beta = runif(1, 0, 100),
                         n1_gamma = runif(1, 0, 100),
                         n2_alpha = runif(1, 100, 100),
                         n2_beta = runif(1, 100, 100),
                         n2_gamma = runif(1, 100, 100),
                         alpha = runif(1, -100, 200), 
                         beta = runif(1, -100, 200),
                         gamma = runif(1, -100, 200),
                         variance = runif(1, 0, 100))

# get MCMC samples (returned as coda mcmc object, ready to anlisys and plot
niter <- 2000
nchains <- 3
mcmc <- nimbleMCMC(code = model_code, 
                   constants = constants, 
                   data = data,
                   monitors = c("alpha", "beta", "gamma", "variance"),
                   inits = inits,
                   niter = niter,
                   nchains = nchains,
                   setSeed = T, 
                   samplesAsCodaMCMC = TRUE)

# plot runs and distributions
plot(mcmc)

# window function: iteration window for runs
burnin  <- 500
mcmc_afther_burnin = window(mcmc, start = burnin + 1, end = niter);

# plot runs and distributions afther burn-in
plot(mcmc_afther_burnin)


summary(mcmc_afther_burnin)



##############################################################################
# 3)                                                                         #
##############################################################################

model_code <- nimbleCode({
  
  # priors
  alpha ~ dnorm(mu_alpha, sd = sigma_alpha)
  beta ~ dnorm(mu_beta, sd = sigma_beta)
  gamma ~ dnorm(mu_gamma, sd = sigma_gamma)
  variance ~ dinvgamma(a, b)
  
  # likelihood
  for (i in seq_len(N)) { 
    predicted_y[i] <- alpha + beta * x[i] + gamma * z[i]
    y[i] ~ dnorm(predicted_y[i], sd = sqrt(variance))
  }
  y16 <- alpha + beta * 16
})

# vague priors with reasonable mean for the distributions, 
# but with a large standard deviation (sd) parameter
mu_alpha <- 0;
sigma_alpha <- 10;
mu_beta <- 4;
sigma_beta <- 10;
mu_gamma <- 3; 
sigma_gamma <- 10;
a <- 3;
b <- 40;

# explained variable: time spent telephoning in a given day (minutes)
y <- c(19.9, 27.3, 21.0, 3.7, 48.4, 66.4, 13.1, 38.6, 22.7, 17.4, 40.7, 
       47.0, 53.7, 61.9, 80.8, 11.4, 16.2, 8.6, 32.6, 60.4)

# explanatory variable: number of telephone calls made by individual
x <- c(3, 6, 4, 1, 9, 15, 1, 7, 6, 2, 9, 12, 14, 17, 18, 3, 5, 2, 8, 12)

# explanatory variable: weekday / weekend
z <- c(1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)


# run MCMC
constants <- list(N = length(x), 
                  mu_alpha = mu_alpha, sigma_alpha = sigma_alpha,
                  mu_beta = mu_beta, sigma_beta = sigma_beta,
                  mu_gamma = mu_gamma, sigma_gamma = sigma_gamma,
                  a = a, b = b)
data <- list(y = y, x = x, z = z)


# assessing convergence to the stationary distribution 
# via the assessment of multiple replications starting from overdispersed starting points.
inits <- function() list(alpha = rnorm(1, mu_alpha, 3 * sigma_alpha), 
                         beta = rnorm(1, mu_beta, 3 * sigma_beta),
                         gamma = rnorm(1, mu_gamma, 3 * sigma_gamma),
                         variance = rinvgamma(1, a, b))

# get MCMC samples (returned as coda mcmc object, ready to anlisys and plot
niter <- 2000
nchains <- 3
mcmc <- nimbleMCMC(code = model_code, 
                   constants = constants, 
                   data = data,
                   monitors = "y16",
                   inits = inits,
                   niter = niter,
                   nchains = nchains,
                   setSeed = T, 
                   samplesAsCodaMCMC = TRUE)

plot(mcmc)

# window function: iteration window for runs
burnin  <- 500
mcmc_afther_burnin = window(mcmc, start = burnin + 1, end = niter);

# plot runs and distributions afther burn-in
plot(mcmc_afther_burnin)


samples = as.vector(do.call(rbind, mcmc_afther_burnin))
sum(samples < 60) / length(samples) * 100

summary(mcmc_afther_burnin, quantiles = 0.044)



'END'

