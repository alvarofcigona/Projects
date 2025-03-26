library(statsecol)
data("wildebeest")
wildebeest

sink("ibexSSM.txt")
cat("
model{
  n_1 ~ dnorm(0.263, 1/10)
  N[1] <- n_1
  beta_0 ~ dunif(-10,10)
  beta_1 ~ dunif(-10,10)
  lambda_sig ~ dunif(0,10)
  lambda_tau <- pow(lambda_sig, -2)
  
  # Likelihood - State process
  for (t in 1:(nyrs-1)){
    lambda_mu[t] <- beta_0 + beta_1*rain[t]
    lambda[t] ~ dnorm(lambda_mu[t], lambda_tau)
    N[t+1] ~ dpois(lambda[t]*N[t])
    N[t+1] <- N[t+1] - harvest[t+1]
  }
  
  # Likelihood - Observation process
  for(t in valid_rows){
    y[t] ~ dnorm(N[t], 1/sehat[t])
  }
}
",fill = TRUE)
sink()

# finding the rows that have NAs
valid_rows <- which(is.na(wildebeest$Nhat)==FALSE)

wildebeest_data <- list(y = wildebeest$Nhat,
                        rain = wildebeest$rain,
                        sehat = wildebeest$sehat,
                        c = wildebeest$Catch,
                        nyrs = nrow(wildebeest),
                        valid_rows = valid_rows)


inits <- function(){
  list(beta_0 = 0,
       beta_1 = 0.1,
       lamba_sig = 1,
       N = 0.263
       )
}

param_monitor <- c(


