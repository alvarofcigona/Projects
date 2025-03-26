library(statsecol)
library(jagsUI)
library(MCMCvis)
library(ggplot2)

###GENERATING THE DATA
#study parameters
set.seed(123)
n.sites <- 27   
n.years <- 15   
n.visits <- 2   

#model parameters

prw <- 0.7     #Pr Occupancy in countries with ban on the hunting of wolves
prwhunt <- 0.4  #Pr Occupany in countries with hunting ban lifted
gamma <- 0.4   #Pr colonization
epsilon <- 0.6  #Pr extention
p <- 0.4       #Pr detection

#storage objects
z1 <- array(NA, dim=c(n.sites, n.years))
z2 <- array(NA, dim=c(n.sites, n.years))
y <- array(NA, dim=c(n.sites, n.years))

#initial states
z1[,1] <- rbinom(n=n.sites, size = 1, prob = prw)
z2[,1] <- rbinom(n=n.sites, size = 1, prob = prwhunt)
#subsequent states z1
for(t in 2:n.years){
  expected.prw <- (1-z1[,t-1])*gamma + z1[,t-1]*(1-epsilon) 
  z1[,t] <- rbinom(n=n.sites, size = 1, prob = expected.prw) 
}
for(t in 2:n.years){
  expected.prwhunt <- (1-z1[,t-1])*gamma + z2[,t-1]*(1-epsilon) 
  z2[,t] <- rbinom(n=n.sites, size = 1, prob = expected.prwhunt) 
}
#putting together z
z <-rbind(z1, z2)


#detection-nondetection data
for(t in 1:n.years){
  y[,t] <- rbinom(n=n.sites, size = n.visits, p*z[,t])
}


true.occ <- apply(z,2,mean)
naive.occ <- apply(y>0,2,mean)
years <- 1:n.years
plot(true.occ ~ years, lwd=2, col="blue", las = 1, ylim=c(0,1), 
     ylab="Proportion Occupied", xlab = "Year", type="o",pch=16)
lines(naive.occ ~ years, lwd=2, col="red",type="o",pch=16)
legend("topright",c("Truth","Naive"), lwd=2, col=c("blue","red"), bty="n")



###Model
sink("wolvesSSM.txt")
cat("
model{
  # Priors and constraints
  prw ~ dunif(0, 1)    
  prwhunt ~ dunif(0, 1)
  gamma ~ dunif(0, 1)   
  epsilon ~ dunif(0, 1) 
  p ~ dunif(0, 1)       

  # Likelihood - State process
  for(i in 1:n.sites){
    z[i,1] ~ dbern(prw) #initial
    for(t in 2:n.years){ #subsequent
      z[i,t] ~ dbern((1 - z[i,t-1])*gamma + z[i,t-1]*(1-epsilon))
    }
  }
  
  # Likelihood - Observation process
  for(i in 1:n.sites){
    for(t in 1:n.years){
      y[i,t] ~ dbin(p*z[i,t],n.visits)
      yobs[i,t] <- y[i,t]                                   #unneccessary, but for completeness
      ysim[i,t] ~ dbin(p*z[i,t],n.visits)                   #simulate new counts from the model
      yexp[i,t] <- p*n.visits*z[i,t] +0.001                 #expected counts under the model
      
      # Observation level discrepency: [(o - e)^2 / e]
      disc.obs[i,t] <- pow((yobs[i,t] - yexp[i,t]),2)/(yexp[i,t]) # for observed data
      disc.sim[i,t] <- pow((ysim[i,t] - yexp[i,t]),2)/(yexp[i,t]) # for 'ideal' data
    }
  }
  
  #sum discrepencies for Ch-Sq statistic
  chi2.obs <- sum(disc.obs[,])
  chi2.sim <- sum(disc.sim[,])
  chi2.ratio <- chi2.obs/chi2.sim
}


",fill = TRUE)
sink()

# Bundle the data
wolvesdata <- list(n.sites = n.sites,
                  n.years = n.years,
                  n.visits = n.visits,
                  y = y)

# Initial values function
wolvesinits <- function(){
  list(prw = runif(1,0,1),
       gamma = runif(1,0,1),
       epsilon = runif(1,0,1),
       z = ifelse(y>0,1,0)) 
}

# Parameters to monitor
wolvesparms <- c("prw","gamma","epsilon","p", "chi2.obs","chi2.sim","chi2.ratio")

# MCMC settings
nc <- 3
nb <- 1000
ni <- 5000 + nb
nt <- 1

wolvesout <- jags(data = wolvesdata,
                 inits = wolvesinits,
                 parameters.to.save = wolvesparms,
                 model.file = "wolvesSSM.txt",
                 n.chains = nc,
                 n.iter = ni,
                 n.burnin = nb,
                 n.thin = nt)

MCMCtrace(wolvesout,                
          params = wolvesparms[1:4],
          type = "trace",
          iter = ni,  
          pdf = FALSE)

MCMCtrace(wolvesout,                
          params = wolvesparms[1:4],
          type = "density",
          iter = ni,  
          pdf = FALSE,
          ind = FALSE)

MCMCsummary(wolvesout,
            params = wolvesparms[1:4],
            digits = 2)


fill = TRUE
sink()

wolvesparmsgof <- c("prw","gamma","epsilon","p", "chi2.obs","chi2.sim","chi2.ratio")

wolvesoutgof <- jags(data = wolvesdata,
                    inits = wolvesinits,
                    parameters.to.save = wolvesparmsgof,
                    model.file = "wolvesSSM.txt",
                    n.chains = nc,
                    n.iter = ni,
                    n.burnin = nb,
                    n.thin = nt)
 

#Graphs
obs <- wolvesout$sims.list$chi2.obs
sim <- wolvesout$sims.list$chi2.sim
ratio <- wolvesout$sims.list$chi2.ratio


plot(sim~obs,col=adjustcolor(ifelse(ratio<1, "maroon","darkgreen"),0.05), pch=16, asp=1, 
     xlab="Observed distribution", ylab="Simulated distribution")
abline(0,1, lwd=2)
title("Observed vs Simulated Distribution")

hist(ratio, col=adjustcolor("darkblue",0.2), main="Observed vs Simulated ")
abline(v=1, lwd=3, col="red")

mean(obs>sim)

wolvesparms1 <- c("prw","p", "chi2.obs","chi2.sim","chi2.ratio")
MCMCtrace(wolvesout,                
          params = wolvesparms1,   
          iter = ni,                 
          pdf = FALSE,               
          type = "trace")  