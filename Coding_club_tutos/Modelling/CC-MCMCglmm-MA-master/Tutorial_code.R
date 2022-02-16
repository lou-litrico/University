# Bayesian stats CC tutorial script #

# Libraries ----
library("MCMCglmm") # for meta-analysis
library("dplyr") # for data manipulation

# Data ----
migrationdata <- read.csv("migration_metadata.csv", header = T) 
View(migrationdata)  # the dataset has two predictor variables = time and temperature

# Time ----
migrationtime <- migrationdata %>%
  filter(Predictor == "year") # select only time

plot(migrationtime$Slope, I(1/migrationtime$SE))  # plot data slope = (rate of change in days/year) and precision (1/SE)
# data centered around 0 + good distribution in negative and positives 
plot(migrationtime$Slope, I(1/migrationtime$SE), xlim = c(-2,2), ylim = c(0, 60))  # zoom in

# Random effect model (time) ----
randomtest <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study, data = migrationtime)
summary(randomtest)

# summary outputs = posterior mean for each effect 
# + lower and upper 95% credible intervals of the distribution for each effect 
# + effective sample size for each random effect 
# + a pMCMC value only for the fixed effect 
# effective sample size should be high (1000 to 2000)
# credible intervals should not include 0 so we can say fixed effect is significant
# the narrower the interval, the more precise the estimate of the effect is 
# for random effects, we estimate the variance = effect is significant when variance histogram is not pushed against 0 
# the larger the spread, the less precise the estimation is 

par(mfrow = c(1,3))

hist(mcmc(randomtest$VCV)[,"Study"])
hist(mcmc(randomtest$VCV)[,"Location"])
hist(mcmc(randomtest$VCV)[,"Species"])

par(mfrow=c(1,1)) # Reset the plot panel back to single plots

# here the histograms are pushed against 0 for Location and Species so no random effects from those 

plot(randomtest$Sol)  # look at model convergence for fixed 
# trace plot should look like fizzy caterpillar = looks good here so model converged well
# if we think it's not good, we can increase the nb of iterations (nitt = 60000)
# OR increase the burn in (nb of iterations discounted bc not accurate at the beginning) with (burnin = 5000)
# OR increase the thinning interval (thin = 30)
# OR use a stronger prior 

plot(randomtest$VCV)  # same for random effects 

# Temperature ----
migrationtemp <- migrationdata %>%
  filter(Predictor == "temperature") # select only T°

plot(migrationtemp$Slope, I(1/migrationtemp$SE))

# Random effect model (T°) ----
randomtest2 <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study, data = migrationtemp)
summary(randomtest)  # Why is the effective sample size so low this time??? + do we look only at the fixed effect one? 

# Priors ----
a <- 1000
prior1 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*a)))
# new model with new prior and increased nitt 
randomprior <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study, 
                        data = migrationtime, prior = prior1, nitt = 60000)
summary(randomprior)
plot(randomprior$VCV)

# second try for different prior
prior2 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*a),
                        G1 = list(V = diag(1), fix = 1)))

randomerror2 <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study + idh(SE):units, 
                         data = migrationtime, prior = prior2, nitt = 60000)

plot(randomerror2$VCV)
