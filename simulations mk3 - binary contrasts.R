################################################################################
#
# Sample size calculation based on Ben Bolker 
#
################################################################################


#library(devtools)
#install_github("lme4", user = "lme4")


library(lme4)
## Loading required package: Matrix
packageVersion("lme4")
## [1] '1.1.3'
library(plyr)
library(ggplot2)
theme_set(theme_bw())

#two treatments, 100 individuals, 5 clause types per individual per treatment.
N=100
expdat <- expand.grid(indiv = factor(1:N), clause = factor(1:5), ttt = c("animation","stills"))
expdat$clause <- factor(seq(nrow(expdat)))


################################################################################
#Model 1: contrast 1 and 2
################################################################################


set.seed(1981)
nsim <- 100
beta <- c(qlogis(0.9), 0.2)
theta <- c(0.1, 0.1)

ss <- simulate(~ttt + (1 | indiv) + (1 | clause), nsim = nsim, family = binomial, 
               weights = rep(25, nrow(expdat)), newdata = expdat, newparams = list(theta = theta, 
                                                                                   beta = beta))
expdat$resp <- ss[, 1]
fit1 <- glmer(resp ~ ttt + (1 | indiv) + (1 | clause), family = binomial, weights = rep(25, nrow(expdat)), data = expdat)

################################################################################
fit1B <- refit(fit1, ss[[2]])
fitsim <- function(i) {
  coef(summary(refit(fit1, ss[[i]])))["tttstills", ]
}

################################################################################

t1 <- system.time(fitAll <- lapply(seq(nsim), function(i) fitsim(i)))

## you can use .progress='text' to get a progress indicator ...
fitAll <- setNames(as.data.frame(fitAll), c("est", "stderr", "zval", "pval"))


###POWER####################

mean(unlist(fitAll)[1:100*4] < 0.05)

################################################################################
#Model 2: contrast 1 and 3
################################################################################


set.seed(1981)
nsim <- 100
beta <- c(qlogis(0.9), 0.4)
theta <- c(0.1, 0.1)

ss <- simulate(~ttt + (1 | indiv) + (1 | clause), nsim = nsim, family = binomial, 
               weights = rep(25, nrow(expdat)), newdata = expdat, newparams = list(theta = theta, 
                                                                                   beta = beta))
expdat$resp <- ss[, 1]
fit1 <- glmer(resp ~ ttt + (1 | indiv) + (1 | clause), family = binomial, weights = rep(25, nrow(expdat)), data = expdat)

################################################################################
fit1B <- refit(fit1, ss[[2]])
fitsim <- function(i) {
  coef(summary(refit(fit1, ss[[i]])))["tttstills", ]
}

################################################################################

t1 <- system.time(fitAll <- lapply(seq(nsim), function(i) fitsim(i)))

## you can use .progress='text' to get a progress indicator ...
fitAll <- setNames(as.data.frame(fitAll), c("est", "stderr", "zval", "pval"))


###POWER####################

mean(unlist(fitAll)[1:100*4] < 0.05)

ggplot(fitAll, aes(x = est)) + geom_histogram() + geom_vline(xintercept = -0.2, 
                                                             colour = "red")



################################################################################
#Model 3: contrast 2 and 3
################################################################################


set.seed(1981)
nsim <- 50
beta <- c(qlogis(0.9), -0.1)
theta <- c(0.1, 0.1)

ss <- simulate(~ttt + (1 | indiv) + (1 | clause), nsim = nsim, family = binomial, 
               weights = rep(25, nrow(expdat)), newdata = expdat, newparams = list(theta = theta, 
                                                                                   beta = beta))
expdat$resp <- ss[, 1]
fit1 <- glmer(resp ~ ttt + (1 | indiv) + (1 | clause), family = binomial, weights = rep(25, nrow(expdat)), data = expdat)

################################################################################
fit1B <- refit(fit1, ss[[2]])
fitsim <- function(i) {
  coef(summary(refit(fit1, ss[[i]])))["tttstills", ]
}

################################################################################

t1 <- system.time(fitAll <- lapply(seq(nsim), function(i) fitsim(i)))

## you can use .progress='text' to get a progress indicator ...
fitAll <- setNames(as.data.frame(fitAll), c("est", "stderr", "zval", "pval"))


###POWER####################

mean(unlist(fitAll)[1:20*4] < 0.05)


