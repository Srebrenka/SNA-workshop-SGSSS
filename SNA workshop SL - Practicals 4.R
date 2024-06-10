
##################################
#     SNA workshop  4           #
#################################

#rm(list = ls())

#################
#               #
#      Notes    #
#               #
#################

# Srebrenka Letina developed this script
# May 2024


#############
#  Purpose  #
#############

# Example of ERGMs, very introductory - only the most simple models!



#########################
#                       #
#    Load packages      #
#                       #
#########################

library(intergraph)
library(statnet)
library(ergm)

# Covert igraph object to sna object

NETk <- try2

net_obj <-asNetwork(NETk)

summary(sNETk)

# Plotting in statnet
dev.off()
plot(net_obj, 
     vertex.col = "tomato", 
     vertex.cex = 1)


# Building ERGMs
?ergm.terms
# look here: https://cran.r-project.org/web/packages/ergm/vignettes/ergm-term-crossRef.html#absdiff-ergmTerm-9fc8de5a

random_graph <- ergm(net_obj ~ edges, control = control.ergm(seed = 1234))
summary(random_graph)

# How do we interpret this coefficient? Coefficients in ERGMs represent the 
# changein the (log-odds) likelihood of a tie for a unit change in a predictor. 
# We can use a simple formula for converting log-odds to probability to 
# understand them better.


inv.logit <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

theta <- coef(random_graph)
inv.logit(theta)

network.density(sNETk) # it will the same (or very similar) as density

# We can simulate graphs using our ERGM fit

set.seed(1234)
hundred_simulations <- simulate(random_graph, 
                                coef = theta,
                                nsim = 100,
                                control = control.simulate.ergm(MCMC.burnin = 1000,
                                                                MCMC.interval = 1000))

# Let's examine the first nine simulations.

# graphics.off()
png(filename="some_simulated_random_networks.png", # chose a name
    width = 700, height = 700, units = "px")
par(mfrow = c(3, 3))
sapply(hundred_simulations[1:9], plot, vertex.cex = 1, vertex.col = "tomato")
dev.off()

# We can compare all simulations with obesrved value of density

png(filename="simulations_random_model.png", # chose a name
    width = 1000, height = 500, units = "px")
net_densities <- unlist(lapply(hundred_simulations, network.density))

hist(net_densities, xlab = "Density", main = "", col = "lightgray")
abline(v = network.density(sNETk), col = "red", lwd = 3, lty = 2)
abline(v = mean(net_densities), col = "blue", lwd = 3, lty = 1)
dev.off()


# Goodness of fit

gof_stats <- gof(random_graph)

par(mfrow = c(2, 3))
plot(gof_stats, main = '')

# How to we improve our fit? 
# By adding more terms to the model!

model1 <- ergm(net_obj ~ edges + 
                 nodematch("Gender") ) # tendency of ppl of the same gender to form a tie

summary(model1)

# Adding some dyad-dependent terms.---> will need more time to run!

model2 <- ergm(net_obj ~ edges + 
                 nodematch("Gender") + 
                 mutual) # reciprocity

summary(model2)

gof_stats2 <- gof(model2)

par(mfrow = c(2, 3))
plot(gof_stats2, main = '')

# Adding overall job satisfaction

model3 <- ergm(net_obj ~ edges + 
                 nodematch("Gender") + 
                 mutual + # reciprocity
                 absdiff("overall_job_satisfaction", pow=1)) # to check homophily in OBJ

summary(model3)

gof_stats3 <- gof(model3)

par(mfrow = c(2, 3))
plot(gof_stats3, main = '')


# How good is the fit? 

# Triadic closure is not modeled! - so not likely to be good.


# The following models take more time to run bc they include more complex configurations
# meaning, configurations that have more than2 nodes (3)
# Try to run them as a homework, if you wish.
# They may not converge!! Actually they are unlikely to converge due to
# network properties that are not good for running ERGMs.

# The end of the script.
##############################################################################

# The rest is only optional.

# Addding a term for triadic closure. 
# There are a few terms for triads - 
# one of them, triangles, tends to lead to degeneracy. 
# The gwesp term behaves better, but convergence is not guaranteed.

# This will take a while!


model4 <- ergm(net_obj ~ edges + 
                 nodematch("Gender") + 
                 mutual +
                 gwesp(0.4, fixed = T), # decay parameter is set to high due to high transitivity
               control=control.ergm(MCMLE.maxit= 40))
# you can change a number of other things about the MCMC algorithm - from its burn-in to its step and sample size
# here we just up the maximum iterations we wait to see if it has converged

summary(model4)

# Goodness of fit

model4_gof <- gof(model4)



png(filename="gof_model4.png", # chose a name
    width = 1000, height = 1000, units = "px")
par(mfrow = c(3, 2))
plot(model4_gof, main = '')
dev.off()

# We next add homophily in mental health...
model5 <- ergm(net_obj ~ edges + 
                 nodematch("Gender") + 
                 mutual +
                 gwesp(0.4, fixed = T) +
                 absdiff("overall_job_satisfaction", pow=1),
               control=control.ergm(MCMLE.maxit= 40))

# Adding other parameters...