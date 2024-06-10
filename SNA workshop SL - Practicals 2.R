##################################
#     SNA workshop  2           #
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

# Inferential network analysis
# Whole network features


#########################
#                       #
#    Load packages      #
#                       #
#########################

# Loading igraph network object
#load("org_net.RData")

# function that allows you to load the data and name it
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
Net.1 <- loadRData("org_net.RData")

# Generating random networks - Erdos-Rényi-Gilbert model

# First, let's generate just one

set.seed(67789) # bc there are many possible networks, this ensures we get the same next time
ER1 <- erdos.renyi.game(
  gorder(Net.1), # N nodes
  gsize(Net.1), # N edges
  type = "gnm", # "gnp"
  directed = T,
  loops = FALSE
)
# Plot it
# png(filename="Random_graph.png", # I USE THIS ONE
#     width = 1400, height = 1400, units = "px")
# par(mar=c(0,0,0,0))
set.seed(980399)
plot(ER1, 
     vertex.frame.color = NA,
     vertex.size=5.3, 
     vertex.label=NA, 
     edge.arrow.size = 0.10,
     arrow.mode = "-",
     edge.color = "darkgray",
     vertex.color = "cyan4",
     #main = "Frendiships networks"
     layout = layout_nicely(ER1)) 
# dev.off()


# Compare with the observed network

# Network properties of random network

gsize(ER1) # N of links/edges/arcs
gorder(ER1) # N of nodes
round(edge_density(ER1), 2) # density; gden in sna
round(transitivity(ER1,
                   type = "global", # or "localaverage"
                   isolates = "NaN") # important
      , 2) # transitivity; gtrans in sna gives different results
round(reciprocity(ER1), 2) # reciprocity; grecip in sna 

########################
# Other network models
########################
# Small World network - Watts-Strogatz model

set.seed(743321)
SS1 <- sample_smallworld(1, # the dimension of the starting lattice
                         gorder(Net.1), #  the size of the lattice along each dimension.
                         3, # the neighborhood within which the vertices of the lattice will be connected
                         edge_density(Net.1)) # the rewiring probability
SS1 <- simplify(SS1)
gorder(SS1)
gsize(SS1)
edge_density(SS1)
round(transitivity(SS1,
                   type = "global", # or "localaverage"
                   isolates = "NaN"))
hist(degree(SS1))

mean_distance(SS1) # should be small, and it is - the characteristic of SW net

set.seed(980399)
plot(SS1, 
     vertex.frame.color = NA,
     vertex.size=5.3, 
     vertex.label=NA, 
     edge.arrow.size = 0.10,
     arrow.mode = "-",
     edge.color = "darkgray",
     vertex.color = "cyan4",
     #main = "Frendiships networks"
     layout = layout_nicely(SS1)) 

# Preferential attachment random graphs - Barabasi-Albert model

set.seed(667788)
PA1 <- sample_pa(
  n = gorder(Net.1),
  power = 1, # linear preferential attachment
  directed = TRUE
)

gorder(PA1)
gsize(PA1)
edge_density(PA1)
round(transitivity(PA1,
                   type = "global", # or "localaverage"
                   isolates = "NaN"))

hist(degree(PA1))

set.seed(980399)
plot(PA1, 
     vertex.frame.color = NA,
     vertex.size=5.3, 
     vertex.label=NA, 
     edge.arrow.size = 0.10,
     arrow.mode = "-",
     edge.color = "darkgray",
     vertex.color = "cyan4",
     #main = "Frendiships networks"
     layout = layout_nicely(PA1)) 

# Perseving degree sequence

# Degree sequence
outd <- degree(Net.1, mode = "out")
ind <- degree(Net.1, mode = "in")
sum(outd) == sum(ind)

set.seed(50002)
DG1 <- sample_degseq(
  out.deg = outd,
  in.deg = ind,
  method =  "simple.no.multiple" #"simple.no.multiple.uniform", "vl"
)

plot(DG1, 
     vertex.frame.color = NA,
     vertex.size=5.3, 
     vertex.label=NA, 
     edge.arrow.size = 0.10,
     arrow.mode = "-",
     edge.color = "darkgray",
     vertex.color = "cyan4",
     #main = "Frendiships networks"
     layout = layout_nicely(DG1)) 

# It is interesting to note that network is very different despite having the same
# degree sequence
# Main difference: it is connected!

###########################################################################
# Generating many Erdos-Renyi-Gilbert graphs and calculating transitivity
##########################################################################

# Network metric of interest: Transitivity

the_net <- Net.1
N_SIM <- 1000 # number of simulations

# observed transitivity
Otr <- round(transitivity(the_net, type = "global"),2) # observed
Otr 

rtr <- c() # here we save transitivity of random graphs
set.seed(75556)
for(y in 1:N_SIM){
  
  ER <- erdos.renyi.game(
    gorder(the_net), # size
    gsize(the_net),
    type = "gnm", # "gnp"
    directed = T,
    loops = FALSE
  )
  ytr <- round(transitivity(ER, type = "global"),2)
  
  rtr[y] <- ytr
  
}
val <- round(quantile(rtr, probs = c(.025, .5, .975)),2)

max(rtr)
min(rtr)
# Plot it
hist(rtr, breaks = 5, xlim = c(0.0,0.50), plot = T, main ="")
abline(v = Otr, col = 'red', lwd = 2)
abline(v = as.numeric(val[1]), col = "blue", lwd = 2, lty = 'dashed')
abline(v = as.numeric(val[3]), col = "blue", lwd = 2, lty = 'dashed')

##################################
# Conditional Uniform Graphs
##################################

# Network metric of interest: Transitivity

net_obj <- asNetwork(Net.1)
cugDyadT <- cug.test(net_obj,
                     FUN= gtrans, 
                     mode="digraph",   ### ah that was mistake befor
                     cmode="dyad.census", rep = 1000)

cugEdgesT <- cug.test(net_obj,
                      FUN= gtrans, 
                      mode="digraph", 
                      cmode="edges", rep = 1000)


cugSizeT <- cug.test(net_obj,
                     FUN= gtrans,
                     mode="digraph", 
                     cmode="size", rep = 1000)
# Plot it
plot(cugEdgesT) # edge count
plot(cugDyadT) # dyad census
plot(cugSizeT) # sizeonly here is much lower

# The end of the script

