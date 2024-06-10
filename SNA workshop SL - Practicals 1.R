##################################
#     SNA workshop  1           #
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

# Example of descriptive network analysis



#########################
#                       #
#    Load packages      #
#                       #
#########################

# or install.packages 

# network-related packages

library(igraph) # main networks package
library(statnet) # other network package
library(sna) # other network package
library(intergarph) # for transforming network objects for different packages
library(netseg) # for homophily measures

# other packages
library(readxl) # for reading & writing csv or excel data
library(openxlsx) # for reading & writing csv or excel data
library(dplyr)

#########################
#                       #
#     Load functions    #
#                       #
#########################

#source()

# Function transforming directed network to undirected with the "weak rule"
Syms<-function(Z){
  ad <- as_adj(Z)
  adm <- as.matrix(ad)
  ADJ1<- as.data.frame(adm)
  
  
  unmat <- matrix(nrow = vcount(Z), ncol = vcount(Z))
  
  for (i in 1:vcount(Z)) {
    for (j in 1:vcount(Z)) {
      
      if(ADJ1[i,j] != 0| ADJ1[j,i]!=0 ){
        unmat[i,j] <- 1 
        unmat[j,i] <- 1
      }else{
        unmat[i,j] <- 0 
        unmat[j,i] <- 0
      }
      
    }
  }
  rownames(unmat) <- rownames(ADJ1)
  colnames(unmat) <- colnames(ADJ1)
  
  
  GX <- graph_from_adjacency_matrix(unmat, mode = "undirected")
  return(GX)
  
}

#########################
#                       #
#     Main Script       #
#                       #
#########################

getwd() # where are you rading from and saving data?
#setwd() # if you need to change it



# Reading in network data saved as an adjacency matrix in a dataframe
dfnet <-read.xlsx("Network_data.xlsx")
head(dfnet)
str(dfnet)
dim(dfnet)

# Create network
sx1 <- subset(dfnet, select = c(sender, receiver))
edgeY <- sx1


colnames(edgeY) <- c("respondent_id", "alter")
edgecleanY <- edgeY[which(!is.na(edgeY$alter)),] 
# remove duplicate rows - multiple edges
edgecleanY <- edgecleanY[!duplicated(edgecleanY), ]
# remove rows with equal value in the columns - self-loops
edgecleanY <- edgecleanY[(!(edgecleanY$respondent_id == edgecleanY$alter)),]
# finally
Net.1 <-graph_from_data_frame(edgecleanY, directed = TRUE, vertices = NULL) 

# write here how and it gets to igraph object

class(Net.1)
summary(Net.1) # it is directed and some other info

# Important checks:
# Are there loops in the network?
sum(which_loop(Net.1)) # needs to be 0

# Are there multiple ties in the network?
is_simple(Net.1) # needs to be TRUE

# Is it weighted?
is_weighted(Net.1) # in our case needs to be FALSE

# Are there more components?
is_connected(Net.1)

# Other network formats

# as matrix
Mat.1 <- as_adjacency_matrix(Net.1, type= "both",
                             names = T)
Mat.1 
class(Mat.1)
MAT.1 <- as.matrix(Mat.1)
MAT.1
sum(MAT.1)

#######################################
# Visualize the directed network
######################################

# if you want to save it unhash png and dev.off functions

# png(filename="Directed network.png", # chose a name
#     width = 1400, height = 1400, units = "px")

set.seed(980399) # set seeds
par(mar=c(0,0,0,0)) # fix margins
plot(Net.1, 
     vertex.frame.color = NA, # the color of node's edge
     vertex.size=5.3, # node size, all have the same size
     vertex.label=NA, # set to no names
     edge.arrow.size = 0.40, # size of the arrow
     arrow.mode = "-",
     edge.color = "darkgray",
     vertex.color = "darkorchid", # node color, all have the same color
     #main = "Friendships networks"  # add title if you want
     layout = layout_nicely(Net.1)) # have a nice layout 
# dev.off()

######################################################
# The most basic network properties: observed network
######################################################

gsize(Net.1) # N of links/edges/arcs
gorder(Net.1) # N of nodes
round(edge_density(Net.1), 2) # density; gden in sna
round(transitivity(Net.1,
                   type = "global", # or "localaverage"
                   isolates = "NaN") # important
      , 2) # transitivity; gtrans in sna gives different results
round(reciprocity(Net.1), 2) # reciprocity; grecip in sna 


# Number of network components
components(Net.1)
Nc <- components(Net.1)

# Percentage of nodes in the giant component
Nc$membership
table(Nc$membership)

Nc$csize

Prct_giant <- round(Nc$csize[1]/gorder(Net.1)*100, 1)
Prct_giant


# Looking at network configurations

# Dyads
dyad_census(Net.1) # looking into 3 types of dyads

# Triads
triad_census(Net.1) #  -II-       16 types of triads

# The mean degree and degree distributions
dev.off()
hist(degree(Net.1))
min(degree(Net.1))
max(degree(Net.1))
round(mean(degree(Net.1)), 2) # total by default
round(mean(degree(Net.1, mode = "in")), 2)
round(mean(degree(Net.1, mode = "out")), 2) # the average will always be the same as for in-coming ties

# Number of isolates
sum(degree(Net.1)==0)

# Notice: No isolates!

# Transform directed to undirected network (weak" rule)

# Why we do this? - To make simpler examples, but you do not want to do this
# in your research, bc it is a loss of information

Net.1.und <- Syms(Net.1)

# Node neighborhood 

neighborhood(Net.1, order = 1)
neighborhood_size(Net.1, order = 1)
degree(Net.1, mode = "all")
make_ego_graph(Net.1, order = 1)
make_ego_graph(Net.1, order = 1)

# pick one node
V(Net.1)$name
all_nodes <- V(Net.1)$name
all_nodes[22] # this is the node we picked

neighborhood(Net.1, order = 1, 
             nodes = all_nodes[22])

# Make ego-graph for that node
eg1 <- make_ego_graph(Net.1, 
                      order = 1,
                      nodes = all_nodes[22])
eg1 
eg1 <- eg1[[1]]

V(eg1)$name 
vec_color <- ifelse(V(eg1)$name == all_nodes[22], 1, 2) # color ego node differently

plot(eg1, 
     vertex.size = 8,
     #vertex.label = NA,
     edge.arrow.size = 0.1, # size of the arrow
     arrow.mode = "-",
     edge.color = "darkgray",
     vertex.color = vec_color) # we color the ego in different color
# Distances
distances(Net.1)
distances(Net.1.und)

# Centrality measures

# We looked at degree above
betweenness(Net.1,
            directed = T,
            normalized = F) # better to use T
# problematic scale

closeness(Net.1, 
          mode = "all",
          normalized = T)

# Brokerage of nodes
constraint(Net.1) 
# higher value --> more constrained
# lower value --> structural holes (relative to others in the network)


# Other network level measures

# Centralization in degree


Cd <- centr_degree(Net.1,
             mode = "all",
             normalized = T)

Cd$res # node level results: higher scores - more central
degree(Net.1, mode = "all")
Cd$centralization
cent_net <- round(Cd$centralization, 2)
Cd$max

# Centralization in betweenness

Cb <- centr_betw(Net.1,
                   #mode = "all",
                   normalized = T)
Cb

# Centralization in closeness
centr_clo(Net.1,
          mode = "all",
          normalized = T)

#################################
# Community detection algorithms
################################

# Five examples

# edge_betweenness
Ceb <- cluster_edge_betweenness(Net.1, 
                                weights = NULL,
                                directed = T)
Ceb
Mod1 <- max(round(Ceb$modularity,2)) # modularity
Mod1

Ceb$membership
Ng1 <- length(unique(Ceb$membership)) # N of groups
Ng1
table(Ceb$membership)

# Plot it

# one way
plot(Ceb,
     Net.1,
     vertex.frame.color = NA,
     vertex.size = 4,
     vertex.label=NA, # set to no names
     edge.arrow.size = 0.1, # size of the arrow
     arrow.mode = "-",
     edge.color = "darkgray")
# another way
plot(Net.1,
     vertex.frame.color = NA,
     vertex.size = 4,
     vertex.label=NA, # set to no names
     edge.arrow.size = 0.1, # size of the arrow
     arrow.mode = "-",
     edge.color = "darkgray",
     vertex.color = Ceb$membership) # use membership vector to color the nodes

# Other community detection algorithms 

# cluster_infomap
CI <- cluster_infomap(Net.1)
CI
Mod3 <- max(round(CI$modularity,2)) # modularity
Mod3

CI$membership
Ng3 <- length(unique(CI$membership)) # N of groups
Ng3
table(CI$membership)

plot(CI,
     Net.1,
     vertex.frame.color = NA,
     vertex.size = 4,
     vertex.label=NA, # set to no names
     edge.arrow.size = 0.1, # size of the arrow
     arrow.mode = "-",
     edge.color = "darkgray")

plot(Net.1,
     vertex.frame.color = NA,
     vertex.size = 4,
     vertex.label=NA, # set to no names
     edge.arrow.size = 0.1, # size of the arrow
     arrow.mode = "-",
     edge.color = "darkgray",
     vertex.color = CI$membership)

# Etc... we can skip the rest

# cluster_walktrap
CW <- cluster_walktrap(Net.1, steps=4)
CW
Mod2 <- max(round(CW$modularity,2)) # modularity
Mod2

CW$membership
Ng2 <- length(unique(CW$membership)) # N of groups
Ng2
table(CW$membership) # N of nodes per group


# cluster_louvain
# set.seeds
set.seed(98983)
CL <- cluster_louvain(Net.1) # for undirected graphs only

set.seed(98983)
CL <- cluster_louvain(Net.1.und,
                      resolution = 1)

# Saving an igraph network object
save(Net.1, file = "org_net.RData") # directed
save(Net.1.und, file = "org_net_und.RData")

# The end of the script




