##################################
#     SNA workshop  3           #
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
# Dyadic and node attribute questions
# Homophily


#########################
#                       #
#    Load packages      #
#                       #
#########################

#################################################
# Adding node attributes
##############################################

# Read in attribute data

filenamex = "Attribute_data.xlsx"
datax <- read.xlsx(filenamex)

head(datax)
str(datax)

# Add Gender attribute by matching node's id from metwork to node's id in dataframe

indata <- datax

matchgen1 <- indata[indata$name %in% V(Net.1)$name,]
matchgen1 <- matchgen1[match(V(Net.1)$name, matchgen1$name), ]

ATT = "Gender"

NET <- igraph::set_vertex_attr(Net.1, ATT, 
                               index = V(Net.1), matchgen1[, ATT])
V(NET)$Gender
Nna <- sum(is.na(V(NET)$Gender)) # 
Nna

Nna/gorder(Net.1)

V(NET)$Gender
# Identify vertices with missing attribute
vertices_to_delete <- V(NET)[is.na(V(NET)$Gender)] # 5
vertices_to_delete 
# Delete the identified vertices
NETk <- delete_vertices(NET, vertices_to_delete)
round(prop.table(table(V(NETk)$Gender))*100, 1)

# men women 
# 77.8  22.2


# Add Job satisfaction attribute
colnames(indata)
ATT = "overall_job_satisfaction"
matchgen1 <- indata[indata$name %in% V(NETk)$name,]
matchgen1 <- matchgen1[match(V(NETk)$name, matchgen1$name), ]
NETk <- igraph::set_vertex_attr(NETk, ATT, 
                                index = V(NETk), matchgen1[, ATT])
V(NETk)$overall_job_satisfaction

round(prop.table(table(V(NETk)$overall_job_satisfaction))*100, 1)

hist(V(NETk)$overall_job_satisfaction)

vertex.attributes(NETk)

# Convert to dataframe if you want ...


#######################################
# Visualize graph with node attributes
######################################

# Gender 
# Note: Data collected in 2006 - no info on gender identity, just gender
# Gender

V(NETk)$color <- ifelse(V(NETk)$Gender == "women", "darkolivegreen4", "goldenrod3")


# png(filename="Gender_net.png", 
#     width = 1400, height = 1400, units = "px")
set.seed(980399)
par(mar=c(0,0,0,0))
plot(NETk, 
     vertex.frame.color = NA,
     vertex.size=5.3, 
     vertex.label=NA, 
     edge.arrow.size = 0.10,
     arrow.mode = "-",
     edge.color = "grey50",
     vertex.color =  V(NETk)$color,
     #main = "Frendiships networks"
     layout = layout_nicely(NETk))  
# dev.off()

# Net OBJ cont and gender

# png(filename="OBJcont_Gender_net.png", 
#     width = 1400, height = 1400, units = "px")
set.seed(980399)
par(mar=c(0,0,0,0))
plot(NETk, 
     vertex.frame.color = NA,
     vertex.size=V(NETk)$overall_job_satisfaction*1.2, # bigger node - more satisfied 
     vertex.label=NA, 
     edge.arrow.size = 0.20,
     arrow.mode = "-",
     edge.color =  "grey50",
     vertex.color =  V(NETk)$color,
     #main = "Frendiships networks"
     layout = layout_nicely(NETk))  
#dev.off()

# Just overall_job_satisfaction

# png(filename="OBJcont__net.png", 
#     width = 1400, height = 1400, units = "px")
set.seed(980399)
par(mar=c(0,0,0,0))
plot(NETk, 
     vertex.frame.color = NA,
     vertex.size=V(NETk)$overall_job_satisfaction*1.2, 
     vertex.label=NA, 
     edge.arrow.size = 0.20,
     arrow.mode = "-",
     edge.color =  "grey50",
     vertex.color =  "darkorchid",
     #main = "Frendiships networks"
     layout = layout_nicely(NETk))  
#dev.off()

# Gender and a centrality measure

# We are looking at indegree centrality

indeg_cent <- degree(NETk, mode = "in", normalized = F)

id_df <- as.data.frame(indeg_cent)
id_df$indeg_cent

set.seed(980399)
par(mar=c(0,0,0,0))
plot(NETk, 
     vertex.frame.color = NA,
     vertex.size=id_df$indeg_cent + 1.5, # bigger node - more satisfied 
     vertex.label=NA, 
     edge.arrow.size = 0.20,
     arrow.mode = "-",
     edge.color =  "grey50",
     vertex.color =  V(NETk)$color,
     #main = "Frendiships networks"
     layout = layout_nicely(NETk))  

# Men obviously have more in-coming ties

# How to add a centrality measure as a node attribute?

V(NETk)$indegree <- id_df$indeg_cent

vertex.attributes(NETk)

########################
# Homophily measures
########################

# For binary variable (gender, as measured)
EIg <- round(ei(NETk, directed = F, "Gender"),2)
EIg

# For continuous variable (overall job satisfaction)
AssorD <- assortativity(NETk,
                       directed = T,
                       values = V(NETk)$overall_job_satisfaction)

AssorD

AssorUND <- assortativity(NETk,
                        directed = F,
                        values = V(NETk)$overall_job_satisfaction)

AssorUND # in our network, lower when not taking the direction of tie into account

# in netseg pckg
assort(NETk, "overall_job_satisfaction") # different measure

################################
# Network permutation test
###############################

OJS <- V(NETk)$overall_job_satisfaction
the_net <- NETk
Real_A <- round(assortativity(NETk, OJS, directed = T),2)

N_PER <- 1000


AS <- c()
set.seed(44225)
for(i in 1:N_PER){
  OJSper <- sample(OJS, length(OJS), replace = F)
  V(the_net)$OJSper <- OJSper
  AS[i] <- round(assortativity(the_net, OJSper, directed = T),2)
}

valA <- round(quantile(AS, probs = c(.025, .5, .975)),2)

max(AS)
min(AS)
Real_A

hist(AS, breaks = 20, xlim = c(-0.30 , 0.40)
     , plot = T, main ="")
abline(v = Real_A, col = 'red', lwd = 2)
abline(v = as.numeric(valA[1]), col = "blue", lwd = 2, lty = 'dashed')
abline(v = as.numeric(valA[3]), col = "blue", lwd = 2, lty = 'dashed')

#######
# QAP
######

# We need matrices as DV and IVs
# DV
# directed
m1 <- as_adjacency_matrix(NETk)
m1 <- as.matrix(m1)
# undirected - if you want to use it later
NETkund <- Syms(NETk)
m1und <- as_adjacency_matrix(NETkund)
m1und <- as.matrix(m1und)

# IVs
OBJ <- V(NETk)$overall_job_satisfaction
hist(OBJ)

# Disimilarity in OBJ
OBJ_diff_matrix <- outer(OBJ, OBJ, function(x, y) abs(x - y))

DISimilarityOBJ <- OBJ_diff_matrix

# Gender similarity
GenBIN <- ifelse(V(NETk)$Gender == "women", 1, 0)
D4 <- dist(x = GenBIN, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
D5 <- as.dist(D4,  diag = FALSE)
D6 <- as.matrix(D5)
# transform it so that 1 means similarity
SimilarityGender <- ifelse(D6 == 1, 0, 1)

# Number of shared contacts matrix
n <- vcount(NETk)
shared_con <- matrix(0, n, n)

# Fill the matrix
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    shared_neighbors <- unique(neighbors(NETk, i), neighbors(NETk, j))
    nA <- length(shared_neighbors)
    if(nA ==0){
      shared_con[i,j] <-0
      shared_con[j,i] <-0
    }
    else{
      shared_con[i,j] <-nA
      shared_con[j,i] <-nA
    }
    
  }
}


shared_con
sum(shared_con)


# Check for multi-collinearity

Cs <- c("DISimilarityOBJ", "SimilarityGender", "shared_con", "Net_matrix")
cdf <- as.data.frame(cbind(as.vector(DISimilarityOBJ), as.vector(SimilarityGender), 
                           as.vector(shared_con), 
                           as.vector(m1)))
colnames(cdf) <- Cs
cmat <- round(cor(cdf), 2)
cmat

# for binary DV, logistic:

# Model 1
M1 <- netlogit(m1, DISimilarityOBJ, intercept=TRUE, mode="digraph", 
                 diag=F,
                 nullhyp="qapspp", # better to use this one
                 reps=500) # the bigger N, more stable results !

# Make the model easier to read
M1_ <- list()
M1_ <- summary(M1)

# adding lables
M1_$names <- c("Intercept", "DisimilarityOBJ")
summary(M1_)

# Model 2
M2 <- netlogit(m1, list(DISimilarityOBJ, SimilarityGender), intercept=TRUE, 
                 mode="digraph", diag=FALSE,
                 nullhyp="qapspp",
                 reps=500)
# Make the model easier to read
M2_ <- list()
M2_ <- summary(M2) # Similarity in OBJ not important once gender similarity is controlled

# adding lables
M2_$names <- c("Intercept", "DisimilarityOBJ", "SimilarityGender")
summary(M2_)


# Model 3
M3 <- netlogit(m1, list(DISimilarityOBJ, SimilarityGender, 
                            shared_con), intercept=TRUE, 
                   mode="digraph", diag=FALSE,
                   nullhyp= "qapspp", 
                   reps=500)
# Make the model easier to read
M3_ <- list()
M3_ <- summary(M3)
# adding lables
M3_$names <- c("Intercept", "DisimilarityOBJ", "SimilarityGender", 
                         "N shared contactsr")
summary(M3_)

# No evidence of homophily in overall job satisfaction

# We can continue adding IVs... anything that can be defined on dyad level.

# If you want to save igraph with attributes for later use

save(NETk, file = "org_net_with_Att_data.RData")

# The end of the script

