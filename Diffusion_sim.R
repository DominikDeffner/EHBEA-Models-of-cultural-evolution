
#####
#
# Doing violence to reality: Theoretical models of cultural evolution
# Culture evolves II: The spread and evolution of cultural traits
# authored by Dominik Deffner (deffner@mpib-berlin.mpg.de)
#
####

#Load package to construct networks
library(igraph)

#Parameter values
N=100      #Population Size
tmax=1000   #Number of generations
mu = 0.1   #Innovation rate
p = 0.1   #probability any two edges are connected

#Initialize population with cultural traits
#In the beginning, each individual has the same trait  
Pop <- rep(1, N)

#Counter variable to hold the largest id of traits so far; this is to make sure all innovations are novel variants
Counter = 1

#Create output object
Div_NoTraits <- c()

#Create random network
g <- erdos.renyi.game(N, p, type = "gnp")
A <- as.matrix(get.adjacency(g, type = "both"))

#This is how you would do the same thing by hand
# A <- matrix(NA, nrow = N, ncol = N)
# for (i in 1:N) {
#   for (j in 1:N) {
#     if (i == j){
#       A[i,j] <- 0
#     } else if (j > i){
#       A[i,j] <- rbinom(1, 1, p)
#     }
#   }
# }
# A[lower.tri(A)] <- t(A)[lower.tri(A)]


#Loop over generations
for (t in 1:tmax) {

  Pop_new <- c()
  
  #Cultural Transmission in network
  for (i in 1:N) {
    
    #In case agent i has more than 1 network connection, they copy random model
    if (length(which(A[i,] == 1)) > 1){
      
      #IDs of available partners
      Partners <- which(A[i,] == 1)
      
      #Unbiased cultural transmission
      Models <- sample(Partners, size = 1)
      
      #In case agent i has exactly 1 network connection, they copy their trait
    } else if (length(which(A[i,] == 1)) == 1){
      Models <- which(A[i,] == 1)
      
      #In case agent i has no network connection at all, they keep own trait
    } else {
      Models <- i
    }
    
    Pop_new[i] <- Pop[Models]  
  }
  
  # Replace population with new generation
  Pop <- Pop_new
  
  # Innovation
  Innovators <- rbinom(N,1,mu)
  Pop[Innovators == 1] <- (Counter + 1) : (Counter + length(which(Innovators==1)))
  
  #Update counter
  Counter <- max(Pop)
  
  #Store number of unique variants
  Div_NoTraits <- c( Div_NoTraits ,length(unique(Pop)))
  
}#t


#Plotting code
par(mfrow = c(2,1), 
    mar = c(0,4,0,0), 
    oma = c(4,0,0,0.5))


color = sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1])
V(g)$color <- sample(color[sapply(1:N, function(x) which(unique(Pop) == Pop[x]))])
V(g)$frame.color <- V(g)$color
plot(g, vertex.label= NA, edge.arrow.size=0.1,vertex.size = 5)

plot(Div_NoTraits, type = "b", ylab = "", xlab= "", pch = 16)
mtext("Number of unique traits", side = 2, line = 2.75, cex = 1.25)
mtext("Generation", side = 1, line = 2.75, cex = 1.25)


####
###
##
# TASKS
##
###
####


#Explore the (independent) influence of the following factors on cultural diversity:
# a) Population size
# b) Innovation rate
# c) Network density (i.e., value of p parameter)
# Is there anything you find surprising/don't understand?


