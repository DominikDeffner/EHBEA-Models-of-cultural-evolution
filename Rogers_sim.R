

#####
#
# Doing violence to reality: Theoretical models of cultural evolution
# Culture evolves I: The evolution of social learning and our capacity for culture
# authored by Dominik Deffner (deffner@mpib-berlin.mpg.de)
#
####



# basic evolution of social learning
# Rogers (1988) model


# Three kinds of assumptions
# (1) what is the population structure?
# (2) individual states
# (3) life cycle

# (1) population structure
# finite population, well-mixed (everyone can interact with everyone else)
# population all experiences same environment
# environment changes periodically

# (2) individual states
# heritable states: learning strategies - 
# (a) individual learning: pay a personal cost to figure out what to do - c is cost, s is chance you figure out the right thing
# (b) social learning: pay no cost! copy a random adult and maybe get a good behavior
# b is benefit of doing the right thing

# (3) life cycle
# (a) birth
# (b) environmental change
# (c) babies learn
# (d) all adults die


#Simulation function
sim_rogers <- function( 
		num_generations = 1000,
		population_size = 1000,
		w0 = 10,           # baseline fitness
		c,                 # cost of individual learning
		b,                 # benefit of right thing
		u,                 # prob environment changes
		mu			           # mutation rate
 ) 
{

	# Initialize the population; strategy = 1 means individual learning, 2 means social learning
	Strategy <- sample( 1:2 , size=population_size , replace=TRUE )
	
	#All individuals start non-adapted (0 = non-adapted, 1 = adapted)
	Behavior <- rep( 0 , population_size )

	# Initialize history to get full evolutionary history
	history <- data.frame(t = 1:num_generations, Prop_social= NA, Prop_adapted = NA, Fitness=NA, Env_change = NA )

	# loop over generations
	for ( t in 1:num_generations ) {

		# record history

		# freq of social learners
		history$Prop_social[t] <- sum(Strategy==2)/population_size
		# freq of right behavior
		history$Prop_adapted[t] <- sum(Behavior==1)/population_size

		# reproduction / birth
		# compute fitness of each adult
		fitness <- w0 + Behavior*b - (Strategy==1)*c
		
		# record fitness
		history$Fitness[t] <- mean(fitness)
		
		# produce babies in proportion to fitness
		babies_strategies <- sample( Strategy , size=population_size , replace=TRUE , prob=fitness )

		# mutation
		for ( i in 1:population_size ) {
			if ( runif(1) < mu ){
			  babies_strategies[i] <- ifelse(babies_strategies[i] == 1, 2, 1)
			}
		}#i

		# environment changes?
		if ( runif(1) < u ){
		  #if the environment changes, all individuals become non-adapted
		  Behavior <- rep( 0 , population_size )
		  history$Env_change[t] <- 1
    } else{
      Behavior <- Behavior
      history$Env_change[t] <- 0
    }
		
		# learning
		baby_behavior <- rep( 0 , population_size )
		for ( i in 1:population_size ) {
		  
		  #Individual learners learn adaptive behavior with probability s
			if ( babies_strategies[i]==1 ){
				baby_behavior[i] <- 1
			}
		  #Social learners randomly copy one individual from parental generation
			if ( babies_strategies[i]==2 ){
				baby_behavior[i] <- sample( Behavior , size=1 )
			}
		}#i

		# death and growth, we just replace parents with offspring
		Strategy <- babies_strategies
		Behavior <- baby_behavior
	}#t

	return(list(history = history, 
	       u = u, 
	       b=b, 
	       c=c, 
	       mu = mu))

}# end of sim_rogers


#Run simulation and store output
h <- sim_rogers( u=0.05 , b=3 , c=1 , mu = 0.001 )


#Plotting code

{
par(mfrow = c(2,1),
    oma = c(2,0,0,0),
    mar = c(1.5,4,1,4),
    xpd=FALSE)

plot( h$history$Prop_social , ylim=c(0,1)  , xlab="" , ylab="Proportion Social Learners", type="l" , lwd= 2)
axis(side = 4, col = "firebrick", col.axis="firebrick", lwd = 2.5 )
mtext(side = 4, "Proportion of adapted individuals", col = "firebrick", line = 2.7, cex = 1)
lines( h$history$Prop_adapted , ylim=c(0,1)  , xlab="generation" , ylab="Proportion Social Learners", type="l", lwd = 2, col = "firebrick" )
abline(v = which(h$history$Env_change==1), lty = 2, col = "lightgrey")


Fitness_IL <- 10 + h$b - h$c

plot( h$history$Fitness/Fitness_IL ,  xlab="" , ylab="Fitness", type="l" , lwd= 2.5, col = "grey")
abline(h = exp(mean(log(h$history$Fitness/Fitness_IL))), lwd = 1.5, lty = 1, col = "black")
abline(v = which(h$history$Env_change==1), lty = 2, col = "lightgrey")

mtext(side = 1, "Generation", outer = TRUE, line = 1, cex = 1.5)
}




####
###
##
# TASKS
##
###
####


# 1) Run the simulation with the default parameter values and focus on the top plot.
# Describe what happens after a change in the environment (dashed vertical lines): 
# What happens to rates of adaptive behavior (red)?
# What happens to proportion of social learners (black)?
# There is variation in how far adaptive behavior drops. What determines that level?

# 2) The bottom plot shows average per-generation fitness (relative to individual learners).
#   The black line shows (geometric) mean fitness across generations.
#   What do you observe? Can you explain this result?

# 3) Now change u: What is the influence of the rate of environmental change on the adaptive value of social learning?
# 4) What are the consequences of changing the cost c?

# Bonus questions:
# 5) What happens if you increase the mutation rate mu to 0.1, what if you turn it off completely (i.e. set it to 0)? Why do you observe this pattern?
# 6) Set the mutation rate to 0.1 and increase the benefit of adaptive behavior to b=50? What do you see?
    
    