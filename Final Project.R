# importing the necessary libraries
library(ggplot2)
library(readr)

# importing the data
pokemon <- read_csv("R - Code/pokemon_data.csv")
View(pokemon)

# quick histogram to get a rough grouping (not the official one found at the end
# of the python file)
ggplot(pokemon) + geom_histogram(aes(x=AmountOfPokemon), binwidth = 25) +
  scale_x_continuous(breaks = seq(0, 150, 25)) +
  scale_y_continuous(breaks = seq(0, 15, 1)) + labs(x="Amount of Pokemon",
                                                    y="Number Per Bin",
                                                    title="Amount of Pokemon per Type")

# getting a correlation matrix here for extra help with double checking all of
# the sorting done in the python file
corrScores <- cor(pokemon[sapply(pokemon, is.numeric)])
View(corrScores)

# plotting the average speed versus the total average to see the positive
# correlation. helps to confirm the proccess of determining a good type competitivley
# when we did the avgAtt+avgSpeed, since if it was negative, that would have been bogus
ggplot(pokemon) + geom_point(aes(x=AverageSpeed, y=AverageTotal)) +
  geom_smooth(aes(x=AverageSpeed, y=AverageTotal), method="lm", se=F) +
  labs(title="Avg. Speed vs. Total Avg.")

# doing a boxplot of the restricted pokemon to find if there are any outliers
ggplot(pokemon) + geom_boxplot(aes(y=RestrictedPokemon)) +
  labs(title="Chart 3")

# looking at the correlation between the types and their total average stats,
# to make sure that this project even has a basis. the number is nice and all,
# but sometimes seeing is believing
ggplot(pokemon) + geom_point((aes(x=TypesCategory, y=AverageTotal))) +
  geom_smooth(aes(x=TypesCategory, y=AverageTotal), method="lm", se=F) +
  labs(x="Types", title="Type vs. Total Avg.")
# printing the intercept and slope for the above correlation for funnsies
print(lm(pokemon$TypesCategory~pokemon$AverageTotal))

# starting the proccess to determine the optimal number of clusters to use
kList <- 1:10

xPoke <- pokemon[2:20]

wcss = function(k){
  kmeans(xPoke, centers=k)$tot.withinss
}

wcssValues <- sapply(kList, wcss)

pElbowPlot <- data.frame(k = kList, wcss = wcssValues)

# plotting the list from above to determine the number of clusters to make
ggplot(pElbowPlot, aes(x=k, y=wcss)) + geom_line() + geom_point() + labs(title="Chart 1")

# adding said clusters to the dataframe (only here though. had to go and manually
# add them to the python one)
pokeClusters <- kmeans(pokemon[, 2:20], 5)
pokemon$cluster <- as.factor(pokeClusters$cluster)

# plotting everything that seems to be correlated with the clusters to see if the
# pattern can be seen
ggplot(pokemon) + geom_point(aes(y=AmountOfPokemon, x=PhysicalMoves, color=cluster)) +
  labs(title="Amount of Pokemon vs. # of Physical Moves Seperated by Cluster")

ggplot(pokemon) + geom_point(aes(x=AmountOfPokemon, y=StatuesMoves, color=cluster)) +
  labs(title="Amount of Pokemon vs. # of Status Moves Seperated by Cluster")

ggplot(pokemon) + geom_point(aes(x=AmountOfPokemon, y=RestrictedPokemon, color=cluster)) +
  labs(title="Amount of Pokemon vs. # of Restricted Pokemon Seperated by Cluster")

ggplot(pokemon) + geom_point(aes(x=PhysicalMoves, y=StatuesMoves, color=cluster)) +
  labs(title="# of Physical Moves vs. # of Status Moves Seperated by Cluster")

ggplot(pokemon) + geom_point(aes(x=PhysicalMoves, y=RestrictedPokemon, color=cluster)) +
  labs(title="# of Physical Moves vs. # of Restricted Pokemon Seperated by Cluster")

ggplot(pokemon) + geom_point(aes(x=StatuesMoves, y=RestrictedPokemon, color=cluster)) +
  labs(title="# of Status Moves vs. # of Restricted Pokemon Seperated by Cluster")

# looking to see if there is a deeper correlation between the types and the total average
pContTable <- table(pokemon$AverageTotal, pokemon$TypesCategory)
chisq.test(pContTable)
pContTable2 <- table(pokemon$AverageTotal, pokemon$cluster)
chisq.test(pContTable2)
