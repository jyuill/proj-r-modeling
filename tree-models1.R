
## Tree models

## R Book pg 769
## Pollute.txt dataset downloaded (with others) from: http://www.bio.ic.ac.uk/research/mjcraw/therbook/
library(tree)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)

pollute <- read.table("Rbook-datasets/Pollute.txt", header=TRUE)
head(pollute)
tail(pollute)
summary(pollute)

### CORRELATIONS: fine but only gives pair-wise info
## basic correlation matrix
cor(pollute[,2:ncol(pollute)],pollute["Pollution"])

## visualization using PerformanceAnalytics:
chart.Correlation(pollute2, histogram=TRUE, pch=19)

### TREE MODELS ARE MORE EFFECTIVE FOR UNDERSTANDING INTERACTIONS
model <- tree(pollute)
plot(model)
text(model)

cutoff <- 748
low <- pollute$Industry<cutoff ## creates list of logical (TRUE/FALSE) values corresponding to 
## each value in Industry variable, based on whether it is less than 748 or not
tapply(pollute$Pollution, low, mean) ## calculates mean of Pollution values based on whether
## corresponding value of low is TRUE or FALSE (just going by order)

## set up variables to use in plot - Industry example
pollute2 <- pollute
pollute2$low <- pollute2$Industry<cutoff
lowmean <- pollute2 %>% group_by(low) %>% summarize(mean=mean(Pollution))

## scatterplot of Pollution ~ Industry with partitions drawn
plot(pollute$Industry,pollute$Pollution, pch=16)
abline(v=cutoff, lty=2)
lines(c(0,cutoff),c(lowmean[[2,2]],lowmean[[2,2]]))
lines(c(cutoff,max(pollute$Industry)),c(lowmean[[1,2]],lowmean[[1,2]]))

## above tree model printed out in text
model <- tree(Pollution ~., pollute)
print(model)

## info from the model above, translated (applies to tree model or text)

# - strongest indicator of high pollution is high Industry - above 748
# - for the rest, Population is most important variable, but low Population is associated with high Pollution 
# (consider the cause-effect direction)
# - for high levels of Population (above 190), Wet.days is key determinant and places with fewest Wet.days (<108)
# have lowest Pollution of any category in the dataframe (mean=12)
# - for places with more than 108 Wet.days, Temp is the key determinant in explaining variation in Pollution levels;
# warmest places have lower Pollution (mean=15)
# - in cooler places with Wet.days, it is Wind speed that matters: windier places have less Pollution than 
# still places
