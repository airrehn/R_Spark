library(mlbench)
library(ggplot2)
library(dplyr)
data("BostonHousing2")

set.seed(2)
df <- sample_n(BostonHousing2,10)
df <- df %>% mutate(upperbound = -83.144 + 16.392 * rm)
ggplot(df,mapping = aes(y=medv,x=rm)) + geom_point()


# add verticle lines to see how far actual is from predictions (residuels).

ggplot(df, aes(y=medv, x=rm)) + geom_point() + 
  geom_abline(intercept = -83.144, slope = 16.392, color = "red") + 
   geom_linerange(aes(ymin = medv, ymax = upperbound), color = "gray")

# ymin is the starting point of ur verticle line, so medv ("y" of the data itself), 
# then ymax is the ending point, so I need to calculate using the equation of the line.

