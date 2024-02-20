library(mlbench)
library(ggplot2)
library(hexbin)

data("BostonHousing2")

ggplot(BostonHousing2, aes(y=medv,x=rm)) + 
  geom_point() + 
  xlab("Average number of rooms per dwelling") + 
  ylab("Median house price (thousands of dollars)")


ggplot(diamonds, mapping = aes(x = carat, y = price)) + 
  geom_hex(bins=50)

ggplot(diamonds, mapping = aes(x = log10(carat), y = log10(price))) + 
  geom_hex(bins=50)
