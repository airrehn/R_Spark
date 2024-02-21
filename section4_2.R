# LDA for more than 2 classes (not count variable e.g. age, education yrs)

library(MASS)
data("iris")
iris <- as.tibble(iris)
iris
levels(iris$Species) # see 3 species.

ggplot(iris,aes(Sepal.Length,Sepal.Width, col = Species)) + geom_point()

flower_model <- lda(Species ~ Sepal.Length + Sepal.Width, data = iris)

# can we draw lines to see seperation? Decision boundary.

# notice plot y axis is from 2-4.5 ish. x axis is from 4-8 ish.
# split this into a grid


grid <- expand.grid("Sepal.Length" = seq(4,8,length = 100), "Sepal.Width"= seq(2,4.5,length=100))
# create a dataframe of all possible combinations of the 2 supplied vectors

# technically any name is ok for the 2 columns, but when I call predict later,
# the provided model will look for the column name Sepal.Length and Sepal.Width 
# to make prediction.
grid <- as.tibble(grid)

# now for each x value and y value of this tibble (grid), I will predict what species belongs there.

grid$pred_species <- predict(flower_model, grid)$class

grid

ggplot(grid, aes(Sepal.Length, Sepal.Width, fill = pred_species)) + 
   geom_tile(alpha=0.2) +
  geom_point(data = iris, aes(col = Species, fill = NA))
