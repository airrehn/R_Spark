# overfitting example
set.seed(5)
x <- seq(1,5)
y <- x + rnorm(5, mean = 0, sd = 0.5)
# creates 5 random values with mean = 0, sd = 0.5
# standard deviation is on average how much values defer from the mean.

training_data <- tibble(x = x, y = y)

model1 <- lm(y~x)

ggplot(training_data, aes(x,y)) + geom_point() + geom_line(aes(y=fitted(model1)))
# that last geom_line code was clean way to draw the regression line 
# using our x values, and the given model.

# lets give our training data more paras

training_data <- mutate(training_data, x2 = x^2,
                        x3 = x^3,
                        x4 = x^4,
                        x5 = x^5)
# lets make a new better?! (polynomial) model now,
# don't worry we can still use lm.

model2 <- lm(y ~ x + x2 + x3 + x4 + x5, data = training_data)

ggplot(training_data, aes(x,y)) + geom_point() + geom_line(aes(y=fitted(model2)))

# wow it fits so well now the regression! for any x values, it basically goes straight to the 
# y truth value with our model.

# hmm how well does it do for unseen data?

testing_data <- tibble(x = 1.8, y = 1.85)
testing_data <- mutate(testing_data, x2 = x^2,
         x3 = x^3,
         x4 = x^4,
         x5 = x^5)

testing_data <- add_predictions(testing_data,model2, var="pred_y")

ggplot(testing_data, aes(x,y)) + geom_point(color = 'red', size = 5, shape = 'x') + 
  geom_point(aes(x,pred_y), color='blue', size = 5) 

# look how poorly it predicted our data!

# let's see why this is happening, we can make an x_all
# spanning across all the x values of our original training set.

x_all <- seq(1,5,0.01)
data_all <- tibble(x = x_all, x2 = x_all^2, x3 = x_all^3, x4 = x_all^4, x5 = x_all^5)
data_all <- add_predictions(data_all,model2, var="pred_y")
ggplot(training_data, aes(x,y)) + geom_point() + geom_line(data=data_all, aes(x,pred_y)) + 
  geom_point(data = testing_data,color = 'red', size = 5, shape = 'x') + 
  geom_point(data = testing_data, aes(x,pred_y), color='blue', size = 5) 

# this polynomial curve explains why the testing data fitted the way it did earlier.
