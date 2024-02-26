#' h20 allow you to utilise more cores on ur pc for parallisation
#' useful for compute time of deep-learning.

library(h2o)
h2o.init()
# This tells me I got 8 cores available.

iris_link <- as.h2o(iris)
iris_dlmodel <- h2o.deeplearning(x = 1:4, y = 5, training_frame = iris_link)
# column 1-4 is features, 5 is labels.

print(iris_dlmodel)
#' with the blackbox defaults h2o sets for us, we can see the layer used:
#' 1. Input layer, 
#' 2 and 3 are Hidden layers of 200 neurons each. ReLU activation (Rectified).
#' 4. Output layer.
#' 
#' Can also see the Confusion Matrix (actual vs pred classes). only 8 virginica
#' predicted wrongly (as versicolor).

# now make a prediction
predictions <- h2o.predict(iris_dlmodel, iris_link)
print(predictions)
#' for each data point, the (prediction and) probability of each predicted 
#' class is shown.
