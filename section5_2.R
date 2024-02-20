# Predicting

# k-fold cross validation

data(mpg)
mpg

library(modelr)
split_sets <- crossv_kfold(mpg, k=5)
# every sets has 4 part training, 1 part test. 
# Each 1/5 part of the mpg set will be given the chance to be the test set. (other 4/5 is training)
# Therefore we got 5 sets now.
split_sets


# Which model is best to predict hwy? 
# hwy ~ displ or hwy ~ displ + drv ?

line_models1 <- map(split_sets$train, ~lm(hwy~displ, data =.))
# map takes in a list, and a function to map over each element of the list.

# in our case, the given function is the line_model. we put the ~lm() as the ~ 
# tells us what comes after is a function/formula. Similar to lambdas in Python. 
# the "." will take in each element of the given list. i.e. each split_set.

# so this will give us 5 different line_models.

get_pred <- function(model, test_set) {
  data_df <- as.data.frame(test_set)
  pred <- add_predictions(data_df, model, var="pred_hwy")
  # takes a df and add a column named "pred" by default. 
  # Vals determined by given model and df.
  return (pred)
  # returns this df.
}

predictions1 <- map2_df(line_models1, split_sets$test, get_pred, .id = "fromRun")


#map2_df iterates over 2 inputs, applies a function (get_pred) for the 2 inputs, 
# combines the result into a df and returns it.

# the .id = "fromRun" isn't necessary, just basically creates a column "fromRun" in your 
# returned df (the values are taken from the ".id" column of split_set).
# Just as a form of book-keeping.

aggregated_df1 <- group_by(predictions1, fromRun)
# "create" 5 groups here, 1 for each model used for predicting.
meanMSE_forEachSet_1 <- summarise(aggregated_df1, MSE = mean((hwy-pred_hwy)^2))
# for each group, calclate the mean MSE, store into a new column named "MSE". 
# This is in a new df, returns this new df.


####################
# cool now we can do the same for the other model we wanted to test, hwy ~ displ + drv.

line_models2 <- map(split_sets$train, ~lm(hwy ~ displ + drv, data =.))
prediction2 <- map2_df(line_models2, split_sets$test, get_pred, .id = 'fromRun')
aggregated_df2 <- group_by(prediction2, fromRun)
meanMSE_forEachSet_2 <- summarise(aggregated_df2, MSE = mean((hwy-pred_hwy)^2))

print(mean(meanMSE_forEachSet_1$MSE))
print(mean(meanMSE_forEachSet_2$MSE))


# notice the 2nd model with more parameters, give better prediction (lower MSE)!