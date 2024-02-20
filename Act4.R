set.seed(64)
options(digits = 8)  # Set the number of digits to 8
data(heights)
heights$income <- heights$income/1000
heights <- heights[-c(721,973,1611,1641,1866,2828,3122,6178,6319,6794),]
heights$income <- as.numeric(heights$income)
# remove specific NA rows from heights df

heights

library(modelr)

split_sets <- crossv_kfold(heights, k=6)

line_models1 <- map(split_sets$train, ~lm(income ~ height, data =.))
# got 6 line models here
line_models2 <- map(split_sets$train, ~lm(income ~ sex, data =.))
line_models3 <- map(split_sets$train, ~lm(income ~ education, data =.))

get_pred <- function(model, test_data) {
  data <- as.data.frame(test_data)
  pred <- add_predictions(data, model, var = "pred_income")
  return(pred)
}

predictions1 <- map2_df(line_models1, split_sets$test, get_pred, .id = "fromRun")
aggregated_df1 <- group_by(predictions1, fromRun)
meanMSE_forEachSet_1 <- summarise(aggregated_df1, MSE = mean((income-pred_income)^2))


predictions2 <- map2_df(line_models2, split_sets$test, get_pred, .id = "fromRun")
aggregated_df2 <- group_by(predictions2, fromRun)
meanMSE_forEachSet_2 <- summarise(aggregated_df2, MSE = mean((income-pred_income)^2))

predictions3 <- map2_df(line_models3, split_sets$test, get_pred, .id = "fromRun")
aggregated_df3 <- group_by(predictions3, fromRun)
meanMSE_forEachSet_3 <- summarise(aggregated_df3, MSE = mean((income-pred_income)^2))
print(mean(meanMSE_forEachSet_1$MSE))
print(mean(meanMSE_forEachSet_2$MSE))
print(mean(meanMSE_forEachSet_3$MSE))

meanMSE_forEachSet_1
