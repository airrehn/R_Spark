options(pillar.sigfig = 10)
options(digits = 8)
library(nycflights13)
data(flights)
flights <- as_tibble(flights)

ggplot(data = flights, aes(dep_delay, arr_delay)) + geom_point() + labs(
    caption = "scatterplot of dep_delay against arr_delay"
  )

flights.lm <- lm(arr_delay ~ dep_delay, data = flights)
flights.lm

plot(flights.lm, which = 1) # residuals vs fitted
plot(flights.lm, which = 3) # constant spread
plot(flights.lm, which = 2) # Q-Q

arrival_delay <- -5.89949348 + 1.01909292 * 100
arrival_delay


ggplot(data = flights, aes(x= dep_delay, y=arr_delay, color = origin)) + geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Departure Delay", y = "Arrival Delay", color = "Origin") 

flights.lm2 <- lm(arr_delay ~ dep_delay + origin + dep_delay:origin, data = flights)
print(flights.lm2)

LGA_arrdelay <- 1 * -6.2846532  + 1 * 1.7456251 


#### Part 3
library(nycflights13)
data(flights)
flights <- as_tibble(flights)
flights

nesteddf <- nest(group_by(flights, carrier))
nesteddf

get_model <- function(df){
  lm(arr_delay ~ dep_delay, data = df)
}

library(broom)
summariseddf <- mutate(nesteddf, line_model = map(data, get_model),
                       second_col = map(line_model, broom::tidy)) 
summariseddf

qn4 <- filter(summariseddf, carrier == "YX")
qn4$line_model


finaldf <- unnest(summariseddf, second_col)
qn5 <- filter(finaldf, carrier == "YV") %>% 
  select(carrier, data, line_model, term, estimate)
qn5

##################### Part 4

library(nycflights13)
data(flights)
flights <- as_tibble(flights)
flights


flights <- flights %>% mutate(delayed = ifelse(arr_delay > 0, 1, 0))
# 1 is delayed, 0 is not delayed/arrive early.
flights_clean<- filter(flights, !is.na(delayed))

flights_clean$delayed <- as.integer(flights_clean$delayed)


zero_count <- sum(flights_clean$delayed == 1)
print(zero_count)

library(glmx)
flights.glm = glm(delayed ~ origin, family = binomial("logit"), flights_clean)
print(flights.glm)

library(modelr)
flights_clean$prediction <- predict(flights.glm, type = "response", newdata = flights_clean)
flights_clean_EWR <- filter(flights_clean, origin == "EWR")
EWR_mean_pred <- mean(flights_clean_EWR$prediction)
print(EWR_mean_pred)

flights_clean_JFK <- filter(flights_clean, origin == "JFK")
JFK_mean_pred <- mean(flights_clean_JFK$prediction)
print(JFK_mean_pred)

flights_clean_LGA <- filter(flights_clean, origin == "LGA")
LGA_mean_pred <- mean(flights_clean_LGA$prediction)
print(LGA_mean_pred)

##################### Part 5

set.seed(19)
library(nycflights13)
data(flights)
flights <- as_tibble(flights)


early_flights <- filter(flights, arr_delay < 0) 
no_flights <- nrow(early_flights)
print(no_flights)

flights <- na.omit(flights, cols="arr_delay")

full_model <- lm(arr_delay~origin+dep_delay+sched_dep_time+carrier+distance+year
                 +month+day+hour, data = flights)
print(full_model)
summary(full_model)

base_model <- lm(arr_delay ~ 1, flights)
step(full_model, list(lower=base_model), direction = "backward")

library(modelr)
split_sets <- crossv_kfold(flights, k=10)
line_models <- map(split_sets$train, ~lm(arr_delay ~ origin + dep_delay + sched_dep_time + 
                                           carrier + distance + month + hour, data =.))
print(line_models$'6')

get_pred <- function(model, test_set) {
  data_df <- as.data.frame(test_set)
  pred <- add_predictions(data_df, model, var="pred_arr_delay")
  return (pred)
  # returns this df.
}

predictions <- map2_df(line_models, split_sets$test, get_pred, .id = "fromRun")
aggregated_df <- group_by(predictions, fromRun)

# "create" 10 groups here, 1 for each model used for predicting.
meanMSE <- summarise(aggregated_df, MSE = mean((arr_delay-pred_arr_delay)^2))

print(mean(meanMSE$MSE))
