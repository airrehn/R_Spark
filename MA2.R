######## Part 1

library(sparklyr)
sc <- spark_connect(master = "local")

library(nycflights13)
data("flights")
flights_link <- copy_to(sc, nycflights13::flights, "flights")

agg_table <- group_by(flights_link, carrier)
new_col <- summarise(agg_table, mean_airtime = mean(air_time))
collected_data <- collect(new_col)

new_table <- ft_binarizer(flights_link, "distance", "long_flight", threshold = 4000) %>%
  select(distance, long_flight) %>% collect()
num_long_flights <- filter(new_table, long_flight == 1) %>% nrow()



table1 <- tbl(sc, "flights")
sub_table <- select(table1, air_time, distance, dep_time)
clean_table <- na.omit(sub_table)
pca_model <- ml_pca(clean_table)
print(pca_model)

new_table <- ft_binarizer(flights_link, "distance", "long_flight", threshold = 4000)
D <- as.matrix(collect(select(table1, air_time, distance, dep_time)))
E <- as.matrix(pca_model$pc)
P <- D %*% E
PC <- as.data.frame(P)
PC$long_flight <- collect(new_table)$long_flight
ggplot(PC, aes(PC1, PC2)) + geom_point(aes(color = factor(long_flight))) 

ggplot(new_table, aes(distance, dep_time)) + geom_point(aes(color = factor(long_flight)), shape = '+', size = 5) 
                                       

################################## Part 2

set.seed(42)
library(sparklyr)
sc <- spark_connect(master = "local")

library(nycflights13)
data("flights")
flights_link <- copy_to(sc, nycflights13::flights, "flights", overwrite = TRUE)

new_table <- mutate(flights_link, late_arrival = ifelse(arr_delay > 30, 1, 0))
lates <- filter(new_table, late_arrival == 1) %>% collect()
num_late <- nrow(lates)

sub_table <- select(new_table, late_arrival) %>% collect()
missing <- filter(sub_table, is.na(late_arrival))
num_missing <- nrow(missing)

cleaned_table <- filter(new_table, !is.na(late_arrival)) 
relevant <- select(cleaned_table, late_arrival, carrier, dep_delay, month, 
                   year) %>% mutate(late_arrival = as.numeric(late_arrival),
                                    dep_delay = as.numeric(dep_delay),
                                    month = as.numeric(month),
                                    year = as.numeric(year))

partition <- sdf_random_split(relevant, train = 0.75, test = 0.25, seed = 42)

#' trained_model <- ml_logistic_regression(partition$train, formula = 
#'                                          late_arrival ~ carrier + dep_delay +
#'                                          month + year)
#'                                          

 trained_model <- ml_logistic_regression(partition$train, formula = 
                                          late_arrival ~ dep_delay +
                                     carrier + month)

pred_df <- collect(ml_predict(trained_model, partition$test))

pred_df$prob_of_late <- unlist(pred_df$probability)[c(FALSE,TRUE)]

' ROC curve code
#'
#' @param L y_actual (e.g. 0 or 1).
#' @param f, y_predicted_probability.
#'
#' @return points in ROC space and score

get_roc <- function(L, f) {
  # Calculate P and N
  P <- sum(L==1)
  N <- sum(L==0)
  # Order the observations by prediction
  df  <- tibble(L, f)
  df <- df %>% arrange(desc(f))
  # Set TP and FP to zero
  TP <- 0
  FP <- 0
  # Set up matrix for results
  R <- NULL
  # Set previous f
  f_prev <- -Inf
  # set counter
  i <- 1
  while(i <= length(df$L)){
    if( df$f[i] != f_prev){
      R <- rbind(R, c(FP/N, TP/P, df$f[i]))
      f_prev <- df$f[i]
    }
    if(df$L[i] == 1){
      TP <- TP + 1
    } else {
      FP <- FP + 1
    }
    i <- i + 1
  }
  R <- rbind(R, c(FP/N, TP/P, f_prev))
  R <- data.frame(R)
  colnames(R) <- c("FPR","TPR", "Score")
  return(R)
}


ROC <- get_roc(L=pred_df$late_arrival, f= pred_df$prob_of_late)

ggplot(ROC, aes(x=FPR, y=TPR)) + geom_line(aes(col = "without Year"))



################ Part 3
library(readr)
library(dplyr)
url <- "https://gist.githubusercontent.com/tijptjik/9408623/raw/b237fa5848349a14a14e5d4107dc7897c21951f5/wine.csv"
wine_df <-  read_delim(url, delim = ",")

wine_df <- mutate(wine_df, Wine = factor(Wine))

set.seed(42)
library(caret)
library(nnet)
model <- train(Wine~., wine_df, method ='nnet', trace = FALSE)
summary(model)

prediction <- predict(model, wine_df)
table(prediction, wine_df$Wine)


library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(model,nid=T,var.labs=F)

######################## Part 4

library(h2o)
h2o.init()

library(nycflights13)
flights_df <- as_tibble(flights)
print(flights_df)

write.csv(flights_df, "flights_data.csv", row.names = FALSE)
flights_df_read <- read.csv("flights_data.csv")
file.remove("flights_data.csv")
print(flights_df_read)

flights_link <- as.h2o(flights_df_read)
flights_link$late_arrival <- ifelse(flights_link$arr_delay >= 30, 1, 0)
flights_link$late_arrival <- as.factor(flights_link$late_arrival)

print(flights_link)

set.seed(42)
partition <- h2o.splitFrame(data = flights_link, ratios = 0.8, seed = 42)
flight_train <- partition[[1]]
flight_test <- partition[[2]]

predictors <- c("sched_dep_time", "dep_delay", "air_time", "distance")

model <- h2o.deeplearning(x=predictors, y = "late_arrival", training_frame = flight_train,
                         reproducible = T, seed = 42)

print(model)

predictions <- h2o.predict(model, flight_test)
predictions$late_arrival <- flight_test$late_arrival

for_roc<- as.data.frame(predictions)
ROC <- get_roc(L=for_roc$late_arrival, f= for_roc$p1)
ggplot(ROC, aes(x=FPR, y=TPR)) + geom_line(aes(col = "prediction"))


test_pred <- as.data.frame(predictions)
matching_predictions <- sum(test_pred$predict == test_pred$late_arrival)
total_predictions <- nrow(test_pred)
accuracy <- matching_predictions / total_predictions * 100
print(accuracy)
