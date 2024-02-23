#' Superevised machine learning in sparklyr
#' 
#' LMs (section 2) and GLMs (section 4)

##################################
# LMs

library(sparklyr)
sc <- spark_connect("local")
mpg_link <- copy_to(sc, mpg, overwrite = TRUE)

partition <- sdf_random_split(mpg_link, training = 0.8, test = 0.2)
# sdf_random_split from sparklyr library


model_lm <- ml_linear_regression(partition$training, hwy ~ displ + drv)
#' ml_linear_regression also from sparklyr to build LMs. Give it the 
#' training data and formula.

summary(model_lm)

pred_df <- ml_predict(model_lm, partition$test)
# ml_predict from sparklyr too. Returns a df with the "prediction" column attached.
# this is a "lazy" df stored on the sc.

pred_df <- collect(pred_df)

# remember to collect() to retrieve data in local tibble

MSE <- mean((pred_df$hwy - pred_df$prediction)^2)

print(MSE)

################################################################
# GLM - Logistic Regression

#' Trying to build Predictive model for Survived (0 or 1).
#' recall: earlier we build Explanatory model in section 5.

library(sparklyr)
sc <- spark_connect("local")

library(titanic)
titanic_link <- copy_to(sc, titanic_train, "titanic", overwrite = TRUE)

#' 1. Data Cleaning, things to do:
#' 
#' Create a new feature "Family_Size" from siblings, spouse, parents and children.
#' +1L means 1 "literal". In R, specifying the 1 to be int type.
#' Convert the Pclass to chracter type (from int type)
#' Filter out NA values and empty values from Embarked.
#' Fill NA in Age as the Mean Age of the dataset.

cleaned_titanic_link <- titanic_link %>% mutate(Family_Size = SibSp + Parch + 1L) %>%
  mutate(Pclass = as.character(Pclass)) %>% filter(!is.na(Embarked) & Embarked != "") %>%
  mutate(Age = if_else(is.na(Age), mean(Age), Age)) %>% 
  sdf_register("titanic_2")

# this last statement stores another Spark df in sc called titanic_2.
# different from the initial one when we first copy_to().

#' 2. Preprocessing, things to do:
#' Recall native data type in sparklyr (Apache Spark) is Numeric (dbl). It 
#' doesn't like non-numeric variables.
#' - Handle Categorical variables by converting them into numeric representation.
#' - Change int features to Numeric.

#' Buckets/categories for Family size
#'  1-2: 0
#'  3-5: 1
#'  6-12: 2

# as numeric changes int -> double.
processed_tatanic_link <- cleaned_titanic_link %>% 
  mutate(Family_Size = as.numeric(Family_Size)) %>%
  ft_bucketizer("Family_Size", "Family_Size_Cat", splits = c(1,2,5, 12)) %>%
  mutate(Family_Size_Cat = as.character(as.integer(Family_Size_Cat))) %>%
  mutate(Survived = as.numeric(Survived),
         SibSp = as.numeric(SibSp),
         Parch = as.numeric(Parch)) %>%
  sdf_register("titanic_3")


# 3. Create Partition
relevant_data <- select(processed_tatanic_link, Survived, Pclass, Sex, Age, 
                        SibSp, Parch, Fare, Embarked, Family_Size_Cat)


partition <- sdf_random_split(relevant_data, train = 0.75, test = 0.25)


# 4. Train Model and predict
trained_model <- ml_logistic_regression(partition$train, formula = 
                                          Survived ~ Pclass + Sex + Age + 
                                        SibSp + Parch + Fare + 
                                        Embarked + Family_Size_Cat)

pred_df <- collect(ml_predict(trained_model, partition$test))

# Go click the pred_df in the Environment section to see the full returned df.

# There a label column, which is true value of Survived (0 or 1). 
# Then a prediction column (0 or 1) based on probabilities calculated. 
# Higher probabily = belong to that class.


pred_df$prob_of_survived <- unlist(pred_df$probability)[c(FALSE,TRUE)]
# this flattens the probability column's arrays of e.g. c(0.600796597374386, 0.399203402625614),
# and only takes the second value (0.3992xxxx), and store it into the new column.

#' since the format was c(prob_of_survived0, prob_of_survived1), and 0 = dead, 
#' 1 = alive, we know taking the second value means prob_of_survived.

# steal the ROC function agn.

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


ROC <- get_roc(L=pred_df$Survived, f= pred_df$prob_of_survived)

ggplot(ROC, aes(x=FPR, y=TPR)) + geom_line(aes(col = "LR prediction"))

#LR is logistic regression btw.

#########################################################
#' Other models:
#' 
#' We there's a lot of other models besides LR. 
#' We can use Random Forest too for example.
#' It takes the average (or mode for Classification) of the outputs as the result.
#' It also outputs the probability of said output (similar to LR).
#' 
#' Let's do an ROC using Random Forest. Starting from:
#' Step 4. Train Model and predict

rf_model <- ml_random_forest(partition$train, 
                             formula = Survived ~ Pclass + Sex + Age + 
                               SibSp + Parch + Fare + 
                               Embarked + Family_Size_Cat, 
                             type = "classification")
pred_df2 <- collect(ml_predict(rf_model, partition$test))

pred_df2$prob_of_survived <- unlist(pred_df2$probability)[c(FALSE, TRUE)]

ROC_rf <- get_roc(L=pred_df2$Survived, f = pred_df2$prob_of_survived)

ggplot(ROC_rf, aes(x=FPR, y=TPR)) + geom_line(aes(col="RF ROC")) +
  geom_line(data = ROC, aes(col = "LR ROC"))

# We see RF have slightly better prediction, at around 0.25 FPR, it 
# has higher TPR than LR.

spark_disconnect(sc)
