set.seed(77)
options(pillar.sigfig = 7)
options(digits = 8)

library(sparklyr)
sc <- spark_connect("local")

formula1 <- formula(sex ~ income + height)
formula2 <- formula(sex ~ height + education)

library(modelr)
height <- modelr::heights

# Data Cleaning
height <- height[-c(721,973,1611,1641,1866,2828,3122,6178,6319,6794),]
height$marital <- as.numeric(recode(height$marital, "divorced"="0", "married"="1", "separated"="2", "single"="3", "widowed"="4"))
height$sex <- ifelse(height$sex=="male",0,1)
# male is 0, female 1.
# divorced is 0, married 1, separated 2, single 3, widowed 4

height_link <- copy_to(sc, height, overwrite = TRUE)

partition <- sdf_random_split(height_link, train = 0.8, test = 0.2, seed = 77)

sdf_nrow(partition$test) 
#for spark_tbl objects (u cant use normal nrow() because Spark df is in the cluster)

trained_model1 <- ml_logistic_regression(partition$train, formula = formula1)
print(trained_model1)

pred_df <- collect(ml_predict(trained_model1, partition$test))
pred_df

pred_df$prob_of_female<- unlist(pred_df$probability)[c(FALSE,TRUE)]

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

ROC <- get_roc(L=pred_df$sex, f= pred_df$prob_of_female)
ggplot(ROC, aes(x=FPR, y=TPR)) + geom_line(aes(col = "LR prediction"))


#########################################################################
# formula2
trained_model2 <- ml_logistic_regression(partition$train, formula = formula2)
print(trained_model2)

pred_df2 <- collect(ml_predict(trained_model2, partition$test))
pred_df2

pred_df2$prob_of_female<- unlist(pred_df2$probability)[c(FALSE,TRUE)]

ROC2 <- get_roc(L=pred_df2$sex, f= pred_df2$prob_of_female)


ggplot(ROC, aes(x=FPR, y=TPR)) + geom_line(aes(col = "For1 pred")) +
  geom_line(data=ROC2, aes(col="For2 pred"))