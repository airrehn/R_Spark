# ROC function

# we can use this premade ROC function:

' ROC curve code
#'
#' @param L y_actual (0 or 1).
#' @param f, y_predicted/prob.
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

#############################################
# Taking fake data from section7_2.

set.seed(2017)
df <- tibble(
  x = runif(500, max = 10), # generates random deviates betwen 0-10
  y = rbinom(500, 1, prob = exp(x - 5) / (1 + exp(x - 5))) 
  # generates random binary (0 or 1) w the given probability
)

line_model <- glm(y ~ x, family = (binomial()), data = df)
summary(line_model)

library(modelr)
df$prediction <- predict(line_model, type = "response")

df <- df %>% mutate(pred = ifelse(prediction >= 0.5, 1, 0))

print(df)

ROC  <- get_roc(L = df$y, f = df$pred)

################################
#' Lets add in some random bad predictions to see how that ROC would compare

df$bad_pred  <- runif(500)
bad_ROC  <- get_roc(L = df$y, f = df$bad_pred)


ggplot(ROC, aes(x = FPR, y = TPR)) + geom_line(aes(col = "Legit prediction")) + 
  geom_line(data = bad_ROC, aes(col = "Bad prediction"))

#' Notice how you can tell for the "legit prediction" the TPR rises much faster 
#' in comparison to the FPR. This is a good sign.
