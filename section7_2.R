# A ROC curve represent how well a classification model predict a 
# binary response variable. 


# fake data
set.seed(2017)
df <- tibble(
  x = runif(500, max = 10), # generates random deviates betwen 0-10
  y = rbinom(500, 1, prob = exp(x - 5) / (1 + exp(x - 5))) 
  # generates random binary (0 or 1) w the given probability
)
df

ggplot(df, aes(x,y)) + geom_jitter() + geom_smooth()
# geom_jitter adds a lil shuffle to the points when points land on the same spot.
# nicer to look at compared to geom_point() for binary response (0 or 1).

# look at this urself.
# ggplot(df, aes(x,y)) + geom_point() + geom_smooth()

line_model <- glm(y ~ x, family = (binomial("logit")), data = df)
summary(line_model)

library(modelr)
df$prediction <- predict(line_model, type = "response")
# will using our x values as predictor (stated in line formula y ~ x).

#' NOTE: using logistic regression model (glm, binomial above) 
#' returns us a probability 0.00~1.00 (instead of a hard 0 or 1 prediction).
#' This means we have to decide on a cut-off.
#' See line 44.

# A plot of our prediction
ggplot(df, aes(x,y)) + geom_jitter() + 
  geom_line(aes(x, prediction), col = "orange", size = 2)

#####################################################
# Going back to section 4_1 how do we consider what is "correct" or "wrong"
# (to calculate TPR, FPR etc..), we make a rule saying:
# anything prediction > 0.5 = 1 (fail), else 0 (sucess).

# RMB we define 0 is "success", 1 is "failure".

df <- df %>% mutate(pred = ifelse(prediction >= 0.5, 1, 0))
grouped_observation <- df %>% count(y, pred, name = "observation_count")
confusion_matrix1 <- grouped_observation %>% spread(key = y, value = observation_count, fill = 0)
print(confusion_matrix1)

FPR <- 30/(30+206)
TPR <- 232/(232 + 32)

# now we do a STRICTER prediction > 0.3 = 1, else 0.

confusion_matrix2 <- df %>% mutate(pred = ifelse(prediction >= 0.3, 1, 0)) %>% 
  count(y, pred, name = "observation_count") %>% spread (key = y, value = observation_count, 
                                                         fill = 0)
print(confusion_matrix2)

FPR_stricter <- 14/(14+222)
TPR_stricter <- 216/(216 + 48)
# FPR dropped, good. but TPR went down too, bad.

# now we do a LESS STRICT  prediction > 0.8 = 1 (fail), else 0 (success).

confusion_matrix3 <- df %>% mutate(pred = ifelse(prediction >= 0.8, 1, 0)) %>% 
  count(y, pred, name = "observation_count") %>% spread (key = y, value = observation_count, 
                                                         fill = 0)
print(confusion_matrix3)

FPR_relax <- 70/(70+166)
TPR_relax <- 256/(256 + 8)

rates  <- tribble(
  ~strictness, ~FPR, ~TPR,
  "base", FPR, TPR,
  "stricter", FPR_stricter, TPR_stricter,
  "relaxed", FPR_relax, TPR_relax
)


arrange(rates, TPR)

# we see that as we get stricter requirement for success, TPR drops (bad), FPR drops too (good).
# but if we relax the requirement for success, TPR increases (good), FPR increases too (bad).

ggplot(rates, aes(x = FPR, y = TPR)) + geom_point() +
  xlim(0,1) + ylim

# this is actually an ROC curve, if you vary every single prediction/probability 
# cutoff from 0 to 1 to what counts as success, and failure.
# e.g. start with most strict: pred > 0.00001 is 1, 
# then all the way to: pred > 0.99999 is 1. Plot every point.


##############################################################################3

# Easier way to plot ROC curve

# we can use this premade ROC function:

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

#############################################
# Taking fake data from above.

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

# no need to define cut-off w/ this method 
# df <- df %>% mutate(pred = ifelse(prediction >= 0.5, 1, 0))

print(df)

ROC  <- get_roc(L = df$y, f = df$prediction)

################################
#' Lets add in some random bad predictions to see how that ROC would compare

df$bad_pred  <- runif(500)
bad_ROC  <- get_roc(L = df$y, f = df$bad_pred)


ggplot(ROC, aes(x = FPR, y = TPR)) + geom_line(aes(col = "Legit prediction")) + 
  geom_line(data = bad_ROC, aes(col = "Bad prediction"))

#' Notice how you can tell for the "legit prediction" the TPR rises much faster 
#' in comparison to the FPR. This is a good sign.