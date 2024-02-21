# GLM when response is either 0 or 1.

library(glmx)
data("BeetleMortality")
class(BeetleMortality)
as_tibble(BeetleMortality)
BeetleMortality <- mutate(BeetleMortality, percent_died = died/n)
# BeetleMortality$prop_died <- BeetleMortality$died/BeetleMortality$n
# above works too
BeetleMortality
ggplot(BeetleMortality, aes(dose,percent_died)) + geom_point()
# notice it is non-linear, either beetles mostly dead or alive. 
# Point it switches is around 1.78 dosage.

library(titanic)
data("titanic_train")
as_tibble(titanic_train)

titanic_model = glm(Survived ~ Age + Sex, family = binomial(), titanic_train)
# glm is Generalised Linear Model
# binomial because response either 1 (alive) or 0 (dead). 
# AKA logistic regression.

# family = binomial("logit") is the same thing.



summary(titanic_model)

#' NOTE: using logistic regression model (glm, binomial above) 
#' when we predict, it returns us a probability 0~1 instead of a 
#' hard 0 or 1 prediction.
#' This means we have to decide on a cut-off. e.g.

#' library(modelr)
#' df$prediction <- predict(line_model, type = "response")
#' df <- df %>% mutate(pred = ifelse(prediction >= 0.5, 1, 0))

########################################################################

#' But what if got more than 2 classes? 
#' We use Poisson Distribution.
#' family = poisson(link = "log") 
#' 
#' For example Response is an integer count of an event (skin cancer patients)