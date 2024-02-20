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
# glm is generalised linear model
# binomial because either 1 (alive) or 0 (dead). AKA logistic regression.
summary(titanic_model)


# what if got more than 2 classes? Poisson distribution is good. 
# For example response is an integer count of an event (skin cancer patients)
