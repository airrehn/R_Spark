# Categorical regression

library(tidyverse)
AdX_team  <- tribble(
  ~Person, ~Pet, 
  "Lewis", "Cat",
  "Sash", "Cat",
  "David", "Dog",
  "Jono", "Rabbit",
  "Dee", "Rabbit",
  "Alex", "Dog"
)
AdX_team <- AdX_team %>% mutate(Pet = factor(Pet))
# tells R that Pet is a categorical variable. 

model_matrix(AdX_team, ~Pet)
# view the encoded matrix for Pet.

########################################################
# Multiple Categorical Predictors
data("chickwts")
chickwts  <- as_tibble(chickwts)
ggplot(chickwts, aes(feed, weight)) + geom_boxplot()

line <- lm(weight ~ feed, data = chickwts)
summary(line)
# every feed is compared to the reference level (the only one not there, casein).

# e.g. feedhorsebin -10, means -10 lower than the mean weight of chicks on casein.
# feedsunflower p-value 0.812495, means no significant difference in weight 
# compared to weight of chicks on casein.

anova(line)
# tells us if feed has a statistically significant effect on weight.
# yes it has, its p-value is  5.936 x 10^-10.

#######################################################
# Quiz 3

data("esoph")
esoph <- esoph %>% mutate(pcase = (ncases/(ncases+ncontrols)))
# is the same as esoph <- mutate(esoph, pcase = (ncases/(ncases+ncontrols)))
print(esoph)
line = lm(pcase ~ alcgp + tobgp, data = esoph)
summary(line)

