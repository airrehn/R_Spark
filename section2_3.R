# Multiple predictors
# Mix Continuous and Categorical var

data("BostonHousing2")
BostonHousing2 <- as_tibble(BostonHousing2)
BostonHousing2

line <- lm(medv ~ rm + chas, data = BostonHousing2)
summary(line)
ggplot(BostonHousing2, aes(rm,medv)) + geom_point(aes(color = chas)) +
  geom_smooth(method = "lm", aes(color = chas))

line2 = lm(medv ~ rm + chas + rm:chas, data = BostonHousing2)
# this add interaction variable C3*x1*x2 in C0 + C1x1 + C2x2 + C3x1x2 + err.
# can also be written simply as:
# #line2 = lm(medv ~ rm*chas, data = BostonHousing2) 

summary(line2)
# coeff C3 0 tells us no interaction between predictors x1 and x2. However we can
# also see P value. Pvalue quite big here, therefore CANNOT reject there is no interaction.
# Hence we cannot conclude there is an interaction.