library("modelr")
data("heights")
as_tibble(heights)
levels(heights$sex)
# tells u male is level 1 and there reference level.
# so glm tell tell us odds of being female (w/ education for example)

model = glm(sex ~ education, family = binomial(link="logit"),data = heights)
summary(model)
# so education coeff being 0.047826 tells us for every 1 unit of eduation, 
# log odds of being female increases by 0.047826

# for predicting more than 2 lvls (0 or 1). education yrs is a int count number.
model_2 = glm(education ~ sex + income, family = poisson(link = "log"), data = heights)
summary(model_2)
# notice sexmale is the reference variable again. (coeff 0, 
# sexfemale coeff is 0.05982 higher than male)


