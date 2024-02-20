# What model explains data best?

# AIC and BIC is trade-off between complexity and fitting our data well (explanatory power).

data("mtcars")
mtcars <- as.tibble(mtcars)

model <- lm(mpg ~ disp, mtcars)
summary(model)

broom::glance(model)

model2 <- lm(mpg ~ disp + wt +hp, mtcars)

broom::glance(model2)

# notice how AIC and BIC becomes smaller (better). Better explanatory power.


# can we make it even better? 

model3 <- lm(mpg ~ disp + wt + hp + cyl, data=mtcars)
broom::glance(model3)

model4 <- lm(mpg ~ disp + wt + hp + cyl + gear, data=mtcars)
broom::glance(model4)

#oops it finally became worse (AIC, BIC increase). How do we know how much paras
# can we add? or even what to add and what to leave out?


###############################
# Step-wise Regression

# We start with the most basic model, then keep add variables until 
# we see an increase in AIC, this tells us our model got worse 
# (the step() function automates this)

# (AIC is the chosen criteria here, there are other functions in other 
# packages that can help u if you want BIC)


# step() takes in the starting model, the most complex upper limit (we want to consider 
# all models until the full additive model), and the direction of step-wise reg.


base_model <- lm(mpg ~ 1, mtcars)

# this is just model with an intercept term.

summary(base_model)
AIC(base_model)

# it sucks, lets do step-wise regression to find 

step(base_model, list(upper = lm(mpg~., mtcars)), direction= "forward")

# notice it tells us wt + cyl + hp (AIC 62.66) is the best, 
# adding am,disp,carb and so on only increases AIC.

################################
# We can also start from Most Complex and go backwards.

 mostComplex_model <- lm(mpg~., mtcars)
 # every single predictor is inside
 summary(mostComplex_model)
 AIC(mostComplex_model)
 
 step(mostComplex_model, list(lower=base_model), direction = "backward")
 
 # notice now it tells us wt + qsec + am (AIC 61.31) is best.
 

 #################################
 # If you alr got a model, you can even start from the middle and go both ways.
 
 eg_existing_model <- lm(mpg~disp + cyl + qsec, mtcars)
 AIC(eg_existing_model)
 
 step(eg_existing_model, list(upper=mostComplex_model, lower=base_model), direction = "both")
 
 # it decided same as before qsec + wt + am is the best. :D
 
 # You can choose which stepwise regression method you like the most/smallest AIC BIC!



