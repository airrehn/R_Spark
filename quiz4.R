data("diamonds")
class(diamonds)
diamonds <- as_tibble(diamonds)

diamonds
line <- lm(price ~ carat * factor(cut,ordered=FALSE), data = diamonds)
#factor tells R that cut is categorical data. 
#treat as ordered or nominal data? use ordered = FALSE for nominal
summary(line)

pred_price <- sum(line$coefficients*c(1,0.433,0,0,1,0,0,0,0.433,0))

# multiply each coeff by the array I specify. 
# the interaction term C3 will be multiplied by 
# x1x2 (essentially 0.433 * 1)
# then add all up.
