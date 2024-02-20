# Checking assumptions

data(diamonds)
diamonds
diamonds.lm <- lm(price ~ carat, data = diamonds)
# create linear model
summary(diamonds.lm)

# check assumptions
plot(diamonds.lm, which = 1)

ggplot(diamonds,aes(carat)) + geom_bar()

plot(diamonds.lm, which = 3)

plot(diamonds.lm, which = 2)
