# Neural Networks

library(MASS)
data("iris")
iris <- as.tibble(iris)

library(caret)
# train the NN (finds the weights for the nodes)
model <- train(Species~., iris, method ='nnet', trace = FALSE)
summary(model)

prediction <- predict(model, iris)
table(prediction, iris$Species)
# We see only 3 plants classified wrongly. Much better than section 4's LDA!

# External package to help us visualise our NN.
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(model,nid=T,var.labs=F)
# inputs I1-I4. Hidden nodes H1-H5. Bias B1-B2. Output O1-O3.

###############################################################################
#' Ok but we cheated, section 4 we only used 2 features of the flower. 
#' Our model here uses all 4.
#' How about we try if we used only 2?

model2 <- train(Species ~ Sepal.Length + Sepal.Width, iris, method='nnet', 
                trace = FALSE)
prediction2 <- predict(model2, iris)
table(prediction2, iris$Species) 

# a lil worse than LDA now :(

plot.nnet(model2,nid=T,var.labs=F)
#' See we only got I1-I2 as input. Blackbox settings chose the 
#' hidden and bias layers for us.
#' 
#' Can we choose these settings ourselves to optimise our performance?
