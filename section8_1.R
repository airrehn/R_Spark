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

