# Autoencoders

#' Autoencoders allow you to train deep networks in smaller steps.
#' 
#' You introduce a smaller layer (AC) between two layers (I1 and I2) of the 
#' same size. I1 and I2 are exactly the same and your AC will take in I1 and 
#' try to reproduce I2. Because of the bottleneck AC, AC must throw away
#' noise and irrelevant information and only keep important features. 
#' 
#' After training on the 3 layers, your AC is build to preserve (impt) information 
#' from I1. You then throw away I2 and create another smaller layer (AC2) between
#' AC and a new replicated AC3 (this is the same as AC). 
#' You can repeat this indefinitely until you train a very deep network.

x <- runif(100, min=-1, max=1)
y <- x + rnorm(100, mean = 0, sd = 0.02)
# introduce some noise around "straight line" of x,y.

I1 <- data.frame(x,y)
names(I1) <- c('Y1','Y2')

I2 <- I1
# basically replicating the I1 to create I2.

plot(I2)

# build Autoencoder AC with hidden layer of 1 neuron.
AC <- nnet::nnet(I1, I2, size = 1, linout = TRUE, maxit=200)

pred_I2 <- predict(AC, I1)
plot(pred_I2)

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(AC)

