
#' Install tensorflow to facilitate use of GPU (or multiple cores of CPU) 
#' for training deep learning models.
library(tensorflow)
install_tensorflow(extra_packages = c("keras", "pillow"), update = TRUE)
#install_tensorflow(version = "gpu") 
#try this if you know you have NVIDIA GPU!


library(reticulate) #you might need this in order to have conversions of types 
# when calling tensorflow

# for deploying and building models
install.packages("keras")
library(keras)

# don't build from scratch, download and use prebuild resnet50.
# application_resnet50() from keras package.
model_resnet <- application_resnet50(weights = 'imagenet')
print(model_resnet)
# v long, it has 152 layers.


head(capture.output(print(model_resnet)), n=15)

tail(capture.output(print(model_resnet)), n=15)
# can see last layer, it can predict into 1000 classes (objects, like fridge, dog ,cat etc)
# also it has 25 million parameters!


###############################################
#' Do some prediction with this resnet50 model.

img <- image_load("./R/assets/fridge.jpg", target_size = c(224,224)) #use fridge image
# resize to match input neurons of resnet (244 x 244 x 3)

# head(capture.output(print(model_resnet)), n=7)  # check for urself!
# the None indicates it can take any batch size.


img_array <- image_to_array(img) #convert to an array
# print(dim(img_array)) # perfect, 244 x 244 x 3 (rgb channels)
img_array <- array_reshape(img_array, c(1, dim(img_array)))
# shape to a 4D(-tensor), the 1 is indicating the batch size. 
# i.e. a single element/image.
# print(dim(img_array)) # 1 x 244 x 244 x 3

img_array <- imagenet_preprocess_input(img_array)
# does some rescaling, among other things

prediction <- predict(model_resnet, img_array)
imagenet_decode_predictions(prediction, top = 3)
# woo refridgerator is the top prediction! (:

###############################################
#' Build your own!

train_d <- "./R/assets/image_keras/data/train" #make sure this points to your training set
validation_d <- "./R/assets/image_keras/data/validation" #ditto for validation set
image_width <- 150
image_height <- 150
# define these ourselves

batch_size <- 32 #no. of samples averaging error before each update of weights.
epochs <- 30 # no. of sweeps of all the training data
train_samples_size = 2048 # items in the /train folder
validation_samples_size = 832 # items in the /val folder

train_data <- flow_images_from_directory(train_d, generator = image_data_generator(), 
                                    target_size = c(image_width, image_height), 
                                    color_mode = "rgb", class_mode = "binary", 
                                    batch_size = batch_size, shuffle = TRUE,
                                    seed = 42)

train_data <- flow_images_from_directory(train_d, generator = image_data_generator(),
                                         target_size = c(image_width, image_height), color_mode = "rgb",
                                         class_mode = "binary", batch_size = batch_size, shuffle = TRUE,
                                         seed = 42)

# the given directory should be given one subdirectory PER class. 
# We have Cat and Dog subdirectories there.

validation_data <- flow_images_from_directory(validation_d, generator = image_data_generator(),
                                              target_size = c(image_width, image_height), 
                                              color_mode = "rgb", classes = NULL,
                                              class_mode = "binary", batch_size = batch_size, 
                                              shuffle = TRUE, seed = 42)

# build model
model <- keras_model_sequential()

# stack stacking in a pipe

model %>% layer_conv_2d(filter = 32, kernel_size = c(3,3), 
                        input_shape = c(image_width, image_height, 3)) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  #' 32 filters means 32 different filters will be applied to the input data,
  #' producing 32 different output feature maps.
  #' 
  #' the 3x3 kernal will scan through the original 150x150 input size.
  
  layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_conv_2d(filter = 64, kernel_size = c(3,3)) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  layer_flatten() %>%
  layer_dense(64) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(1) %>%
  layer_activation("sigmoid")

print(model)
model %>% compile(loss="binary_crossentropy", 
                  optimizer = optimizer_rmsprop(learning_rate = 0.0001),
                  metrics = "accuracy")
print(model)

# 3 hidden layers (w/ convolution and maxpooling)
# dense layers then final layer with only 1 neuron (on/off = Cat or dog)

# untrained prediction
evaluate(model,validation_data, batch_size = batch_size)

# train the model
fit_generator(model, train_data,
  steps_per_epoch = as.integer(train_samples_size/batch_size),  
  epochs = epochs,
  validation_data = validation_data,
  validation_steps = as.integer(validation_samples_size/batch_size), 
  verbose=2
)
