# LDA for classifying into iris species in section 4.2
# But what about unsupervised learning? 
# Without giving labels, can we cluster the flowers into groups?

#Introducing PCA (Principal component analysis) and K-means clustering.

sc <- spark_connect(master = "local")

iris_tbl_link <- copy_to(sc, iris, "iris_table", overwrite = TRUE)

table1 <- tbl(sc, "iris_table")
# get table from a source ("iris_table" in Spark sc)


########### data cleaning ###############
sub_table1 <- select(table1, -Species)
# removing the "response" var column 
# (We don't need it for unsupervised leanrning)

sub_table1 <- na.omit(sub_table1)
# clean NA values

# build Principal Component model
pca_model <- ml_pca(sub_table1)
print(pca_model)

#' PC1 and 2 show the most variance, while PC3 and 4 show little variance.  
#' This shows PC3 and 4 actually not so important in determining the 
#' species of the flower.
#' 
#' Note: PCA is actually a preprocessing technique for Dimension Reduction to 
#' reduce the number of features.

# pca_model$pc is a Covariance Matrix
# It shows relationship between different variables. 1 variable wrt to another.

# We want to transform our original data D, into Principal Component matrix P.
# we do this by matrix multiplying D by eigenvector of covariance matrix E (given by 
# pca model) 


D <- as.matrix(iris[1:4])
# all columns except the Species (response column). (150 x 4)

E <- as.matrix(pca_model$pc)
# eigenvectors of covariance matrix. (4 x 4)

P <- D %*% E
# this transforms our original data into Principal Component matrix P.

PC <- as.data.frame(P)
# convert back to df
# add in the species labels again to help colour our graph

PC$Species <- iris$Species
ggplot(PC, aes(PC1, PC2)) + geom_point(aes(color = Species))
# this is our predicted classification. Not bad?
# versicolor and virginia not very good tho, can we do better?

#############################################

# K-means clustering
kmeans_model <- ml_kmeans(iris_tbl_link, formula = ~ Petal_Width + Petal_Length, k = 3)
# the formula says that I want to cluster based on the 
# Petal_Width and Petal_Length variables. 

# (these variables will be used to iteratively compute the means and 
#recluster blah blah blah, go watch a vid)

print(kmeans_model)
# shows us the 3 clusters and their means/centers (of the given features).

clustered_points <- collect(ml_predict(kmeans_model))

print(clustered_points)
print(unique(clustered_points$prediction))

# can see the new column added of the predicted cluster groups (0,1,2).
clustered_points

data(iris)
iris <- as_tibble(iris)
print(iris)
colnames(iris)

# lets plot now

ggplot(clustered_points, aes(Petal_Length, Petal_Width)) + 
  geom_point(aes(Petal_Width, Petal_Length,color = factor(prediction)), size = 2, alpha = 0.5) +
  geom_point(data = kmeans_model$centers, aes(Petal_Width, Petal_Length), pch = 'x', size = 12) +
  geom_point(aes(Petal_Width, Petal_Length,color = Species), size = 1, alpha = 0.5) +
  scale_color_discrete(name = "Predicted Clusters and Truths")

spark_disconnect(sc)





