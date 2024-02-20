library(sparklyr)
sc <- spark_connect(master = 'local')

install.packages("rattle")
library(rattle)
data(wine)
wine[-1] <- scale(wine[-1])
# scale all column, except first, of wine dataset to have mean of 0 & varaince of 1.
# then remove first column.
print(wine)

wine_link <- copy_to(sc, wine, "wine_table", overwrite = TRUE)

##############################################
# using PCA

wine_table <- tbl(sc, "wine_table")
wine_table <- select(wine_table, -Type)
# remove Type label/column.

pca_model <- ml_pca(wine_table)
print(pca_model)

##############################################
# using K-means clustering
kmeans_model <- ml_kmeans(wine_link, formula = ~ Nonflavanoids + Hue, k = 3)
print(kmeans_model)






