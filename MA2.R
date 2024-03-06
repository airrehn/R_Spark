######## Part 1

library(sparklyr)
sc <- spark_connect(master = "local")

library(nycflights13)
data("flights")
flights_link <- copy_to(sc, nycflights13::flights, "flights")

agg_table <- group_by(flights_link, carrier)
new_col <- summarise(agg_table, mean_airtime = mean(air_time))
collected_data <- collect(new_col)

new_table <- ft_binarizer(flights_link, "distance", "long_flight", threshold = 4000) %>%
  select(distance, long_flight) %>% collect()
num_long_flights <- filter(new_table, long_flight == 1) %>% nrow()



table1 <- tbl(sc, "flights")
sub_table <- select(table1, air_time, distance, dep_time)
clean_table <- na.omit(sub_table)
pca_model <- ml_pca(clean_table)
print(pca_model)

new_table <- ft_binarizer(flights_link, "distance", "long_flight", threshold = 4000)
D <- as.matrix(collect(select(table1, air_time, distance, dep_time)))
E <- as.matrix(pca_model$pc)
P <- D %*% E
PC <- as.data.frame(P)
PC$long_flight <- collect(new_table)$long_flight
ggplot(PC, aes(PC1, PC2)) + geom_point(aes(color = factor(long_flight))) 

ggplot(new_table, aes(distance, dep_time)) + geom_point(aes(color = factor(long_flight)), shape = '+', size = 5) 
                                       