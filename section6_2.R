# sparklyr only has numeric variable type. So how do we deal with 
# logical (boolean) or categorical data?
options(pillar.sigfig = 7)
options(digits = 8)

spark_install()
sc <- spark_connect(master = "local")

library(nycflights13)
flights_table <- copy_to(sc, nycflights13::flights, "flights")
flights_table

#ft_binarizer takes in a table, a var (column) to convert, 
#the name of the new column, and the predicate/threshold for conversion.

#e.g.
new_table <- ft_binarizer(flights_table, "distance", "is_long_flight", threshold = 1500)

sub_table<-select(new_table, distance, is_long_flight)
# see these 2 columns, anymore >= 1500 is 1. else 0.

#################################
#now we can bring this back to our system and convert it to logical/char or whatever.
shortLong_columnAdded <- collect(sub_table) %>% mutate('is_long_flight2' = ifelse(is_long_flight==0, "short", "long"))

shortLong_columnAdded

# beautiful, this is how we end up with logical data on our system after 
# computation in sparklyr.

############################################
# what if we want more Categories?

new_table2 <- ft_bucketizer(flights_table, "distance", "distance_categories", 
                            splits = c(0,500,1500,Inf))

#ft_bucketzer takes in a table, a var (column) u want to convert to categories, 
#the name of the new column, and the buckets of the categories.

# e.g. 0-500 -> 0 
#      501-1500 -> 1
#      >1500 -> 2

sub_table2 <- select(new_table2, distance, distance_categories)

categories_column_added <- collect(sub_table2)







