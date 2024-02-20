options(pillar.sigfig = 7)
options(digits = 8)


install.packages("sparklyr")
library(sparklyr)
spark_install()
sc <- spark_connect(master = "local")
# spark cluster is on our local machine

library(nycflights13)

flights_table <- copy_to(sc, nycflights13::flights, "flights")
# copied the dataset flights (in nycflights13) to our cluster sc. 
# The table name is specified as flights.
# a link to this via variable flights_table.

src_tbls(sc)
# check table in our cluster

########################
# can use dplyr functions as per normal


filter(flights_table, year == 2013)

select(flights_table, year, origin)
# select x1 x2 .. columns from flights_table.

aggregated_table <- group_by(flights_table, origin)
newdf <- summarise(aggregated_table, mean_delay = mean(dep_delay))

aggregated_table2 <- group_by(flights_table, tailnum)
newdf2 <- summarise(aggregated_table2, count = n(), mean_dist = mean(distance), 
                    mean_delay = mean(dep_delay))
filtered_df <- filter(newdf2, count>20, mean_dist < 2000, !is.na(mean_delay))
# stacking multiple conditions is performing AND.

delay_data<-collect(filtered_df)
# collect: retrieves data from Spark (sc cluster), 
# back into a local tibble/df on our system.

########################################################
# we can even plot now after collecting our data from sc

ggplot(delay_data, aes(mean_dist, mean_delay)) + geom_point(aes(size = count), alpha = 0.5) +
  geom_smooth() +
  scale_size_area(max_size = 2)
