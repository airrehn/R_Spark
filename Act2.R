library(gapminder)
library(dbplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(broom)

nesteddf <- nest(group_by(gapminder, country, continent))

get_model <- function(df){
  lm(gdpPercap ~ year, data = df)
}

summariseddf <- mutate(nesteddf, model = map(data, get_model),
                       glance = map(model, broom::glance)) 

glancedf <- unnest(summariseddf, glance, .drop = TRUE)

sorteddf <- arrange(glancedf, r.squared)

filtereddf <- filter(glancedf, r.squared < 0.25)

print(filtereddf$country)
print(filtereddf$continent)
print(nesteddf$continent)

au <- filter(glancedf, country == "Australia")

ggplot(glancedf, aes(continent, r.squared)) + geom_jitter(aes(color=continent))

