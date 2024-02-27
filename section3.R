# Dataframes in dataframes (models itself in dfs)

library(gapminder)
library(dbplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(broom)
ggplot(gapminder, aes(year, lifeExp, group = country)) + geom_line(alpha = 0.25)
#groupby achieved with group.

au <- filter(gapminder, country == "Australia")
# easy filter of gapminder data on country column.
ggplot(au, aes(year, lifeExp)) + geom_line()


line = lm(lifeExp ~ year, data = au)
summary(line)
# if I wanna do every country have to repeat the above with each filter?
# no!

data("gapminder")
class(gapminder)
# nrow(filter(gapminder, country == "Afghanistan"))
# returns number of rows present

nesteddf <- nest(group_by(gapminder, country, continent))
# This creates a hierarchical grouping structure where there are two levels of grouping: 
# first by continent, then by country.
# nest means put each of this aggregated group into dataframes of their own
nesteddf$data[1]

get_model <- function(df){
  lm(lifeExp ~ year, data = df)
}

summariseddf <- mutate(nesteddf, line_model = map(data, get_model),
                       second_col = map(line_model, broom::tidy)) 
#second_col is useful because it a tidied version of the line_model column. 
# This gives more information.

summariseddf$line_model[1]
summariseddf$second_col[1]
# u notice that the second_column column gives more data, like the pvalue.

# finally we can unnest the second_column. Basically explodes the 2 x 5 tibble there and gives
# each (of the 2) row, a row of it's own on the outer table.
finaldf <- unnest(summariseddf, second_col)
finaldf
                        
plottingdf <- mutate(finaldf, term = fct_recode(term, Intercept = "(Intercept)", 
                                                Slope = "year"))

# just changing the names of the "levels" under "term" column (i.e. (Intercept), and year)

ggplot(plottingdf, aes(estimate, fill=term)) + geom_density(show.legend = FALSE, alpha = 0.5) + 
  geom_histogram(col = "black", fill = "lightgrey",
                 alpha = 0.5,
                 aes(y = ..density..)) + 
  facet_wrap(~term, scales = "free") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal() + 
  labs(y = NULL, x = "Estimate")
