#Install packages

#install.packages("tidyverse")

library(tidyverse)

library(knitr)


fastfood <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-04/fastfood_calories.csv")

head(fastfood, 10)

kable(fastfood[1:10, 1:5])

