
#PART 1

#Install packages

#install.packages("tidyverse")

library(tidyverse)

library(knitr)


fastfood <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-04/fastfood_calories.csv")

head(fastfood, 10)

kable(fastfood[1:10, 1:5])



#PART 2

library(tidyr)
library(dplyr)
library(ggplot2)

#8
fastfood %>%
  filter(calories >= 1000)

#9
dont_eat_this <- fastfood %>%
  filter(total_fat >=40, total_carb >=80) %>%
  arrange(desc(total_carb))

#10
heavy_food <- fastfood %>%
  mutate(heavy_food = case_when(calories>500 ~ "heavy", calories<250 ~ "low", TRUE ~ "average"))


#11
library(skimr)

skim(fastfood)

#12
fastfood %>%
  count(restaurant) %>%
  arrange(desc(n))

#13
fastfood %>%
  group_by(restaurant) %>%
  summarise(average_calories = mean(calories)) %>%
  ungroup()
  
#14
#1. The average calories of each restaurant
average_calories <- fastfood %>%
  group_by(restaurant) %>%
  summarise(average_calories = mean(calories)) %>%
  ungroup() %>%
  arrange(desc(average_calories)) %>%
  mutate_if(is.numeric, round, digits = 2)


#2. The maximum total fat of each restaurant

max_fat <- fastfood %>%
  group_by(restaurant) %>%
  summarise(total_fat = max(total_fat)) %>%
  ungroup() %>%
  arrange(desc(total_fat))


#3. The minimum cholesterol of each restaurant

min_cholesterol <- fastfood %>%
  group_by(restaurant) %>%
  summarise(cholesterol = min(cholesterol)) %>%
  ungroup() %>%
  arrange(cholesterol)
  


#15
#1. Total fat of each restaurant
#2-3 oguulber nemehee martahgui bh!

total_fat_per_restaurant <- fastfood %>%
  group_by(restaurant) %>%
  summarise(total_fat = sum(total_fat)) %>%
  ungroup() %>%
  arrange(total_fat)

total_fat_per_restaurant %>%
  mutate(restaurant = fct_reorder(restaurant, total_fat)) %>%
  ggplot(aes(restaurant, total_fat, group = 1, fill = total_fat)) +
  geom_col() +
  scale_fill_viridis_b() +
  coord_flip() +
  geom_text(aes(label = total_fat), fontface = "bold", size = 4, vjust = 0) +
  labs(x = NULL, y = "Total fat", 
       title = "TOTAL FAT OF EACH RESTAURANT", caption = "Figure 1a")

total_fat_per_restaurant %>%
  mutate(restaurant = fct_reorder(restaurant, total_fat)) %>%
  ggplot(aes(restaurant, total_fat, group = 1, color = total_fat)) +
  geom_line() +
  geom_point() +
  coord_flip() +
  geom_text(aes(label = total_fat), fontface = "bold", size = 4, vjust = 0) +
  labs(x = NULL, y = "Total fat", 
       title = "TOTAL FAT OF EACH RESTAURANT", caption = "Figure 1b") +
  theme_light()


  
#16
new_fastfood <- fastfood %>%
  mutate(cholesterol_sodium = cholesterol + sodium)
  
new_fastfood$salad <- NULL


#17
#Use observations for Mcdonalds to plot sugar variable against protein with `geom_point()` 
 
Mcdonalds <- fastfood %>%
    select(restaurant, item, sugar, protein) %>%
  filter(restaurant == "Mcdonalds")
 

Mcdonalds %>%
  ggplot(aes(sugar, protein)) +
  geom_point() +
  geom_line() +
  labs(x = "Sugar ", y = "Protein",
       title = "THE PROPOTION OF SUGAR AND PROTEIN", 
       subtitle = "The graph shows the propotion of Mcdonald's fastfoods", caption = "Figure 2") +
  theme_gray() 

  

#PART 3
#18
#Identify variable(s) which should be factors and transform their type into a factor variable.

skim(fastfood)

class(fastfood$item)

fastfood$item <- as_factor(fastfood$item)

fastfood %>%
  mutate(item = fct_lump(item, n = 10)) %>%
  count(item)



#19
#Read about `cut_number()` function using Help and add a new variable to the dataset `calories_type`. Use `calories` variable for `cut_number()` function to split it into 3 categories `n=3`, add labels `labels=c("low", "med", "high")` and make the dataset ordered by arranging it according to calories. 
#Do not forget to save the updated dataset. 

updated_fastfood <- fastfood %>%
  mutate(calories_type = cut_number(calories, n = 3, labels = c("low", "med", "high"))) %>%
  arrange(desc(calories_type))
         

#20
#Create a data viz that shows the distribution of `calories_type` in food items for each type of restaurant. Think carefully about the choice of data viz. 
#Use facets, coordinates and theme layers to make your data viz visually appealing and meaningful. 
#Use factors related data viz functions.


ggplot(updated_fastfood, aes(calories_type, calories)) +
  geom_boxplot(color = "dark green") +
  facet_wrap(~restaurant) +
    theme_light() +
    labs(x = 'Calories type', y = NULL,  
          title = 'THE CALORIES TYPE PER RESTAURANT',  caption = 'Figure 3')



#21
#The second variable should show the percentage of `trans_fat` in `total_fat`. 
#Add the variable to the dataset and call it `trans_fat_percent`. 
#Do not forget to save the updated dataset.

trans_fat_per <- fastfood %>%
  mutate(trans_fat_percent = trans_fat/total_fat*100) %>%
  mutate_if(is.numeric,
            round, 
            digits = 2)



#22
#Create a dataviz that shows the distribution of `trans_fat` in food items for each type of restaurant. 
#Think carefully about the choice of data viz. 
#Use facets, coordinates and theme layers to make your data viz visually appealing and meaningful.


fastfood %>%
  ggplot(aes(restaurant, trans_fat)) +
  geom_col(colour = "purple") +
  facet_wrap(~restaurant) +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = 'Trans fat',
       title = "DISTRIBUTION OF TRANS FAT EACH RESTAURANT", caption = "Figure 4")




#23
#Calculate and show the average (mean)  `total_fat` for each type of restaurant. 
#No need to save it as a variable.

fastfood %>%
  group_by(restaurant) %>%
  summarise(total_fat = mean(total_fat)) %>%
  ungroup() %>%
  arrange(total_fat)

#24
#And create a dataviz that allow to compare different restaurants on this variable (`total_fat`).
#You can present it on one dataviz (= no facets). 


fastfood %>%
  group_by(restaurant) %>%
  summarise(total_fat = mean(total_fat)) %>%
  ungroup() %>%
  arrange(restaurant, total_fat) %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  ggplot(aes(x = reorder(restaurant, - total_fat), y = total_fat, fill = restaurant)) +
  geom_col() +
  geom_text(aes(label = total_fat), fontface = "bold", size = 4, vjust = 0) +
  coord_flip() +
  theme_bw() +
  labs( x = NULL, y = 'Total fat', fill = 'Name of restaurant', 
        title = 'TOTAL FAT PER RESTAURANT', caption = 'Figure 5')


