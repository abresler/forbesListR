# Load Libraries
library(forbesListR) #devtools::install_github("abresler/forbesListR")
library(dplyr)
library(choroplethr) #install.packages('choroplethr')
library(RColorBrewer)
library(ggplot2)

chor_packages <-
  c('dplyr', 'choroplethr', 'RColorBrewer', 'ggplot2', 'magrittr')

lapply(chor_packages, library, character.only = T)

# Read in Forbes Data
dat <-
  get_year_forbes_list_data(list = "Best Cities for Business", year = 2015)

#Summarize Number of Cities by State
bus <-
  dat %>%
  group_by(state) %>%
  summarize(cities = n_distinct(city)) %>%
  mutate(state = state %>% tolower())


#Read in Choroplethr Data
data("df_state_demographics")

# Merge with cities Data
df_state_demographics <-
  df_state_demographics %>%
  left_join(bus, by = c("region" = "state"))

#Fill NAs as 0 (States with no cities)
df_state_demographics[is.na(df_state_demographics)] <-
  0

#Set value to be city
df_state_demographics <-
  df_state_demographics %>%
  mutate(value = cities)

#Create Plot
choro1 <- StateChoropleth$new(df_state_demographics)
choro1$title = "Forbes' Best Cities for Business by State -- 2015"
choro1$set_num_colors(1)
choro1$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro1$ggplot_scale = scale_fill_gradientn(name = "# Cities",
                                           colours = brewer.pal(8, "Purples"))


#Display Plot
choro1$render()


df_state_demographics$value <-
  df_state_demographics$cities / df_state_demographics$total_population

#Create Plot
choro2 <- StateChoropleth$new(df_state_demographics)
choro2$title = "Forbes' Best Cities for Business (Per Capita) by State"
choro2$set_num_colors(1)
choro2$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro2$ggplot_scale = scale_fill_gradientn(name = "Cities per Capita",
                                           colours = brewer.pal(8, "Greens"))

choro2$render()
