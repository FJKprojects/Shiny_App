#read anime data
anime = read.csv('/Users/jk/Downloads/anime_cleaned.csv')
#read anime users data
user = read.csv('/Users/jk/Downloads/users_cleaned.csv')

#load package needed
library(dplyr)
library(maptools)
library(data.table)
library(stringr)

# Clean User dataset

# Below to clean the location of users, extract all country name from the free text 'location' column.

# Using maptools package get a vector of all country names in the world, and clenning process.
data(wrld_simpl)
country = as.data.frame(wrld_simpl@data[["NAME"]])
names(country)[1] = 'country_name'
country$country_name = tolower(country$country_name)

# lower case the location column
user$location = tolower(user$location)

#adding a column in a new data-set user_cleaned_location with default value 'no found'
user_cleaned_location = user%>%
  mutate(
    country = 'no found'
  )

# Use a loop to find test if every observation contain any country_name, overwrite country column with name or 'no found'
for (i in 1:nrow(country)){
  user_cleaned_location$country = 
    ifelse(user_cleaned_location$location %like% country$country_name[i], country$country_name[i], user_cleaned_location$country)
}

# Remove any observation in 'user' data frame without any user with 'no found' country
user = subset(user_cleaned_location, country != 'no found')

# Remove access_rank column, since all value in this column are NA.
user = user[-12]
user = user[-10] 

#=========================================
user2 = user %>%
  group_by(country)%>%
  summarise(n())

country <- user2$country
number <- user2$`n()`
user2 <- data.frame(country,number)
user2$country <- str_to_title(user2$country)
user2$country[user2$country == "United States"] <- "USA"
world_map <- map_data("world")
user$birth <- as.numeric(substring(user$birth_date,1,4))
user$join <- as.numeric(substring(user$join_date,1,4))
user$age <- user$join - user$birth
user$age_group <- ifelse(user$age<18, '<18',ifelse(user$age<30, '18-30', '>30'))
user$gender <- gsub('Non-Binary','Unclear',user$gender)

user$continent <- ifelse(user$country == 'antarctica', user$country == 'Antarctica',countrycode(sourcevar = user[, "country"],
                                                                                                origin = "country.name",
                                                                                                destination = "continent"))
user$continent <- ifelse(user$continent == 'FALSE', 'Antarctica', user$continent)

user2 = user %>%
  group_by(country, age_group)%>%
  summarise(n())
country <- user2$country
number <- user2$`n()`
age_group <- user2$age_group
user2 <- data.frame(country,age_group,number)
user2$country <- str_to_title(user2$country)
user2$country[user2$country == "United States"] <- "USA"
world_map <- map_data("world")
user3 = user %>%
  group_by(country)%>%
  summarise(n())
country <- user3$country
number <- user3$`n()`
user3 <- data.frame(country,number)
user3$country <- str_to_title(user3$country)
user3$country[user3$country == "United States"] <- "USA"
world_map <- map_data("world")

user3$title <- "ALL"
#=========================================

# Clean anime dataset
# Clean non-useful column and Genre column(multiple Genre contain in column, extract first as it's Genre)
# Change decimal to int in duration_min column

anime_cleaned = anime%>%
  select(
    -title_english,
    -title_synonyms,
    -airing,
    -duration,
    -background,
    -premiered,
    -broadcast,
    -related,
    -licensor,
    -producer, 
    -opening_theme, 
    -ending_theme,
    -aired,
    -aired_string
  )%>%
  mutate(
    genre = gsub('\\,', '', word(genre, 1)),
    duration_min = as.integer(duration_min),
    image_url = gsub('cdn-dena.com','net', image_url), #cleaning image_url,found actual pattern of url is 'https://myanimelist.net/anime/'
    actual_url = paste('https://myanimelist.net/anime/',anime_id, sep = '') 
  )%>%
  filter(
    genre != ''
  )



