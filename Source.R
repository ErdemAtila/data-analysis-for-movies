library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(moments)


movies2016_2017 <- read.csv("./Datasets/movies_api_response2016_2017.csv", header = T)
movies2018 <- bind_rows(
  read.csv("./Datasets/movies_api_response2018.csv"),
  read.csv("./Datasets/movies_api_response2018-2.csv")
)
movies2019 <- bind_rows(
  read.csv("./Datasets/movies_api_response2019.csv"),
  read.csv("./Datasets/movies_api_response2019-2.csv")
)
movies2020 <- bind_rows(
  read.csv("./Datasets/movies_api_response2020.csv"),
  read.csv("./Datasets/movies_api_response2020-2.csv")
)
movies2021 <- bind_rows(
  read.csv("./Datasets/movies_api_response2021.csv"),
  read.csv("./Datasets/movies_api_response2021-2.csv")
)
movies2022 <- bind_rows(
  read.csv("./Datasets/movies_api_response2022.csv"),
  read.csv("./Datasets/movies_api_response2022-2.csv")
)
movies2023 <- bind_rows(
  read.csv("./Datasets/movies_api_response2023.csv"),
  read.csv("./Datasets/movies_api_response2023-2.csv")
)
all_movies <- bind_rows(
  movies2018, movies2019, movies2020,
  movies2021, movies2022, movies2023, movies2016_2017
)
releasedCountByYear <- read.csv("movie_year_counts.csv", header = T)
releasedCountByYear


genre_mapping <- c(
  "12" = "Adventure", "14" = "Fantasy", "16" = "Animation", "18" = "Drama",
  "27" = "Horror", "28" = "Action", "35" = "Comedy", "36" = "History",
  "37" = "Western", "53" = "Thriller", "80" = "Crime", "99" = "Documentary",
  "878" = "Science Fiction", "9648" = "Mystery", "10402" = "Music",
  "10749" = "Romance", "10751" = "Family", "10752" = "War", "10770" = "TV Movie"
)


library(dplyr)
library(tidyr)
library(ggplot2)

# Example dataset with genre_ids and vote_count
all_movies <- data.frame(
  movie_id = 1:5,
  genre_ids = c("28, 12", "16, 35", "35, 80", "18, 10751", "99, 18, 14"),
  vote_count = c(500, 300, 150, 400, 250) # Example vote counts
)

# TMDB Genre Mapping
genre_mapping <- c(
  "12" = "Adventure", "14" = "Fantasy", "16" = "Animation", "18" = "Drama",
  "27" = "Horror", "28" = "Action", "35" = "Comedy", "36" = "History",
  "37" = "Western", "53" = "Thriller", "80" = "Crime", "99" = "Documentary",
  "878" = "Science Fiction", "9648" = "Mystery", "10402" = "Music",
  "10749" = "Romance", "10751" = "Family", "10752" = "War", "10770" = "TV Movie"
)


# 1. Process genre_ids and expand dataset
all_genres <- all_movies %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split by comma
  unnest(genre_ids) %>%  # Flatten the list
  mutate(
    genre_ids = as.numeric(genre_ids),  # Convert to numeric
    genre_name = genre_mapping[as.character(genre_ids)]  # Map IDs to names
  )

# 2. Calculate mean vote_count for each genre
genre_means <- all_genres %>%
  filter(vote_average > 1, vote_average < 10) %>%
  group_by(genre_name) %>%
  summarise(mean_vote_count = mean(vote_average, na.rm = TRUE))

# 3. Scatter plot with mean line
ggplot(all_genres %>% filter(vote_average > 1, vote_average < 10), aes(x = genre_name, y = vote_average)) +
  geom_point(color = "skyblue", size = 3) +  # Scatter plot
  geom_point(data = genre_means, aes(x = genre_name, y = mean_vote_count), color = "lightcoral", size = 4, shape = 18) +  # Mean points
  geom_line(data = genre_means, aes(x = genre_name, y = mean_vote_count, group = 1), color = "lightcoral", size = 1) +  # Mean line
  theme_minimal() +
  labs(
    title = "Scatter Plot: Genre vs Vote Average",
    x = "Genre",
    y = "Vote Average"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











library(dplyr)
library(ggplot2)

# Example filtering of data (you already have this part)
filtered_genres <- all_genres %>%
  filter(genre_name %in% selected_genres & vote_average > 1 & vote_average < 10)

# 3. Calculate mean vote_average for selected genres
genre_means <- filtered_genres %>%
  group_by(genre_name) %>%
  summarise(mean_vote_count = mean(vote_average, na.rm = TRUE))

# 4. Scatter plot with mean line, legend at the top
ggplot(filtered_genres, aes(x = genre_name, y = vote_average)) +
  geom_point(aes(color = "Vote Average"), size = 3) +  # Scatter plot
  geom_point(data = genre_means, aes(x = genre_name, y = mean_vote_count, color = "Mean Vote Average"), size = 4, shape = 18) +  # Mean points
  geom_line(data = genre_means, aes(x = genre_name, y = mean_vote_count, group = 1, color = "Mean Vote Average"), size = 1) +  # Mean line
  theme_minimal() +
  labs(
    title = "All Movies Genre and Vote Average Comparison",
    x = "Genre",
    y = "Vote Average",
    color = "Legend"  # Show the legend
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"  # Position the legend at the top
  ) +
  scale_color_manual(values = c("Vote Average" = "skyblue", "Mean Vote Average" = "lightcoral"))  # Custom colors for legend


















# 1. Process genre_ids and expand dataset
all_genres <- all_movies %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split by comma
  unnest(genre_ids) %>%  # Flatten the list
  mutate(
    genre_ids = as.numeric(genre_ids),  # Convert to numeric
    genre_name = genre_mapping[as.character(genre_ids)],  # Map IDs to names
    release_year = year(as.Date(release_date))  # Extract year from release_date
  )

# 2. Filter only selected genres
selected_genres <- c("Drama", "Action", "Romance", "Comedy", "Thriller")

filtered_genres <- all_genres %>%
  filter(genre_name %in% selected_genres & vote_average > 1 & vote_average < 10)

# 3. Calculate mean vote_count for each genre and year
genre_year_means <- filtered_genres %>%
  group_by(genre_name, release_year) %>%
  summarise(mean_vote_count = mean(vote_average, na.rm = TRUE))

# 4. Scatter plot with year-wise facets and legend for mean, positioned at the top
ggplot(filtered_genres, aes(x = genre_name, y = vote_average)) +
  geom_point(aes(color = "Vote Count"), size = 3) +  # Scatter points for vote_count
  geom_point(data = genre_year_means, aes(x = genre_name, y = mean_vote_count, color = "Mean Vote Count"), size = 4, shape = 18) +  # Mean points
  geom_line(data = genre_year_means, aes(x = genre_name, y = mean_vote_count, group = 1, color = "Mean Vote Count"), size = 1) +  # Mean line
  theme_minimal() +
  labs(
    title = "Year-wise: Genre and Vote Average",
    x = "Genre",
    y = "Vote Average",
    color = "Legend"  # Show the legend
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"  # Position the legend at the top
  ) +
  facet_wrap(~release_year, scales = "free_y") +  # Facet by year
  scale_color_manual(values = c("Vote Count" = "skyblue", "Mean Vote Count" = "lightcoral"))  # Custom colors for legend





















all_movies <- all_movies %>%
  mutate(year = year(ymd(release_date))) %>%
  filter(!is.na(year))  # Ensure no missing years


####### MIN - MAX ---->
abc <- all_movies %>% 
  group_by(year) %>% 
  filter(
    vote_count > quantile(vote_count, probs = 0.25, na.rm = TRUE)
  )

abc %>% 
  group_by(year) %>% 
  summarize(
    max_vote_average = max(vote_average),
    min_vote_average = min(vote_average)
  )
####### <--------

#### 2016 data preparation
abc <- all_movies %>% filter(year == 2016)
length(abc$id)

minValue <- quantile(abc$vote_average, probs = 0.03, na.rm = TRUE)
maxValue <- quantile(abc$vote_average, probs = 0.97, na.rm = TRUE)

trimmed2016Movies <- abc %>% filter( vote_average > minValue)
hist(trimmed2016Movies$vote_average)
skewness(trimmed2016Movies$vote_average)
kurtosis(trimmed2016Movies$vote_average)

#### 2017 data preparation
abc <- all_movies %>% filter(year == 2017)
length(abc$id)
hist(abc$vote_average)

minValue <- quantile(abc$vote_average, probs = 0.03, na.rm = TRUE)
minValue
maxValue <- quantile(abc$vote_average, probs = 0.99, na.rm = TRUE)
maxValue

trimmed2017Movies <- abc %>% filter( vote_average > minValue)
hist(trimmed2017Movies$vote_average)
skewness(trimmed2017Movies$vote_average)
kurtosis(trimmed2017Movies$vote_average)

#### 2018 data preparation
abc <- all_movies %>% filter(year == 2018)
length(abc$id)
hist(abc$vote_average)

minValue <- quantile(abc$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(abc$vote_average, probs = 0.98, na.rm = TRUE)
maxValue

trimmed2018Movies <- abc %>% filter( vote_average > minValue)
hist(trimmed2018Movies$vote_average)
skewness(trimmed2018Movies$vote_average)
kurtosis(trimmed2018Movies$vote_average)


#### 2019 data preparation
abc <- all_movies %>% filter(year == 2019)
length(abc$id)
hist(abc$vote_average)

minValue <- quantile(abc$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(abc$vote_average, probs = 0.98, na.rm = TRUE)
maxValue

trimmed2019Movies <- abc %>% filter( vote_average > minValue)
hist(trimmed2019Movies$vote_average)
skewness(trimmed2019Movies$vote_average)
kurtosis(trimmed2019Movies$vote_average)


#### 2020 data preparation
abc <- all_movies %>% filter(year == 2020)
length(abc$id)
hist(abc$vote_average)

minValue <- quantile(abc$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(abc$vote_average, probs = 0.98, na.rm = TRUE)
maxValue

trimmed2020Movies <- abc %>% filter( vote_average > minValue)
hist(trimmed2020Movies$vote_average)
skewness(trimmed2020Movies$vote_average)
kurtosis(trimmed2020Movies$vote_average)

#### 2021 data preparation
abc <- all_movies %>% filter(year == 2021)
length(abc$id)
hist(abc$vote_average)

minValue <- quantile(abc$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(abc$vote_average, probs = 0.98, na.rm = TRUE)
maxValue

trimmed2021Movies <- abc %>% filter( vote_average > minValue)
hist(trimmed2021Movies$vote_average)
skewness(trimmed2021Movies$vote_average)
kurtosis(trimmed2021Movies$vote_average)

#### 2022 data preparation
abc <- all_movies %>% filter(year == 2022)
length(abc$id)
hist(abc$vote_average)

minValue <- quantile(abc$vote_average, probs = 0.08, na.rm = TRUE)
minValue
maxValue <- quantile(abc$vote_average, probs = 0.98, na.rm = TRUE)
maxValue

trimmed2022Movies <- abc %>% filter( vote_average > minValue)
hist(trimmed2022Movies$vote_average)
skewness(trimmed2022Movies$vote_average)
kurtosis(trimmed2022Movies$vote_average)


#### 2023 data preparation
abc <- all_movies %>% filter(year == 2023)
length(abc$id)
hist(abc$vote_average)

minValue <- quantile(abc$vote_average, probs = 0.08, na.rm = TRUE)
minValue
maxValue <- quantile(abc$vote_average, probs = 0.98, na.rm = TRUE)
maxValue

trimmed2023Movies <- abc %>% filter( vote_average > minValue)
hist(trimmed2023Movies$vote_average)
skewness(trimmed2023Movies$vote_average)
kurtosis(trimmed2023Movies$vote_average)

mean(trimmed2016Movies$vote_average)
mean(trimmed2017Movies$vote_average)
mean(trimmed2018Movies$vote_average)
mean(trimmed2019Movies$vote_average)
mean(trimmed2020Movies$vote_average)
mean(trimmed2021Movies$vote_average)
mean(trimmed2022Movies$vote_average)
mean(trimmed2023Movies$vote_average)

mad(movies2023$vote_average)
mad(movies2022$vote_average)
mad(movies2021$vote_average)
mad(movies2020$vote_average)
mad(movies2019$vote_average)
mad(movies2018$vote_average)
movies2016 <- all_movies %>% filter(year == 2016)
movies2017 <- all_movies %>% filter(year == 2017)
mad(movies2016$vote_average)
mad(movies2017$vote_average)

#### SE values
sd(movies2016$vote_average) / sqrt(length(movies2016$vote_average))
sd(movies2017$vote_average) / sqrt(length(movies2017$vote_average))
sd(movies2018$vote_average) / sqrt(length(movies2018$vote_average))
sd(movies2019$vote_average) / sqrt(length(movies2019$vote_average))
sd(movies2020$vote_average) / sqrt(length(movies2020$vote_average))
sd(movies2021$vote_average) / sqrt(length(movies2021$vote_average))
sd(movies2022$vote_average) / sqrt(length(movies2022$vote_average))
sd(movies2023$vote_average) / sqrt(length(movies2023$vote_average))


####### Genre Vote Average DESCRIPTIVE ANALYSIS
####  2016 Action->28, Comedy-> 35, Drama->18, Romance->10749, Thriller->53

action_movies2016 <- movies2016 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 28) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()  

comedy_movies2016 <- movies2016 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 35) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

drama_movies2016 <- movies2016 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 18) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

romance_movies2016 <- movies2016 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 10749) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

thriller_movies2016 <- movies2016 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 53) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()


length(thriller_movies2016$id)
mean(action_movies2016$vote_average)
mean(comedy_movies2016$vote_average)
mean(drama_movies2016$vote_average)
mean(romance_movies2016$vote_average)
mean(thriller_movies2016$vote_average)

sd(action_movies2016$vote_average)
sd(comedy_movies2016$vote_average)
sd(drama_movies2016$vote_average)
sd(romance_movies2016$vote_average)
sd(thriller_movies2016$vote_average)

median(action_movies2016$vote_average)
median(comedy_movies2016$vote_average)
median(drama_movies2016$vote_average)
median(romance_movies2016$vote_average)
median(thriller_movies2016$vote_average)



hist(action_movies2016$vote_average)

minValue <- quantile(abc$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(abc$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedActionMovies2016 <- action_movies2016 %>% filter( vote_average > minValue)
hist(trimmedActionMovies2016$vote_average)
skewness(trimmedActionMovies2016$vote_average)
kurtosis(trimmedActionMovies2016$vote_average)



hist(comedy_movies2016$vote_average)
minValue <- quantile(comedy_movies2016$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(comedy_movies2016$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedComedyMovies2016 <- comedy_movies2016 %>% filter( vote_average > minValue)
hist(trimmedComedyMovies2016$vote_average)
skewness(trimmedComedyMovies2016$vote_average)
kurtosis(trimmedComedyMovies2016$vote_average)


hist(drama_movies2016$vote_average)
minValue <- quantile(drama_movies2016$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(drama_movies2016$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedDramaMovies2016 <- drama_movies2016 %>% filter( vote_average > minValue)
hist(trimmedDramaMovies2016$vote_average)
skewness(trimmedDramaMovies2016$vote_average)
kurtosis(trimmedDramaMovies2016$vote_average)


hist(romance_movies2016$vote_average)
minValue <- quantile(romance_movies2016$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(romance_movies2016$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedRomanceMovies2016 <- romance_movies2016 %>% filter( vote_average > minValue)
hist(trimmedRomanceMovies2016$vote_average)
skewness(trimmedRomanceMovies2016$vote_average)
kurtosis(trimmedRomanceMovies2016$vote_average)





hist(thriller_movies2016$vote_average)
minValue <- quantile(thriller_movies2016$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(thriller_movies2016$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedThrillerMovies2016 <- romance_movies2016 %>% filter( vote_average > minValue)
hist(trimmedThrillerMovies2016$vote_average)
skewness(trimmedThrillerMovies2016$vote_average)
kurtosis(trimmedThrillerMovies2016$vote_average)

mean(trimmedActionMovies2016$vote_average)
mean(trimmedComedyMovies2016$vote_average)
mean(trimmedDramaMovies2016$vote_average)
mean(trimmedRomanceMovies2016$vote_average)
mean(trimmedThrillerMovies2016$vote_average)

mad(trimmedActionMovies2016$vote_average)
mad(trimmedComedyMovies2016$vote_average)
mad(trimmedDramaMovies2016$vote_average)
mad(trimmedRomanceMovies2016$vote_average)
mad(trimmedThrillerMovies2016$vote_average)

quantile(trimmedActionMovies2016$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedComedyMovies2016$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedDramaMovies2016$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedRomanceMovies2016$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedThrillerMovies2016$vote_average, probs = 0.25, na.rm = TRUE)



quantile(trimmedActionMovies2016$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedComedyMovies2016$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedDramaMovies2016$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedRomanceMovies2016$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedThrillerMovies2016$vote_average, probs = 0.75, na.rm = TRUE)



#### 2017

action_movies2017 <- movies2017 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 28) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()  

comedy_movies2017 <- movies2017 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 35) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

drama_movies2017 <- movies2017 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 18) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

romance_movies2017 <- movies2017 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 10749) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

thriller_movies2017 <- movies2017 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 53) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()


length(action_movies2017$vote_average)
length(comedy_movies2017$vote_average)
length(drama_movies2017$vote_average)
length(romance_movies2017$vote_average)
length(thriller_movies2017$vote_average)

mean(action_movies2017$vote_average)
mean(comedy_movies2017$vote_average)
mean(drama_movies2017$vote_average)
mean(romance_movies2017$vote_average)
mean(thriller_movies2017$vote_average)

sd(action_movies2017$vote_average)
sd(comedy_movies2017$vote_average)
sd(drama_movies2017$vote_average)
sd(romance_movies2017$vote_average)
sd(thriller_movies2017$vote_average)

median(action_movies2017$vote_average)
median(comedy_movies2017$vote_average)
median(drama_movies2017$vote_average)
median(romance_movies2017$vote_average)
median(thriller_movies2017$vote_average)



hist(action_movies2017$vote_average)

minValue <- quantile(action_movies2017$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(action_movies2017$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedActionMovies2017 <- action_movies2017 %>% filter( vote_average > minValue)
hist(trimmedActionMovies2017$vote_average)
skewness(trimmedActionMovies2017$vote_average)
kurtosis(trimmedActionMovies2017$vote_average)



hist(comedy_movies2017$vote_average)
minValue <- quantile(comedy_movies2017$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(comedy_movies2017$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedComedyMovies2017 <- comedy_movies2017 %>% filter( vote_average > minValue)
hist(trimmedComedyMovies2017$vote_average)
skewness(trimmedComedyMovies2017$vote_average)
kurtosis(trimmedComedyMovies2017$vote_average)


hist(drama_movies2017$vote_average)
minValue <- quantile(drama_movies2017$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(drama_movies2017$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedDramaMovies2017 <- drama_movies2017 %>% filter( vote_average > minValue)
hist(trimmedDramaMovies2017$vote_average)
skewness(trimmedDramaMovies2017$vote_average)
kurtosis(trimmedDramaMovies2017$vote_average)


hist(romance_movies2017$vote_average)
minValue <- quantile(romance_movies2017$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(romance_movies2017$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedRomanceMovies2017 <- romance_movies2017 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedRomanceMovies2017$vote_average)
skewness(trimmedRomanceMovies2017$vote_average)
kurtosis(trimmedRomanceMovies2017$vote_average)





hist(thriller_movies2017$vote_average)
minValue <- quantile(thriller_movies2017$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(thriller_movies2017$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedThrillerMovies2017 <- romance_movies2017 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedThrillerMovies2017$vote_average)
skewness(trimmedThrillerMovies2017$vote_average)
kurtosis(trimmedThrillerMovies2017$vote_average)

mean(trimmedActionMovies2017$vote_average)
mean(trimmedComedyMovies2017$vote_average)
mean(trimmedDramaMovies2017$vote_average)
mean(trimmedRomanceMovies2017$vote_average)
mean(trimmedThrillerMovies2017$vote_average)

mad(trimmedActionMovies2017$vote_average)
mad(trimmedComedyMovies2017$vote_average)
mad(trimmedDramaMovies2017$vote_average)
mad(trimmedRomanceMovies2017$vote_average)
mad(trimmedThrillerMovies2017$vote_average)

quantile(trimmedActionMovies2017$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedComedyMovies2017$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedDramaMovies2017$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedRomanceMovies2017$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedThrillerMovies2017$vote_average, probs = 0.25, na.rm = TRUE)

quantile(trimmedActionMovies2017$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedComedyMovies2017$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedDramaMovies2017$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedRomanceMovies2017$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedThrillerMovies2017$vote_average, probs = 0.75, na.rm = TRUE)



#### 2018

action_movies2018 <- movies2018 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 28) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()  

comedy_movies2018 <- movies2018 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 35) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

drama_movies2018 <- movies2018 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 18) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

romance_movies2018 <- movies2018 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 10749) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

thriller_movies2018 <- movies2018 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 53) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

length(action_movies2018$vote_average)
length(comedy_movies2018$vote_average)
length(drama_movies2018$vote_average)
length(romance_movies2018$vote_average)
length(thriller_movies2018$vote_average)

mean(action_movies2018$vote_average)
mean(comedy_movies2018$vote_average)
mean(drama_movies2018$vote_average)
mean(romance_movies2018$vote_average)
mean(thriller_movies2018$vote_average)

sd(action_movies2018$vote_average)
sd(comedy_movies2018$vote_average)
sd(drama_movies2018$vote_average)
sd(romance_movies2018$vote_average)
sd(thriller_movies2018$vote_average)

median(action_movies2018$vote_average)
median(comedy_movies2018$vote_average)
median(drama_movies2018$vote_average)
median(romance_movies2018$vote_average)
median(thriller_movies2018$vote_average)



hist(action_movies2018$vote_average)

minValue <- quantile(action_movies2018$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(action_movies2018$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedActionMovies2018 <- action_movies2018 %>% filter( vote_average > minValue)
hist(trimmedActionMovies2018$vote_average)
skewness(trimmedActionMovies2018$vote_average)
kurtosis(trimmedActionMovies2018$vote_average)



hist(comedy_movies2018$vote_average)
minValue <- quantile(comedy_movies2018$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(comedy_movies2018$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedComedyMovies2018 <- comedy_movies2018 %>% filter( vote_average > minValue)
hist(trimmedComedyMovies2018$vote_average)
skewness(trimmedComedyMovies2018$vote_average)
kurtosis(trimmedComedyMovies2018$vote_average)


hist(drama_movies2018$vote_average)
minValue <- quantile(drama_movies2018$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(drama_movies2018$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedDramaMovies2018 <- drama_movies2018 %>% filter( vote_average > minValue)
hist(trimmedDramaMovies2018$vote_average)
skewness(trimmedDramaMovies2018$vote_average)
kurtosis(trimmedDramaMovies2018$vote_average)


hist(romance_movies2018$vote_average)
minValue <- quantile(romance_movies2018$vote_average, probs = 0.15, na.rm = TRUE)
minValue
maxValue <- quantile(romance_movies2018$vote_average, probs = 0.95, na.rm = TRUE)
maxValue

trimmedRomanceMovies2018 <- romance_movies2018 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedRomanceMovies2018$vote_average)
skewness(trimmedRomanceMovies2018$vote_average)
kurtosis(trimmedRomanceMovies2018$vote_average)





hist(thriller_movies2018$vote_average)
minValue <- quantile(thriller_movies2018$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(thriller_movies2018$vote_average, probs = 0.95, na.rm = TRUE)
maxValue

trimmedThrillerMovies2018 <- romance_movies2018 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedThrillerMovies2018$vote_average)
skewness(trimmedThrillerMovies2018$vote_average)
kurtosis(trimmedThrillerMovies2018$vote_average)

mean(trimmedActionMovies2018$vote_average)
mean(trimmedComedyMovies2018$vote_average)
mean(trimmedDramaMovies2018$vote_average)
mean(trimmedRomanceMovies2018$vote_average)
mean(trimmedThrillerMovies2018$vote_average)

mad(trimmedActionMovies2018$vote_average)
mad(trimmedComedyMovies2018$vote_average)
mad(trimmedDramaMovies2018$vote_average)
mad(trimmedRomanceMovies2018$vote_average)
mad(trimmedThrillerMovies2018$vote_average)

quantile(trimmedActionMovies2018$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedComedyMovies2018$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedDramaMovies2018$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedRomanceMovies2018$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedThrillerMovies2018$vote_average, probs = 0.25, na.rm = TRUE)

quantile(trimmedActionMovies2018$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedComedyMovies2018$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedDramaMovies2018$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedRomanceMovies2018$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedThrillerMovies2018$vote_average, probs = 0.75, na.rm = TRUE)


#### 2019

action_movies2019 <- movies2019 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 28) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()  

comedy_movies2019 <- movies2019 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 35) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

drama_movies2019 <- movies2019 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 18) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

romance_movies2019 <- movies2019 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 10749) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

thriller_movies2019 <- movies2019 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 53) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()


length(action_movies2019$vote_average)
length(comedy_movies2019$vote_average)
length(drama_movies2019$vote_average)
length(romance_movies2019$vote_average)
length(thriller_movies2019$vote_average)

mean(action_movies2019$vote_average)
mean(comedy_movies2019$vote_average)
mean(drama_movies2019$vote_average)
mean(romance_movies2019$vote_average)
mean(thriller_movies2019$vote_average)

sd(action_movies2019$vote_average)
sd(comedy_movies2019$vote_average)
sd(drama_movies2019$vote_average)
sd(romance_movies2019$vote_average)
sd(thriller_movies2019$vote_average)

median(action_movies2019$vote_average)
median(comedy_movies2019$vote_average)
median(drama_movies2019$vote_average)
median(romance_movies2019$vote_average)
median(thriller_movies2019$vote_average)




hist(action_movies2019$vote_average)

minValue <- quantile(action_movies2019$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(action_movies2019$vote_average, probs = 0.90, na.rm = TRUE)
maxValue


trimmedActionMovies2019 <- action_movies2019 %>% filter( vote_average > minValue)
hist(trimmedActionMovies2019$vote_average)
skewness(trimmedActionMovies2019$vote_average)
kurtosis(trimmedActionMovies2019$vote_average)



hist(comedy_movies2019$vote_average)
minValue <- quantile(comedy_movies2019$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(comedy_movies2019$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedComedyMovies2019 <- comedy_movies2019 %>% filter( vote_average > minValue)
hist(trimmedComedyMovies2019$vote_average)
skewness(trimmedComedyMovies2019$vote_average)
kurtosis(trimmedComedyMovies2019$vote_average)


hist(drama_movies2019$vote_average)
minValue <- quantile(drama_movies2019$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(drama_movies2019$vote_average, probs = 0.95, na.rm = TRUE)
maxValue

trimmedDramaMovies2019 <- drama_movies2019 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedDramaMovies2019$vote_average)
skewness(trimmedDramaMovies2019$vote_average)
kurtosis(trimmedDramaMovies2019$vote_average)


hist(romance_movies2019$vote_average)
minValue <- quantile(romance_movies2019$vote_average, probs = 0.15, na.rm = TRUE)
minValue
maxValue <- quantile(romance_movies2019$vote_average, probs = 0.95, na.rm = TRUE)
maxValue

trimmedRomanceMovies2019 <- romance_movies2019 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedRomanceMovies2019$vote_average)
skewness(trimmedRomanceMovies2019$vote_average)
kurtosis(trimmedRomanceMovies2019$vote_average)





hist(thriller_movies2019$vote_average)
minValue <- quantile(thriller_movies2019$vote_average, probs = 0.15, na.rm = TRUE)
minValue
maxValue <- quantile(thriller_movies2019$vote_average, probs = 0.95, na.rm = TRUE)
maxValue

trimmedThrillerMovies2019 <- romance_movies2019 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedThrillerMovies2019$vote_average)
skewness(trimmedThrillerMovies2019$vote_average)
kurtosis(trimmedThrillerMovies2019$vote_average)

mean(trimmedActionMovies2019$vote_average)
mean(trimmedComedyMovies2019$vote_average)
mean(trimmedDramaMovies2019$vote_average)
mean(trimmedRomanceMovies2019$vote_average)
mean(trimmedThrillerMovies2019$vote_average)

mad(trimmedActionMovies2019$vote_average)
mad(trimmedComedyMovies2019$vote_average)
mad(trimmedDramaMovies2019$vote_average)
mad(trimmedRomanceMovies2019$vote_average)
mad(trimmedThrillerMovies2019$vote_average)

quantile(trimmedActionMovies2019$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedComedyMovies2019$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedDramaMovies2019$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedRomanceMovies2019$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedThrillerMovies2019$vote_average, probs = 0.25, na.rm = TRUE)

quantile(trimmedActionMovies2019$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedComedyMovies2019$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedDramaMovies2019$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedRomanceMovies2019$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedThrillerMovies2019$vote_average, probs = 0.75, na.rm = TRUE)

#### 2020

action_movies2020 <- movies2020 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 28) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()  

comedy_movies2020 <- movies2020 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 35) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

drama_movies2020 <- movies2020 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 18) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

romance_movies2020 <- movies2020 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 10749) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

thriller_movies2020 <- movies2020 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 53) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()


length(action_movies2020$vote_average)
length(comedy_movies2020$vote_average)
length(drama_movies2020$vote_average)
length(romance_movies2020$vote_average)
length(thriller_movies2020$vote_average)

mean(action_movies2020$vote_average)
mean(comedy_movies2020$vote_average)
mean(drama_movies2020$vote_average)
mean(romance_movies2020$vote_average)
mean(thriller_movies2020$vote_average)

sd(action_movies2020$vote_average)
sd(comedy_movies2020$vote_average)
sd(drama_movies2020$vote_average)
sd(romance_movies2020$vote_average)
sd(thriller_movies2020$vote_average)

median(action_movies2020$vote_average)
median(comedy_movies2020$vote_average)
median(drama_movies2020$vote_average)
median(romance_movies2020$vote_average)
median(thriller_movies2020$vote_average)




hist(action_movies2020$vote_average)

minValue <- quantile(action_movies2020$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(action_movies2020$vote_average, probs = 0.90, na.rm = TRUE)
maxValue


trimmedActionMovies2020 <- action_movies2020 %>% filter( vote_average > minValue)
hist(trimmedActionMovies2020$vote_average)
skewness(trimmedActionMovies2020$vote_average)
kurtosis(trimmedActionMovies2020$vote_average)



hist(comedy_movies2020$vote_average)
minValue <- quantile(comedy_movies2020$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(comedy_movies2020$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedComedyMovies2020 <- comedy_movies2020 %>% filter( vote_average > minValue)
hist(trimmedComedyMovies2020$vote_average)
skewness(trimmedComedyMovies2020$vote_average)
kurtosis(trimmedComedyMovies2020$vote_average)


hist(drama_movies2020$vote_average)
minValue <- quantile(drama_movies2020$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(drama_movies2020$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedDramaMovies2020 <- drama_movies2020 %>% filter( vote_average > minValue)
hist(trimmedDramaMovies2020$vote_average)
skewness(trimmedDramaMovies2020$vote_average)
kurtosis(trimmedDramaMovies2020$vote_average)


hist(romance_movies2020$vote_average)
minValue <- quantile(romance_movies2020$vote_average, probs = 0.25, na.rm = TRUE)
minValue
maxValue <- quantile(romance_movies2020$vote_average, probs = 0.95, na.rm = TRUE)
maxValue

trimmedRomanceMovies2020 <- romance_movies2020 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedRomanceMovies2020$vote_average)
skewness(trimmedRomanceMovies2020$vote_average)
kurtosis(trimmedRomanceMovies2020$vote_average)





hist(thriller_movies2020$vote_average)
minValue <- quantile(thriller_movies2020$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(thriller_movies2020$vote_average, probs = 0.95, na.rm = TRUE)
maxValue

trimmedThrillerMovies2020 <- romance_movies2020 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedThrillerMovies2020$vote_average)
skewness(trimmedThrillerMovies2020$vote_average)
kurtosis(trimmedThrillerMovies2020$vote_average)

mean(trimmedActionMovies2020$vote_average)
mean(trimmedComedyMovies2020$vote_average)
mean(trimmedDramaMovies2020$vote_average)
mean(trimmedRomanceMovies2020$vote_average)
mean(trimmedThrillerMovies2020$vote_average)

mad(trimmedActionMovies2020$vote_average)
mad(trimmedComedyMovies2020$vote_average)
mad(trimmedDramaMovies2020$vote_average)
mad(trimmedRomanceMovies2020$vote_average)
mad(trimmedThrillerMovies2020$vote_average)

quantile(trimmedActionMovies2020$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedComedyMovies2020$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedDramaMovies2020$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedRomanceMovies2020$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedThrillerMovies2020$vote_average, probs = 0.25, na.rm = TRUE)

quantile(trimmedActionMovies2020$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedComedyMovies2020$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedDramaMovies2020$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedRomanceMovies2020$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedThrillerMovies2020$vote_average, probs = 0.75, na.rm = TRUE)



#### 2021

action_movies2021 <- movies2021 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 28) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()  

comedy_movies2021 <- movies2021 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 35) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

drama_movies2021 <- movies2021 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 18) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

romance_movies2021 <- movies2021 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 10749) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

thriller_movies2021 <- movies2021 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 53) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()


length(action_movies2021$vote_average)
length(comedy_movies2021$vote_average)
length(drama_movies2021$vote_average)
length(romance_movies2021$vote_average)
length(thriller_movies2021$vote_average)

mean(action_movies2021$vote_average)
mean(comedy_movies2021$vote_average)
mean(drama_movies2021$vote_average)
mean(romance_movies2021$vote_average)
mean(thriller_movies2021$vote_average)

sd(action_movies2021$vote_average)
sd(comedy_movies2021$vote_average)
sd(drama_movies2021$vote_average)
sd(romance_movies2021$vote_average)
sd(thriller_movies2021$vote_average)

median(action_movies2021$vote_average)
median(comedy_movies2021$vote_average)
median(drama_movies2021$vote_average)
median(romance_movies2021$vote_average)
median(thriller_movies2021$vote_average)




hist(action_movies2021$vote_average)

minValue <- quantile(action_movies2021$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(action_movies2021$vote_average, probs = 0.90, na.rm = TRUE)
maxValue


trimmedActionMovies2021 <- action_movies2021 %>% filter( vote_average > minValue)
hist(trimmedActionMovies2021$vote_average)
skewness(trimmedActionMovies2021$vote_average)
kurtosis(trimmedActionMovies2021$vote_average)



hist(comedy_movies2021$vote_average)
minValue <- quantile(comedy_movies2021$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(comedy_movies2021$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedComedyMovies2021 <- comedy_movies2021 %>% filter( vote_average > minValue)
hist(trimmedComedyMovies2021$vote_average)
skewness(trimmedComedyMovies2021$vote_average)
kurtosis(trimmedComedyMovies2021$vote_average)


hist(drama_movies2021$vote_average)
minValue <- quantile(drama_movies2021$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(drama_movies2021$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedDramaMovies2021 <- drama_movies2021 %>% filter( vote_average > minValue)
hist(trimmedDramaMovies2021$vote_average)
skewness(trimmedDramaMovies2021$vote_average)
kurtosis(trimmedDramaMovies2021$vote_average)


hist(romance_movies2021$vote_average)
minValue <- quantile(romance_movies2021$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(romance_movies2021$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedRomanceMovies2021 <- romance_movies2021 %>% filter( vote_average > minValue)
hist(trimmedRomanceMovies2021$vote_average)
skewness(trimmedRomanceMovies2021$vote_average)
kurtosis(trimmedRomanceMovies2021$vote_average)





hist(thriller_movies2021$vote_average)
minValue <- quantile(thriller_movies2021$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(thriller_movies2021$vote_average, probs = 0.95, na.rm = TRUE)
maxValue

trimmedThrillerMovies2021 <- romance_movies2021 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedThrillerMovies2021$vote_average)
skewness(trimmedThrillerMovies2021$vote_average)
kurtosis(trimmedThrillerMovies2021$vote_average)

mean(trimmedActionMovies2021$vote_average)
mean(trimmedComedyMovies2021$vote_average)
mean(trimmedDramaMovies2021$vote_average)
mean(trimmedRomanceMovies2021$vote_average)
mean(trimmedThrillerMovies2021$vote_average)

mad(trimmedActionMovies2021$vote_average)
mad(trimmedComedyMovies2021$vote_average)
mad(trimmedDramaMovies2021$vote_average)
mad(trimmedRomanceMovies2021$vote_average)
mad(trimmedThrillerMovies2021$vote_average)

quantile(trimmedActionMovies2021$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedComedyMovies2021$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedDramaMovies2021$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedRomanceMovies2021$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedThrillerMovies2021$vote_average, probs = 0.25, na.rm = TRUE)

quantile(trimmedActionMovies2021$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedComedyMovies2021$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedDramaMovies2021$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedRomanceMovies2021$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedThrillerMovies2021$vote_average, probs = 0.75, na.rm = TRUE)



#### 2022

action_movies2022 <- movies2022 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 28) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()  

comedy_movies2022 <- movies2022 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 35) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

drama_movies2022 <- movies2022 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 18) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

romance_movies2022 <- movies2022 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 10749) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

thriller_movies2022 <- movies2022 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 53) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()


length(action_movies2022$vote_average)
length(comedy_movies2022$vote_average)
length(drama_movies2022$vote_average)
length(romance_movies2022$vote_average)
length(thriller_movies2022$vote_average)

mean(action_movies2022$vote_average)
mean(comedy_movies2022$vote_average)
mean(drama_movies2022$vote_average)
mean(romance_movies2022$vote_average)
mean(thriller_movies2022$vote_average)

sd(action_movies2022$vote_average)
sd(comedy_movies2022$vote_average)
sd(drama_movies2022$vote_average)
sd(romance_movies2022$vote_average)
sd(thriller_movies2022$vote_average)

median(action_movies2022$vote_average)
median(comedy_movies2022$vote_average)
median(drama_movies2022$vote_average)
median(romance_movies2022$vote_average)
median(thriller_movies2022$vote_average)




hist(action_movies2022$vote_average)

minValue <- quantile(action_movies2022$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(action_movies2022$vote_average, probs = 0.95, na.rm = TRUE)
maxValue


trimmedActionMovies2022 <- action_movies2022 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedActionMovies2022$vote_average)
skewness(trimmedActionMovies2022$vote_average)
kurtosis(trimmedActionMovies2022$vote_average)



hist(comedy_movies2022$vote_average)
minValue <- quantile(comedy_movies2022$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(comedy_movies2022$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedComedyMovies2022 <- comedy_movies2022 %>% filter( vote_average > minValue)
hist(trimmedComedyMovies2022$vote_average)
skewness(trimmedComedyMovies2022$vote_average)
kurtosis(trimmedComedyMovies2022$vote_average)


hist(drama_movies2022$vote_average)
minValue <- quantile(drama_movies2022$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(drama_movies2022$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedDramaMovies2022 <- drama_movies2022 %>% filter( vote_average > minValue)
hist(trimmedDramaMovies2022$vote_average)
skewness(trimmedDramaMovies2022$vote_average)
kurtosis(trimmedDramaMovies2022$vote_average)


hist(romance_movies2022$vote_average)
minValue <- quantile(romance_movies2022$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(romance_movies2022$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedRomanceMovies2022 <- romance_movies2022 %>% filter( vote_average > minValue)
hist(trimmedRomanceMovies2022$vote_average)
skewness(trimmedRomanceMovies2022$vote_average)
kurtosis(trimmedRomanceMovies2022$vote_average)





hist(thriller_movies2022$vote_average)
minValue <- quantile(thriller_movies2022$vote_average, probs = 0.15, na.rm = TRUE)
minValue
maxValue <- quantile(thriller_movies2022$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedThrillerMovies2022 <- romance_movies2022 %>% filter( vote_average > minValue)
hist(trimmedThrillerMovies2022$vote_average)
skewness(trimmedThrillerMovies2022$vote_average)
kurtosis(trimmedThrillerMovies2022$vote_average)

mean(trimmedActionMovies2022$vote_average)
mean(trimmedComedyMovies2022$vote_average)
mean(trimmedDramaMovies2022$vote_average)
mean(trimmedRomanceMovies2022$vote_average)
mean(trimmedThrillerMovies2022$vote_average)

mad(trimmedActionMovies2022$vote_average)
mad(trimmedComedyMovies2022$vote_average)
mad(trimmedDramaMovies2022$vote_average)
mad(trimmedRomanceMovies2022$vote_average)
mad(trimmedThrillerMovies2022$vote_average)

quantile(trimmedActionMovies2022$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedComedyMovies2022$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedDramaMovies2022$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedRomanceMovies2022$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedThrillerMovies2022$vote_average, probs = 0.25, na.rm = TRUE)

quantile(trimmedActionMovies2022$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedComedyMovies2022$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedDramaMovies2022$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedRomanceMovies2022$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedThrillerMovies2022$vote_average, probs = 0.75, na.rm = TRUE)




#### 2023

action_movies2023 <- movies2023 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 28) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()  

comedy_movies2023 <- movies2023 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 35) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

drama_movies2023 <- movies2023 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 18) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

romance_movies2023 <- movies2023 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 10749) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()

thriller_movies2023 <- movies2023 %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre_ids into a list
  unnest(genre_ids) %>%                             # Expand list into separate rows
  mutate(genre_ids = as.numeric(genre_ids)) %>%     # Convert to numeric
  filter(genre_ids == 53) %>%                       # Filter rows where genre_ids is 28 (Action)
  distinct()


length(action_movies2023$vote_average)
length(comedy_movies2023$vote_average)
length(drama_movies2023$vote_average)
length(romance_movies2023$vote_average)
length(thriller_movies2023$vote_average)

mean(action_movies2023$vote_average)
mean(comedy_movies2023$vote_average)
mean(drama_movies2023$vote_average)
mean(romance_movies2023$vote_average)
mean(thriller_movies2023$vote_average)

sd(action_movies2023$vote_average)
sd(comedy_movies2023$vote_average)
sd(drama_movies2023$vote_average)
sd(romance_movies2023$vote_average)
sd(thriller_movies2023$vote_average)

median(action_movies2023$vote_average)
median(comedy_movies2023$vote_average)
median(drama_movies2023$vote_average)
median(romance_movies2023$vote_average)
median(thriller_movies2023$vote_average)




hist(action_movies2023$vote_average)

minValue <- quantile(action_movies2023$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(action_movies2023$vote_average, probs = 0.90, na.rm = TRUE)
maxValue


trimmedActionMovies2023 <- action_movies2023 %>% filter( vote_average > minValue)
hist(trimmedActionMovies2023$vote_average)
skewness(trimmedActionMovies2023$vote_average)
kurtosis(trimmedActionMovies2023$vote_average)



hist(comedy_movies2023$vote_average)
minValue <- quantile(comedy_movies2023$vote_average, probs = 0.05, na.rm = TRUE)
minValue
maxValue <- quantile(comedy_movies2023$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedComedyMovies2023 <- comedy_movies2023 %>% filter( vote_average > minValue)
hist(trimmedComedyMovies2023$vote_average)
skewness(trimmedComedyMovies2023$vote_average)
kurtosis(trimmedComedyMovies2023$vote_average)


hist(drama_movies2023$vote_average)
minValue <- quantile(drama_movies2023$vote_average, probs = 0.10, na.rm = TRUE)
minValue
maxValue <- quantile(drama_movies2023$vote_average, probs = 0.90, na.rm = TRUE)
maxValue

trimmedDramaMovies2023 <- drama_movies2023 %>% filter( vote_average > minValue)
hist(trimmedDramaMovies2023$vote_average)
skewness(trimmedDramaMovies2023$vote_average)
kurtosis(trimmedDramaMovies2023$vote_average)


hist(romance_movies2023$vote_average)
minValue <- quantile(romance_movies2023$vote_average, probs = 0.15, na.rm = TRUE)
minValue
maxValue <- quantile(romance_movies2023$vote_average, probs = 0.95, na.rm = TRUE)
maxValue

trimmedRomanceMovies2023 <- romance_movies2023 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedRomanceMovies2023$vote_average)
skewness(trimmedRomanceMovies2023$vote_average)
kurtosis(trimmedRomanceMovies2023$vote_average)





hist(thriller_movies2023$vote_average)
minValue <- quantile(thriller_movies2023$vote_average, probs = 0.15, na.rm = TRUE)
minValue
maxValue <- quantile(thriller_movies2023$vote_average, probs = 0.95, na.rm = TRUE)
maxValue

trimmedThrillerMovies2023 <- romance_movies2023 %>% filter( vote_average > minValue, vote_average < maxValue)
hist(trimmedThrillerMovies2023$vote_average)
skewness(trimmedThrillerMovies2023$vote_average)
kurtosis(trimmedThrillerMovies2023$vote_average)

mean(trimmedActionMovies2023$vote_average)
mean(trimmedComedyMovies2023$vote_average)
mean(trimmedDramaMovies2023$vote_average)
mean(trimmedRomanceMovies2023$vote_average)
mean(trimmedThrillerMovies2023$vote_average)

mad(trimmedActionMovies2023$vote_average)
mad(trimmedComedyMovies2023$vote_average)
mad(trimmedDramaMovies2023$vote_average)
mad(trimmedRomanceMovies2023$vote_average)
mad(trimmedThrillerMovies2023$vote_average)

quantile(trimmedActionMovies2023$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedComedyMovies2023$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedDramaMovies2023$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedRomanceMovies2023$vote_average, probs = 0.25, na.rm = TRUE)
quantile(trimmedThrillerMovies2023$vote_average, probs = 0.25, na.rm = TRUE)

quantile(trimmedActionMovies2023$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedComedyMovies2023$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedDramaMovies2023$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedRomanceMovies2023$vote_average, probs = 0.75, na.rm = TRUE)
quantile(trimmedThrillerMovies2023$vote_average, probs = 0.75, na.rm = TRUE)






##### PERCENTAGE CHANGES GENRE 
# Extract the year from release_date
all_movies <- all_movies %>%
  mutate(year = year(ymd(release_date))) %>%
  filter(!is.na(year))  # Ensure no missing years

length(all_movies$id)

# Process genre data
genre_changes <- all_movies %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre IDs
  unnest(genre_ids) %>%
  mutate(genre_ids = as.numeric(genre_ids)) %>%
  count(year, genre_ids) %>%  # Count movies for each genre per year
  group_by(genre_ids) %>%
  arrange(year) %>%
  mutate(
    pct_change = c(0, diff(n) / head(n, -1) * 100)  # Calculate percentage change
  )

# Map genre IDs to names
genre_changes <- genre_changes %>%
  mutate(genre_name = recode(as.character(genre_ids), !!!genre_mapping))

# Select top 5 genres with the most movies overall
top_genres <- genre_changes %>%
  group_by(genre_name) %>%
  summarise(total_count = sum(n, na.rm = TRUE)) %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 10) %>%
  pull(genre_name)

# Filter data for top genres
filtered_genre_changes <- genre_changes %>%
  filter(genre_name %in% top_genres)

# Plotting the percentage changes for top genres
ggplot(filtered_genre_changes, aes(x = year, y = pct_change, color = genre_name, group = genre_name)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1") +  # Use a distinctive color palette
  labs(
    title = "Percentage Change in Movie Genres Over the Years",
    x = "Year",
    y = "Percentage Change",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )

colnames(all_movies)




##### BUBBLE CHART
ggplot(filtered_genre_changes, aes(x = year, y = genre_name, size = abs(pct_change), color = pct_change)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(
    title = "Bubble Chart: Percentage Change in Genres",
    x = "Year",
    y = "Genre",
    size = "Magnitude",
    color = "Change"
  ) +
  theme_minimal()









##########. RELEASE MONTH (ALL_MOVIES)
# Add the 'period' column
all_movies <- all_movies %>%
  mutate(
    release_month = month(release_date, label = TRUE, abbr = TRUE),  # Extract month as factor
    period = ifelse(year(ymd(release_date)) < 2020, "Pre-Pandemic", "Post-Pandemic")  # Define period
  ) %>%
  filter(!is.na(release_month))  # Remove rows with invalid release_date

# Aggregate movie counts by release_month and period
monthly_frequency <- all_movies %>%
  group_by(release_month, period) %>%
  summarise(frequency = n(), .groups = 'drop')

# Create the area plot for Pre and Post Pandemic
ggplot(monthly_frequency, aes(x = release_month, y = frequency, group = period, fill = period)) +
  geom_area(alpha = 0.7) +
  geom_line(color = "#D0E8C5", size = 1) +
  labs(
    title = "Movies Released by Month",
    x = "Month",
    y = "Count"
  ) +
  scale_fill_manual(values = c("Pre-Pandemic" = "steelblue", "Post-Pandemic" = "lightcoral")) +  # Custom colors
  facet_wrap(~period) +  # Create separate plots for Pre and Post Pandemic
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"  # Hide the legend since it is redundant with faceting
  )





############. RELEASE DAY OF MONTH (ALL_MOVIES)
# Add 'period' variable to categorize data into pre-pandemic and post-pandemic
all_movies <- all_movies %>%
  mutate(
    release_day = day(release_date),  # Extract day of the month
    period = ifelse(year(ymd(release_date)) < 2020, "Pre-Pandemic", "Post-Pandemic")  # Categorize into periods
  ) %>%
  filter(!is.na(release_day))  # Remove rows with invalid release_date

# Aggregate movie counts by release_day and period
daily_frequency <- all_movies %>%
  group_by(release_day, period) %>%
  summarise(frequency = n())

# Create the area plot with separate areas for pre-pandemic and post-pandemic
ggplot(daily_frequency, aes(x = release_day, y = frequency, fill = period)) +
  #geom_area(alpha = 0.7, position = "stack", stat = "identity") +  # Fill areas based on 'period'
  geom_line(aes(color = period), size = 1) +  # Line for both periods
  scale_fill_manual(values = c("Pre-Pandemic" = "steelblue", "Post-Pandemic" = "lightcoral")) +  # Color the areas
  scale_color_manual(values = c("Pre-Pandemic" = "steelblue", "Post-Pandemic" = "lightcoral")) +  # Line color for periods
  labs(
    title = "Movies Released by Day of the Month (Pre and Post Pandemic)",
    x = "Day of the Month",
    y = "Count"
  ) +
  scale_x_continuous(
    limits = c(1, 31),  # Limit x-axis between 1 and 31
    breaks = c(1, 10, 20, 31)  # Display ticks at 1, 10, 20, and 31
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold",  hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "top"
  )









############. RELEASE DAY OF YEAR (ALL_MOVIES, from other source)
ggplot(releasedCountByYear, aes(x = year, y = count, group = 1)) +
  geom_line(color = "steelblue", size = 1) +  # Line first
  geom_point(fill = "#D0E8C5", color = "black", alpha = 0.7, size = 3) +  # Points on top of the line
  coord_cartesian(ylim = c(15000, 22000)) +
  labs(
    title = "Movies Released by Year",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = c(2016:2023)  # Display ticks at 1, 10, 20, and 31
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )





######### Comparison of Adult and All Movies Changes

# Calculate percentage changes for 'count' and 'adult'
releasedCountByYear <- releasedCountByYear %>%
  arrange(year) %>%
  mutate(
    all_movie_pct_change = c(0, diff(count) / head(count, -1) * 100),  # Set 2018 as 0% change for all movies
    adult_movie_pct_change = c(0, diff(adult) / head(adult, -1) * 100)   # Set 2018 as 0% change for adult movies
  )

# Reshaping the data to long format using pivot_longer
long_data <- releasedCountByYear %>%
  pivot_longer(cols = c(all_movie_pct_change, adult_movie_pct_change), names_to = "type", values_to = "value")

max(releasedCountByYear$all_movie_pct_change)
# Plotting
ggplot(long_data, aes(x = year, y = value, color = type, group = type)) +
  geom_line(size = 1) +
  geom_point(fill = "#D0E8C5", color = "black", alpha = 0.7, size = 3) +  # Points on top of the line
  scale_color_manual(values = c("all_movie_pct_change" = "steelblue", "adult_movie_pct_change" = "#D0E8C5")) +
  labs(
    title = "Percentage Change in All Released Movies and Adult Movies",
    x = "Year",
    y = "Percentage Change",
    color = ""
  ) +
  coord_cartesian(ylim = c(-25, 20)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )













######. Descriptive Analysis on Vote Average
# Mean and standard deviation
sd_vote_avg <- sd(all_movies$vote_average, na.rm = TRUE)

mean_vote_count <- mean(all_movies$vote_count, na.rm = TRUE)
sd_vote_count <- sd(all_movies$vote_count, na.rm = TRUE)

# Mode function
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}

mode_vote_count <- get_mode(all_movies$vote_count)


# Create 'period' variable based on the release date
filtered_movies <- all_movies %>%
  mutate(period = ifelse(year(ymd(release_date)) < 2020, "Pre-Pandemic", "Post-Pandemic"))

# Calculate mean, median, and mode for vote_average for each period
mean_vote_avg <- filtered_movies %>%
  group_by(period) %>%
  summarise(mean_vote = mean(vote_average, na.rm = TRUE))

mode_vote_avg <- filtered_movies %>%
  group_by(period) %>%
  summarise(mode_vote = get_mode(vote_average))

median_vote_avg <- filtered_movies %>%
  group_by(period) %>%
  summarise(median_vote = median(vote_average, na.rm = TRUE))

# Pre-pandemic data
pre_pandemic_data <- filtered_movies %>%
  filter(period == "Pre-Pandemic")

# Post-pandemic data
post_pandemic_data <- filtered_movies %>%
  filter(period == "Post-Pandemic")

# Pre-pandemic Histogram
pre_pandemic_data <- pre_pandemic_data %>% filter(vote_average >2)
ggplot(pre_pandemic_data, aes(x = vote_average)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(vote_average), color = "Mean"), size = 1) +
  geom_vline(aes(xintercept = median(vote_average), color = "Median"), size = 1) +
  geom_vline(aes(xintercept = get_mode(vote_average), color = "Mode"), size = 1) +
  scale_color_manual(
    name = "",
    values = c("Mean" = "red", "Median" = "green", "Mode" = "cyan"),
    labels = c("Mean", "Median", "Mode")
  ) +
  scale_x_continuous(
    breaks = seq(floor(min(pre_pandemic_data$vote_average, na.rm = TRUE)),
                 ceiling(max(pre_pandemic_data$vote_average, na.rm = TRUE)),
                 by = 0.5), 
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    title = "Pre-Pandemic Distribution of Vote Average",
    x = "Vote Average",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )



# Post-pandemic Histogram
post_pandemic_data <- post_pandemic_data %>% filter(vote_average >2)
ggplot(post_pandemic_data, aes(x = vote_average)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(vote_average), color = "Mean"), size = 1) +
  geom_vline(aes(xintercept = median(vote_average), color = "Median"), size = 1) +
  geom_vline(aes(xintercept = get_mode(vote_average), color = "Mode"), size = 1) +
  scale_color_manual(
    name = "",
    values = c("Mean" = "lightcoral", "Median" = "green", "Mode" = "cyan"),
    labels = c("Mean", "Median", "Mode")
  ) +
  scale_x_continuous(
    breaks = seq(floor(min(post_pandemic_data$vote_average, na.rm = TRUE)),
                 ceiling(max(post_pandemic_data$vote_average, na.rm = TRUE)),
                 by = 0.5), 
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    title = "Post-Pandemic Distribution of Vote Average",
    x = "Vote Average",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )








# Histogram for vote_count
ggplot(filtered_movies, aes(x = vote_count)) +
  geom_histogram(binwidth = 50, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Vote Count", x = "Vote Count Category", y = "Frequency") +
  coord_cartesian(xlim = c(0, 3000)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
  




# Create histograms for Pre and Post Pandemic periods -- VOTE COUNT
ggplot(filtered_movies %>% filter(vote_count> 20, vote_count < 1000), aes(x = vote_count)) +
  geom_histogram(binwidth = 50, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Vote Count", x = "Vote Count Category", y = "Frequency") +
  coord_cartesian(xlim = c(20, 1000)) +
  facet_wrap(~period) +  # Facet by the 'period' variable
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )




# Create histograms for Pre and Post Pandemic -- VOTE AVERAGE

ggplot(filtered_movies %>% filter(vote_average > 2), aes(x = vote_average)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(
    breaks = seq(floor(min(filtered_movies$vote_average, na.rm = TRUE)),
                 ceiling(max(filtered_movies$vote_average, na.rm = TRUE)),
                 by = 1), 
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    title = "Distribution of Vote Average",
    x = "Vote Average",
    y = "Frequency"
  ) +
  theme_minimal() +
  facet_wrap(~period) +  # Facet by 'period' variable (Pre-Pandemic and Post-Pandemic)
  theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))

























# Scatter plot: vote_average vs vote_count
ggplot(filtered_movies, aes(x = vote_average, y = vote_count)) +
  geom_point(color = "purple", alpha = 0.5) +
  labs(title = "Distribution of Vote Average and Vote Count", x = "Vote Average", y = "Vote Count")


# Create a 'period' variable for pre and post pandemic based on release date
filtered_movies <- filtered_movies %>%
  mutate(period = ifelse(year(ymd(release_date)) < 2020, "Pre-Pandemic", "Post-Pandemic"))

# Scatter plot with color distinction for pre and post pandemic
ggplot(filtered_movies %>% filter(vote_count < 1000, vote_average > 2, vote_average < 9), aes(x = vote_average, y = vote_count, color = period)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Distribution of Vote Average and Vote Count by Period",
    x = "Vote Average",
    y = "Vote Count",
    color = "Period"
  ) +
  scale_color_manual(values = c("Pre-Pandemic" = "steelblue", "Post-Pandemic" = "lightcoral")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face ="bold", hjust = 0.5)
  )











# ------>
# Group by pre/post-pandemic based on release_date
grouped_summary <- all_movies %>%
  mutate(period = ifelse(year(ymd(release_date)) < 2020, "Pre-Pandemic", "Post-Pandemic")) %>%
  group_by(period) %>%
  summarise(
    mean_vote_avg = mean(vote_average, na.rm = TRUE),
    sd_vote_avg = sd(vote_average, na.rm = TRUE),
    mean_vote_count = mean(vote_count, na.rm = TRUE),
    sd_vote_count = sd(vote_count, na.rm = TRUE)
  )

# Print grouped summary
print(grouped_summary)

# Boxplot for vote_average by period
ggplot(all_movies %>% 
         mutate(period = ifelse(year(ymd(release_date)) < 2020, "Pre-Pandemic", "Post-Pandemic")),
       aes(x = period, y = vote_average, fill = period)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Vote Average by Period", x = "Period", y = "Vote Average") +
  coord_flip() + # Flip the coordinates to make the plot horizontal
  theme_minimal()
# <------









# Assuming all_movies dataset has been defined and includes release_date and original_language
# Add the 'period' column
all_movies <- all_movies %>%
  mutate(
    period = ifelse(year(ymd(release_date)) < 2020, "Pre-Pandemic", "Post-Pandemic")  # Define period
  ) %>%
  filter(!is.na(original_language))  # Remove rows with invalid original_language

# Aggregate movie counts by original_language and period
language_frequency <- all_movies %>%
  group_by(original_language, period) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  filter(frequency >= 100)  # Remove languages with fewer than 10 movies

# Create the bar plot for Descriptive Analysis
ggplot(language_frequency, aes(x = reorder(original_language, frequency), y = frequency, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Distribution of Movie Languages (Pre vs Post Pandemic)",
    x = "Original Language",
    y = "Number of Movies"
  ) +
  scale_fill_manual(values = c("Pre-Pandemic" = "steelblue", "Post-Pandemic" = "lightcoral")) +  # Custom colors
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "top"
  )
















###### Word Cloud for TITLE
library(NLP)
library(RColorBrewer)
library(tm)
library(wordcloud)

# Filter data for pre-pandemic movies
pre_pandemic_data <- all_movies %>% filter(year(ymd(release_date)) < 2020)

# Combine the titles of all pre-pandemic movies
titles_text <- paste(pre_pandemic_data$title, collapse = " ")

# Create a text corpus from the titles
corpus <- Corpus(VectorSource(titles_text))

# Apply transformations to the corpus
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert text to lowercase
corpus <- tm_map(corpus, removePunctuation)             # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                 # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Remove common stopwords

# Create a term-document matrix (TDM) to check the frequency of words
tdm <- TermDocumentMatrix(corpus)

# Convert the TDM to a matrix
m <- as.matrix(tdm)

# Get the frequency of each word
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Create a data frame of words and their frequencies
word_freq_table <- data.frame(word = names(word_freqs), freq = word_freqs)

# Create a word cloud
wordcloud(
  words = word_freq_table$word, 
  freq = word_freq_table$freq, 
  scale = c(3, 0.5), 
  max.words = 100, 
  random.order = FALSE, 
  colors = brewer.pal(8, "Dark2"),
  main = "Word Cloud for Pre-Pandemic Movies"
  )






# Filter data for post-pandemic movies
post_pandemic_data <- all_movies %>% filter(year(ymd(release_date)) >= 2020)

# Combine the titles of all post-pandemic movies
titles_text <- paste(post_pandemic_data$title, collapse = " ")

# Create a text corpus from the titles
corpus <- Corpus(VectorSource(titles_text))

# Apply transformations to the corpus
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert text to lowercase
corpus <- tm_map(corpus, removePunctuation)             # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                 # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Remove common stopwords

# Create a term-document matrix (TDM) to check the frequency of words
tdm <- TermDocumentMatrix(corpus)

# Convert the TDM to a matrix
m <- as.matrix(tdm)

# Get the frequency of each word
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Create a data frame of words and their frequencies
word_freq_table <- data.frame(word = names(word_freqs), freq = word_freqs)

# Create a word cloud
wordcloud(
  words = word_freq_table$word, 
  freq = word_freq_table$freq, 
  scale = c(3, 0.5), 
  max.words = 100, 
  random.order = FALSE, 
  colors = brewer.pal(8, "Dark2"),
  main = "Word Cloud for Post-Pandemic Movies"
)







###### Word Cloud -- OVERVIEW


# Filter data for pre-pandemic movies
pre_pandemic_data <- all_movies %>% filter(year(ymd(release_date)) < 2020)

# Combine the overviews of all pre-pandemic movies
overview_text <- paste(pre_pandemic_data$overview, collapse = " ")

# Create a text corpus from the overviews
corpus <- Corpus(VectorSource(overview_text))

# Apply transformations to the corpus
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert text to lowercase
corpus <- tm_map(corpus, removePunctuation)             # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                 # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Remove common stopwords

# Create a term-document matrix (TDM) to check the frequency of words
tdm <- TermDocumentMatrix(corpus)

# Convert the TDM to a matrix
m <- as.matrix(tdm)

# Get the frequency of each word
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Create a data frame of words and their frequencies
word_freq_table <- data.frame(word = names(word_freqs), freq = word_freqs)

# Create a word cloud
wordcloud(
  words = word_freq_table$word, 
  freq = word_freq_table$freq, 
  scale = c(2.5, 0.3), 
  max.words = 100, 
  random.order = FALSE, 
  colors = brewer.pal(8, "Dark2"),
  main = "Word Cloud for Pre-Pandemic Movies (Overview)"
)





# Filter data for post-pandemic movies
post_pandemic_data <- all_movies %>% filter(year(ymd(release_date)) >= 2020)

# Combine the overviews of all post-pandemic movies
overview_text <- paste(post_pandemic_data$overview, collapse = " ")

# Create a text corpus from the overviews
corpus <- Corpus(VectorSource(overview_text))

# Apply transformations to the corpus
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert text to lowercase
corpus <- tm_map(corpus, removePunctuation)             # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                 # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Remove common stopwords

# Create a term-document matrix (TDM) to check the frequency of words
tdm <- TermDocumentMatrix(corpus)

# Convert the TDM to a matrix
m <- as.matrix(tdm)

# Get the frequency of each word
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Create a data frame of words and their frequencies
word_freq_table <- data.frame(word = names(word_freqs), freq = word_freqs)

# Create a word cloud
wordcloud(
  words = word_freq_table$word, 
  freq = word_freq_table$freq, 
  scale = c(2.5, 0.3), 
  max.words = 100, 
  random.order = FALSE, 
  colors = brewer.pal(8, "Dark2"),
  main = "Word Cloud for Post-Pandemic Movies (Overview)"
)






####### Genre

# 1. Tüm genre_ids'leri bir array'e dönüştür
all_genres <- all_movies %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Virgül ile ayırarak listeye çevir
  unnest(genre_ids) %>%  # Listeyi patlat
  mutate(genre_ids = as.numeric(genre_ids)) %>%  # ID'leri sayıya çevir
  pull(genre_ids)  # Tüm ID'leri bir vektör olarak çek

genre_frequencies <- table(all_genres)

genre_data <- as.data.frame(genre_frequencies)




ggplot(genre_data, aes(x = as.factor(all_genres), y = Freq)) +
  geom_bar(stat = "identity", fill = "darkseagreen1", color = "black") +
  theme_minimal() +
  labs(title = "Genre Distribution",
       x = "Genres",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    labels = genre_mapping
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )




library(dplyr)
library(ggplot2)
library(tidyr)

# Filter data for pre-pandemic movies
pre_pandemic_data <- all_movies %>% filter(year(ymd(release_date)) < 2020)

# Extract and process genre IDs
pre_pandemic_genres <- pre_pandemic_data %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre IDs by comma
  unnest(genre_ids) %>%                            # Flatten the list
  mutate(genre_ids = as.numeric(genre_ids)) %>%    # Convert to numeric
  pull(genre_ids)                                  # Pull as a vector

# Calculate genre frequencies
pre_pandemic_frequencies <- table(pre_pandemic_genres)
pre_pandemic_genre_data <- as.data.frame(pre_pandemic_frequencies)

# Create the bar plot
ggplot(pre_pandemic_genre_data, aes(x = as.factor(pre_pandemic_genres), y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Pre-Pandemic Genre Frequencies",
       x = "Genre",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    labels = genre_mapping
  )




# Filter data for post-pandemic movies
post_pandemic_data <- all_movies %>% filter(year(ymd(release_date)) >= 2020)

# Extract and process genre IDs
post_pandemic_genres <- post_pandemic_data %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%  # Split genre IDs by comma
  unnest(genre_ids) %>%                            # Flatten the list
  mutate(genre_ids = as.numeric(genre_ids)) %>%    # Convert to numeric
  pull(genre_ids)                                  # Pull as a vector

# Calculate genre frequencies
post_pandemic_frequencies <- table(post_pandemic_genres)
post_pandemic_genre_data <- as.data.frame(post_pandemic_frequencies)

# Create the bar plot
ggplot(post_pandemic_genre_data, aes(x = as.factor(post_pandemic_genres), y = Freq)) +
  geom_bar(stat = "identity", fill = "lightcoral", color = "black") +
  theme_minimal() +
  labs(title = "Post-Pandemic Genre Frequencies",
       x = "Genre",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    labels = genre_mapping
  )




# Combine the datasets with a new column to indicate the period
pre_pandemic_genre_data$Period <- "Pre-Pandemic"
post_pandemic_genre_data$Period <- "Post-Pandemic"

combined_data <- rbind(
  pre_pandemic_genre_data %>% rename(Genres = pre_pandemic_genres),
  post_pandemic_genre_data %>% rename(Genres = post_pandemic_genres)
)

# Create the ggplot with facets
ggplot(combined_data, aes(x = as.factor(Genres), y = Freq, fill = Period)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("Pre-Pandemic" = "lightblue", "Post-Pandemic" = "lightcoral")) +
  theme_minimal() +
  labs(
    title = "Genre Frequencies (Pre and Post Pandemic)",
    x = "Genre",
    y = "Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    labels = genre_mapping
  ) +
  scale_y_continuous(breaks = seq(0, max(combined_data$Freq, na.rm = TRUE), by = 300)) +
  facet_wrap(~ Period, ncol = 2, scales = "free_x") +
  theme(
    plot.title = element_text(face =  "bold", hjust = 0.5)
  )













library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

# Pre-Pandemic Genre Ratios
pre_pandemic_data <- all_movies %>% filter(year(ymd(release_date)) < 2020)

pre_pandemic_genres <- pre_pandemic_data %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%
  unnest(genre_ids) %>%
  mutate(genre_ids = as.numeric(genre_ids)) %>%
  count(genre_ids) %>%
  mutate(period = "Pre-Pandemic") %>%
  mutate(ratio = n / sum(n))  # Calculate ratios

# Post-Pandemic Genre Ratios
post_pandemic_data <- all_movies %>% filter(year(ymd(release_date)) >= 2020)

post_pandemic_genres <- post_pandemic_data %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%
  unnest(genre_ids) %>%
  mutate(genre_ids = as.numeric(genre_ids)) %>%
  count(genre_ids) %>%
  mutate(period = "Post-Pandemic") %>%
  mutate(ratio = n / sum(n))  # Calculate ratios

# Combine the datasets and filter for top 10 genres
combined_genres <- bind_rows(pre_pandemic_genres, post_pandemic_genres) %>%
  group_by(genre_ids) %>%
  summarise(total_movies = sum(n)) %>%
  top_n(10, total_movies) %>%  # Select top 10 genres
  left_join(combined_genres, by = "genre_ids")

# Map genre IDs to their names
combined_genres <- all_movies %>%
  mutate(genre_name = recode(as.character(genre_ids), !!!genre_mapping))

# Create the proportional comparison bar plot
ggplot(combined_genres, aes(x = period, y = ratio, fill = genre_name)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  theme_minimal() +
  labs(
    title = "Top 10 Movie Genres: Pre-Pandemic vs Post-Pandemic",
    x = "Period",
    y = "Proportion",
    fill = "Genre"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Paired") +  # Use a paired color palette for clarity
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(face = "bold")
  )









# Pre-Pandemic Genre Ratios
pre_pandemic_data <- all_movies %>% filter(year(ymd(release_date)) < 2020)

pre_pandemic_genres <- pre_pandemic_data %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%
  unnest(genre_ids) %>%
  mutate(genre_ids = as.numeric(genre_ids)) %>%
  count(genre_ids) %>%
  mutate(period = "Pre-Pandemic") %>%
  mutate(ratio = n / sum(n))  # Calculate ratios

# Post-Pandemic Genre Ratios
post_pandemic_data <- all_movies %>% filter(year(ymd(release_date)) >= 2020)

post_pandemic_genres <- post_pandemic_data %>%
  mutate(genre_ids = strsplit(genre_ids, ",")) %>%
  unnest(genre_ids) %>%
  mutate(genre_ids = as.numeric(genre_ids)) %>%
  count(genre_ids) %>%
  mutate(period = "Post-Pandemic") %>%
  mutate(ratio = n / sum(n))  # Calculate ratios

# Combine the datasets and filter for top 10 genres
combined_genres <- bind_rows(pre_pandemic_genres, post_pandemic_genres) %>%
  group_by(genre_ids) %>%
  summarise(total_movies = sum(n)) %>%
  top_n(10, total_movies)  # Select top 10 genres

# Now, create a combined dataset with the 'ratio' column from both periods
combined_genres <- bind_rows(pre_pandemic_genres, post_pandemic_genres) %>%
  filter(genre_ids %in% combined_genres$genre_ids) %>%
  mutate(genre_name = recode(as.character(genre_ids), !!!genre_mapping))

# Create the proportional comparison bar plot
ggplot(combined_genres, aes(x = period, y = ratio, fill = genre_name)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  theme_minimal() +
  labs(
    title = "Top 10 Movie Genres: Pre-Pandemic vs Post-Pandemic",
    x = "Period",
    y = "Proportion",
    fill = "Genre"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Paired") +  # Use a paired color palette for clarity
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(face = "bold")
  )










