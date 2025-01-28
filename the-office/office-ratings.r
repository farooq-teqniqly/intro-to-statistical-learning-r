library(tidyverse)

office_ratings <- read_csv(
  paste0(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
    "master/data/2020/2020-03-17/office_ratings.csv"
  )
)

head(office_ratings, n = 5)
