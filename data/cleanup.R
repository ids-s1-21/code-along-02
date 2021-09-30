#Cleanup script
#Included for reference, but *you do not need to run this* to follow the
#code-along
library(tidyverse)
library(lubridate)
chart_data <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv"
)
features <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv"
)

chart_data_clean <- chart_data %>%
  mutate(chart_date = mdy(week_id))
  
songs <- chart_data_clean %>%
  mutate(
    date_if_no_1 = case_when(
      week_position == 1 ~ chart_date,
      TRUE               ~ as.Date(NA)
    )
  ) %>%
  group_by(song_id, song, performer) %>%
  summarise(
    peak_position = min(week_position),
    times_on_chart = max(instance),
    weeks_on_chart = n(),
    first_appearance = min(chart_date),
    last_appearance = max(chart_date),
    is_number_one = (min(week_position) == 1),
    weeks_at_number_one = sum(week_position == 1),
    first_number_one = case_when(
      min(date_if_no_1, na.rm = TRUE) == Inf ~ as.Date(NA),
      TRUE                                   ~ min(date_if_no_1, na.rm = TRUE)
    ),
    last_number_one = case_when(
      max(date_if_no_1, na.rm = TRUE) == -Inf ~ as.Date(NA),
      TRUE                                    ~ max(date_if_no_1, na.rm = TRUE)
    ),
    .groups = "drop"
  )

songs_full <- songs %>%
  left_join(
    features,
    by = c("song_id", "song", "performer")
  )

saveRDS(chart_data_clean, file = "data/billboard.rds")
saveRDS(songs_full, file = "data/songs.rds")
