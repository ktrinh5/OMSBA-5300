library(tidyverse)
library(rio)
library(lubridate)

# List all trends files
trend_files <- list.files(pattern = "^trends_up_to_.*\\.csv$")

# Import and combine them
trends_all <- import_list(trend_files, rbind = TRUE)

# Import the link file
id_link <- import("id_name_link.csv")

# Import the scorecard
scorecard <- import("Most+Recent+Cohorts+(Scorecard+Elements).csv")
view(scorecard)

## Trend files data cleaning
# Then convert date string to an actual Date object
trends_all <- trends_all %>%
  mutate(date = ymd(str_sub(monthorweek, 1, 10)))

# Go by the month
trends_all <- trends_all %>%
  mutate(month = floor_date(date, unit = "month"))
view(trends_all)

#standardize index within each schname and keyword
trends_all <- trends_all %>%
  group_by(schname, keyword) %>%
  mutate(new_index = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)) %>%
  ungroup()

## scorecard data cleaning
scorecard_clean <- scorecard %>%
  select(
    UNITID,
    OPEID,
    INSTNM,
    PREDDEG,
    `md_earn_wne_p10-REPORTED-EARNINGS`
  ) %>%
  rename(
    schname = INSTNM,
    degree_type = PREDDEG,
    earnings = `md_earn_wne_p10-REPORTED-EARNINGS`
  ) %>%
  filter(degree_type == 3) %>%
  filter(!is.na(earnings) & earnings != "PrivacySuppressed") %>%
  mutate(earnings = as.numeric(earnings))

#lowercase key variable to match with other dataset
scorecard_clean <- scorecard_clean %>%
  rename(unitid = UNITID, opeid = OPEID)

## id_link dataset cleaning
id_link_clean <- id_link %>%
  group_by(schname) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  select(-n)

## joining part
trends_id <- inner_join(trends_all, id_link_clean, by = "schname")
trends_scorecard_final <- inner_join(trends_id, scorecard_clean, by = "unitid")


