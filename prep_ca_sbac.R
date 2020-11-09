library(tidyverse)
library(magrittr)
library(tidyselect)

# read in the data
all <- read.csv("data/ca_sbac.csv")

# filter for state level data from all grade levels
interest <- all %>%
  filter(institution_type == "state" & 
           grade == "all") 


# select columns containing # students, pct met and above values, & dimensions
tab_prep <- interest %>%
  select(group_id, subgroup_id, test_id, students_tested, pct_met_and_above, area_1_percentage_above_standard, area_2_percentage_above_standard, area_3_percentage_above_standard, area_4_percentage_above_standard)


# pivot the data into tody format, so all % above or met value is in a single column
tab_prep_long <- tab_prep %>%
  pivot_longer(contains("above"), names_to = "test_type", values_to = "pct_met_and_above")

# get different test names
levels(as.factor(tab_prep$test_name))


# add a single column containing correct test names and remove the old two
tab_prep %>%
  mutate(test_name = case_when(
    test_id == "SB_ELA" & test_type == "pct_met_and_above" ~ "English - Overall",
    test_id == "SB_ELA" & test_type == "area_1_percentage_above_standard" ~ "English - Overall",
    test_id == "SB_ELA" & test_type == "area_2_percentage_above_standard" ~ "English - Overall",
    test_id == "SB_ELA" & test_type == "area_3_percentage_above_standard" ~ "English - Overall",
    test_id == "SB_ELA" & test_type == "area_4_percentage_above_standard" ~ "English - Overall",
    x %% 5 == 0 ~ "fizz",
    x %% 7 == 0 ~ "buzz",
    TRUE ~ as.character(x)
  ))

#subgroup_id and test_type


  