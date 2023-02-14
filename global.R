shhh <- suppressPackageStartupMessages
shhh(library(shinyGovstyle))
shhh(library(shiny))
shhh(library(ggplot2))
shhh(library(dplyr))
shhh(library(styler))
shhh(library(data.table))
shhh(library(stringr))
shhh(library(plotly))

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  # message("R scripts")
  # message("----------------------------------------")
  # r_scripts <- eval(styler::style_dir("R/")$changed)
  # message("Test scripts")
  # message("----------------------------------------")
  # test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts) # , r_scripts, test_scripts)
  return(script_changes)
}

attendance_data <- read.csv(file = "ees_weekly_data_.csv") %>%
  as.data.frame() %>%
  mutate(time_identifier = str_remove_all(time_identifier, "Week ")) %>% 
  mutate_at(c(1:2, 12:63), as.numeric) %>%
  filter(time_period == '2022')

# Add geog lookup
geog_lookup <- attendance_data %>%
  dplyr::select(geographic_level, region_name, la_name) %>%
  unique() %>%
  arrange(region_name, la_name)

geog_levels <- geog_lookup %>%
  dplyr::select(geographic_level) %>%
  unique() %>%
  as.data.table()

school_type_lookup <- attendance_data %>%
  dplyr::select(geographic_level, school_type) %>%
  unique() %>%
  arrange(geographic_level, school_type)
