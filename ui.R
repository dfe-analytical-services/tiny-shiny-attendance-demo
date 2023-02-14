ui <- function(input, output, session) {
  fluidPage(
      fluidRow(
        width = 12,
        column(
          width=3,
          selectInput(inputId = "geography_choice",
                         label = "Choose geographic level:",
                         choices = c("National","Regional","Local authority"),
                         selected = head(geog_levels,1)
          )
        ),
        column(
          width=3,
          selectInput(inputId = "region_choice",
                         label = "Choose region:",
                         choices = geog_lookup %>% dplyr::filter(geographic_level == 'National') %>% dplyr::select(region_name) %>% unique() %>% as.data.table(),
                         #selected = head(reg_geog,1)
          )
        ),
        column(
          width=3,
          selectInput(inputId = "la_choice",
                         label = "Choose local authority:",
                         choices = geog_lookup %>% dplyr::filter(region_name == "East Midlands") %>% dplyr::select(la_name) %>% unique() %>% as.data.table(),
                         #selected = la_geog()[2,1]
          )
        ),
        column(
          width=3,
          selectInput(inputId = "school_choice",
                         label = "Choose school type:",
                         choices = school_type_lookup %>% dplyr::filter(geographic_level == "National") %>% dplyr::select(school_type) %>% unique() %>% as.data.table(),
                         selected = "Primary"
          )
        )
      ),
      fluidRow(
        column(
          12,
          plotlyOutput("absence_rates_timeseries_plot")
        )
      )
  )
}
