ui <- function(input, output, session) {
  fluidPage(
      fluidRow(
        width = 12,
        column(
          width=6,
          selectizeInput("filter1",
            label = "Filter 1",
            choices = c("England", "North East", "Yorkshire and the Humber"),
            selected = "England"
          )
        ),
        column(
          width=6,
          selectizeInput("filter2",
            label = "Filter 2",
            choices = c("All", "Primary", "Secondary"),
            selected = "All"
          )
        )
      ),
      fluidRow(
        column(
          12,
          plotOutput("chart")
        )
      )
  )
}
