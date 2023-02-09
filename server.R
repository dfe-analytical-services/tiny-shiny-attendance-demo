server <- function(input, output, session) {
  output$chart <- renderPlot({
    plot_data <- data %>% filter(area == input$filter1, phase == input$filter2)
    ggplot(plot_data, aes(x = time_period, y = values, fill = values)) +
      geom_col() +
      theme_classic()
  })
}
