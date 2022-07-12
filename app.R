#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(imputeREE)
`%notin%` <- Negate(`%in%`)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Calculate REE"),
    fileInput(inputId = 'newFile', label = 'Upload File'),
    checkboxGroupInput(inputId = 'NormalizeMethod' ,label = 'Chondrite Values', choices = REE_plus_Y_Elements, selected = REE_plus_Y_Elements[REE_plus_Y_Elements %notin% c('La','Ce','Eu','Y')], inline = T),
    # Sidebar with a slider input for number of bins

        # Show a plot of the generated distribution
        mainPanel(
           textOutput('Rees'),
           dataTableOutput('Result'),
           plotOutput('R2_plot')
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

  modelled_data <- reactive({
   REE_to_model <- REE_plus_Y_Elements[REE_plus_Y_Elements %notin% input$NormalizeMethod]
   print(REE_to_model)
   data <- read_csv(input$newFile$datapath)
   data <- data  %>%  model_REE(., exclude = REE_to_model)
   return(data)
 })


    output$Rees <- renderText({
       input$NormalizeMethod

    })
    output$Result <- renderDataTable({

      modelled_data()


    }
    )
output$R2_plot <- renderPlot({

  modelled_data() %>%  ggplot(aes(model_r.squared)) + geom_histogram()
})
}

# Run the application
shinyApp(ui = ui, server = server)
# testing_data %>%  slice(1:50) %>%  select(matches(paste0('Zr_',REE_plus_Y_Elements, '_ppm'))) %>%  rename_with(~str_remove_all(.x, '^Zr_|_ppm$')) %>%  write_csv('test.csv')
# testing_data %>% model_REE(prefix = 'Zr', suffix = 'ppm') %>%  glimpse()
