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



sample_data <- testing_data %>%  slice(10) %>%  select(matches(paste0('Zr_',REE_plus_Y_Elements, '_ppm'))) %>%  rename_with(~str_remove_all(.x, '^Zr_|_ppm$'))

`%notin%` <- Negate(`%in%`)
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel("Calculate REE"),


sidebarLayout(
  sidebarPanel(
    ### test
    selectInput(inputId = 'test', label = 'asdfadsfa', choices = '12', selectize = F
    ),

    ###load_file
    fileInput(inputId = 'newFile', label = 'Upload File', ),

    ### select REE to model
    checkboxGroupInput(inputId = 'NormalizeMethod' ,label = 'Chondrite Values', choices = REE_plus_Y_Elements, selected = REE_plus_Y_Elements[REE_plus_Y_Elements %notin% c('La','Ce','Eu','Y')]),
    ###impute?
    checkboxInput(inputId = 'impute' ,label = 'Impute?', value = TRUE),

    ### chondrite_values_from
    radioButtons(inputId = 'chondrite_values',label = 'Chondrite Values',
                 choices = c('PalmeOneill2014CI','McDonough1995CI')),

    ### Print modelled REE TODO change for excluded elements
    textOutput('Rees')


  ),
  mainPanel(

    plotOutput('R2_plot' ),
    dataTableOutput('Result'),
  )

)

    )


# Define server logic required to draw a histogram
server <- function(input, output) {


  thematic::thematic_shiny()  ## makes plots to match html theme


  ### calculata missign data
  modelled_data <- reactive({

   REE_to_model <- REE_plus_Y_Elements[REE_plus_Y_Elements %notin% input$NormalizeMethod]
   print(REE_to_model)


   data <- read_csv(input$newFile$datapath)
   data <- data  %>%  model_REE(., exclude = REE_to_model, method = !!sym(input$chondrite_values))
   if (input$impute) {
     data <- data %>%  impute_REE()
   }

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

  modelled_data() %>%  ggplot(aes(model_r.squared)) + geom_histogram()+ geom_density() + scale_x_continuous(trans = 'logit')
})
}

# Run the application
shinyApp(ui = ui, server = server)
# testing_data %>%  slice(1:50) %>%  select(matches(paste0('Zr_',REE_plus_Y_Elements, '_ppm'))) %>%  rename_with(~str_remove_all(.x, '^Zr_|_ppm$')) %>%  write_csv('test.csv')
# testing_data %>% model_REE(prefix = 'Zr', suffix = 'ppm') %>%  glimpse()
