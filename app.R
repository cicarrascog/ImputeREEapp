#a
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(vctrs)
library(shiny)
library(tidyverse)
library(imputeREE)
library(reactable)
library(reactablefmtr)


# options(shiny.ragg = TRUE)


`%notin%` <- Negate(`%in%`)
# test_data <- testing_data %>%  slice(1:50) %>%  select(matches(paste0('Zr_',REE_plus_Y_Elements, '_ppm'))) %>%  rename_with(~str_remove_all(.x, '^Zr_|_ppm$')) %>%  write_csv('test.csv')

sample_data <- testing_data %>%
  slice(1:10) %>%
  select(matches(paste0("Zr_", REE_plus_Y_Elements, "_ppm"))) %>%
  rename_with(~ str_remove_all(.x, "^Zr_|_ppm$"))






ui <- fluidPage(
  # theme = bslib::bs_theme(bootswatch = "journal"),
 # shinythemes::themeSelector(),
  theme = shinythemes::shinytheme('journal'),
  titlePanel("ImputeREEapp"),
  sidebarLayout(
    ############### SIDEBAR #####################
    sidebarPanel(

      tableOutput("testing"),

      sliderInput(inputId = 'Y', label = 'Y correction',  value = 1.29,     min = 0.1, max = 2 ),
      sliderInput(inputId = 'Yb',label = 'Yb correction', value = 1/0.8785, min = 0.1, max = 2  ),
      sliderInput(inputId = 'Lu',label = 'Lu correction', value = 1/0.8943, min = 0.1, max = 2 ),


      ### load_file ##############
      fileInput(inputId = "newFile", label = NULL, accept = c(".csv"), buttonLabel = "Upload...", placeholder = "a .csv file"),


      ### Print modelled REE ############
      textOutput("Rees"),


      ### select REE to model #############
      checkboxGroupInput(inputId = "NormalizeMethod", label = "Chondrite Values", choices = REE_plus_Y_Elements, selected = REE_plus_Y_Elements[REE_plus_Y_Elements %notin% c("La", "Ce", "Eu", "Y")]),
      ############################

      ### impute? ################
      checkboxInput(inputId = "impute", label = "Impute?", value = FALSE),
      ### Include_norm? ################
      checkboxInput(inputId = "include_norm_values", label = "Include Chondrite Normalized Values?", value = FALSE),


      ### chondrite_values_from #############
      radioButtons(
        inputId = "chondrite_values", label = "Chondrite Values",
        choices = c(
          `Palme and O'Neill (2014)` = "PalmeOneill2014CI",
          `McDonough and Sun (1995)` = "McDonough1995CI"
        )
      ),
      downloadButton(outputId = "donwload_data"),
      ################################

      ############ Main Panel ########################
    ),
    mainPanel(
      plotOutput("R2_plot"),
      plotOutput("REE_plot"),
      plotOutput("ratio_plot"),
      reactableOutput("Result"),
      tableOutput('REE')
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  thematic::thematic_shiny(font = thematic::font_spec(families = 'helvetica')) ## makes plots to match html theme

  # Check_names <- reactive({
  #
  # })

  sample_data <- reactive({

    testing_data %>%
      slice(1:10) %>%
      select(matches(paste0("Zr_", REE_plus_Y_Elements, "_ppm"))) %>%
      rename_with(~ str_remove_all(.x, "^Zr_|_ppm$"))
  })


  ### MODEL REEE #########

  base_data <- reactive({


    # req(input$newFile$datapath)


    if (is.null(input$newFile)) {
      data <- sample_data()
    } else{
      data <- read_csv(input$newFile$datapath)
    }



    REE_to_model <- REE_plus_Y_Elements[REE_plus_Y_Elements %notin% input$NormalizeMethod]
    print(REE_to_model)


    # data <- read_csv(input$newFile$datapath)

    check_columns <- REE_plus_Y_Elements[REE_plus_Y_Elements %notin% names(data)]

    if (length(check_columns > 0)) {
      data[, check_columns] <- NA_real_
      data <- data %>% relocate(REE_plus_Y_Elements)
    }


    normalized_data <- data %>% imputeREE:::Element_norm(method = !!sym(input$chondrite_values))

    data <- data %>% model_REE(., exclude = REE_to_model, method = !!sym(input$chondrite_values), correct_heavy = F)
    complete_data <- left_join(data, normalized_data, by = "rowid") %>%
      relocate(
        rowid, REE_plus_Y_Elements,
        matches("_Normalized$"), matches("^ppmCalc_"), matches("^NormalizedCalc_")
      )

    return(complete_data)
  })


  modelled_data <- reactive({
    data <- base_data() %>%  imputeREE:::correct_heavy(Y_correction_fact = input$Y ,
                                                       Yb_correction_fact = input$Yb,
                                                       Lu_correction_fact = input$Lu)

    return(data)
  })

  ###  Create long format with normalized values
  norm_table <- reactive({

   data <-  modelled_data() %>%  select(rowid, matches('Normalized')) %>%  rename_with(.cols = matches('Normalized$'), ~paste0('Normalized_',str_remove_all(.x, '_Normalized$'))) %>%
      pivot_longer(cols = matches('Normalized'), names_to = c('.value','Element_name'), names_sep = '_') %>%  imputeREE:::add_IonicRadii() %>%  rename(`Ionic Radius` = ShannonRadiiVIII_Coord_3plus) %>%  mutate(Ratio = NormalizedCalc/Normalized)

  data_long <-  data %>%  select(-Ratio) %>%  pivot_longer(cols = c('NormalizedCalc', 'Normalized'), names_to = 'Data Type', values_to = 'Chondrite Normalized')
    return(list(data_long, data))

  })


  ######### Excluded REE ######
  output$Rees <- renderText({
    paste("Excluded from model:", paste(" ", REE_plus_Y_Elements[REE_plus_Y_Elements %notin% input$NormalizeMethod], collapse = " , "))
  })

  ####### render reactable with all the data ##############

  output$Result <- renderReactable({
    data <- modelled_data()
    if (input$include_norm_values == FALSE) {
      data <- data %>% select(!matches("^NormalizedCalc|Normalized"))
    }

    if (input$impute) {
      data <- data %>% impute_REE()
    }

    reactable(data,
      defaultColDef = colDef(format = reactable::colFormat(digits = 3)),
      columns = list(
      rowid =  colDef(name = 'ID', format = colFormat(digits = 0))
      ),

      resizable = T,
      searchable = T,
      highlight = T,
      onClick = "select",
      selection = "single",
      defaultSelected = 1,

    )
  })

####### summary table #########
  output$REE <- renderTable({

      req(getReactableState("Result")$selected)
      id_num <- getReactableState("Result")$selected

    # req(getReactableState("Result")$selected)


    norm_table()[[2]] %>% filter(rowid == id_num)
  })

# Plots ######


  output$REE_plot <- renderPlot({
    breaks <- imputeREE::Element_Data %>% filter(Element_name %in% REE_plus_Y_Elements)
    labels <- breaks$Element_name
    breaks <- breaks$ShannonRadiiVIII_Coord_3plus
    req(getReactableState("Result")$selected)

      id_num <- getReactableState("Result")$selected

    norm_table()[[1]] %>% filter(rowid == id_num) %>%
      ggplot(aes(x = `Ionic Radius`, y = `Chondrite Normalized`, group = `Data Type`, color = `Data Type`)) +
      theme(legend.position = c(0.95,0.05),
            legend.justification = c(0.95,0.05)) +
      geom_point() + geom_line() +
      scale_y_log10() +
      scale_x_reverse(breaks = breaks, labels = labels)
  })

###### Rsquared Histogram
  output$R2_plot <- renderPlot({
    modelled_data() %>% ggplot(aes(model_r.squared)) +
      geom_histogram() +
      geom_density() +
      scale_x_continuous(trans = "logit")
  })

  output$ratio_plot <- renderPlot({
    norm_table()[[2]] %>%  ggplot(aes(y = Ratio, x = Element_name)) + geom_boxplot() + scale_y_log10()
  })


#### downloads ####

  output$donwload_data <- downloadHandler(
    filename = function() {
      paste0(input$dataset, "calc.csv")
    },
    content = function(file) {
      write.csv(modelled_data(), file)
    }
  )


}

# Run the application
shinyApp(ui = ui , server = server)
