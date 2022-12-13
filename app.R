# a
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(scipen=100000)
library(shiny)
library(tidyverse)
library(imputeREE)
library(reactable)
library(reactablefmtr)
library(markdown)
library(bs4Dash)
`%notin%` <- Negate(`%in%`)


# UI Section ---------

header <- bs4DashNavbar(
  title = dashboardBrand(
    title = "ImputeREEapp",
    color = "primary"
    # href = "https://adminlte.io/themes/v3",
    # image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
  ), border = F,
  skin = "dark"
)

## sidebar pages definition ------
sidebar <- dashboardSidebar(
  title = "ImputeREEapp",
  sidebarMenu(
    menuItem(text = "Welcome", tabName = "Instructions"),
    menuItem(text = "Upload", tabName = "Upload"),
    menuItem(text = "Data", tabName = "Data"),
    menuItem(text = "Download", tabName = "Download"),
    menuItem(text = "References", tabName = "References"),
    menuItem(text = "Changelog", tabName = "Changelog"),
    menuItem(text = "Contact", tabName = "Contact")
  )
)
## Content of pages ------
body <- dashboardBody(
  tabItems(
    ### Instruction Tab ------
    tabItem(
      tabName = "Instructions",
      box(includeMarkdown("Instructions.md"), width = 8)
    ),
    ### Upload Tab ------
    tabItem(
      tabName = "Upload",
      fluidRow(
        box(title = HTML('<b>Upload</b>'),
            width = 5,
            headerBorder = F,
            collapsible = F,
            solidHeader = T,
            label = boxLabel(text = "Help", status = "info", tooltip = "Upload a CSV file"),
            fileInput(
              inputId = "newFile",
              label = NULL,
              accept = c(".csv"),
              buttonLabel = "Upload...",
              placeholder = "a .csv file"
            ),
            radioButtons(
              inputId = "chondrite_values",
              label = "Chondrite Values",
              choices = c(
                `Palme and O'Neill (2014)` = "PalmeOneill2014CI",
                `McDonough and Sun (1995)` = "McDonough1995CI"
              )
            )
        ),
        box(title = HTML('<b>Model variables</b>'),
            width = 7,
            headerBorder = F,
            collapsible = F,
            solidHeader = T,
            HTML('Selected elements are used for modelling. '),
            textOutput("Rees"),
            p(""),
            radioButtons(
              inputId = "method",
              label = "Method",
              choices = c(
                `Carrasco-Godoy and Campbell (2023)` = 1,
                `Zhong et al. (2019)` = 2
              )),
            checkboxGroupInput(
              inputId = "NormalizeMethod",
              label = "Chondrite Values",
              inline = T,
              choices = REE_plus_Y_Elements,
              selected = REE_plus_Y_Elements[REE_plus_Y_Elements %notin%
                                               c("La", "Ce", "Eu", "Y")]
            )
        )
        ## here another box
      ),
      fluidRow(
        box(
          title = HTML('<b>R<sup>2</sup> plot</b>'),
          plotOutput("R2_plot"),
          headerBorder = F,
          collapsible = F,
          solidHeader = T,
          sidebar = boxSidebar(
            id = "reference_line",
            sliderInput(
              inputId = "reference_line_R2",
              label = "Reference Line",
              value = 0.9,
              min = 0.001,
              max = 0.999,
              step = 0.01
            ),
            checkboxInput(
              inputId = "hide_reference_line",
              label = "Hide line"
            ),
            checkboxInput(
              inputId = "AdjustedR2",
              label = "Use adjusted R2"
            )
          )
        ),
        box(
          title = HTML('<b>Ratio Boxplot</b>'),
          plotOutput("ratio_plot"),
          headerBorder = F,
          collapsible = F,
          solidHeader = T,
          sidebar = boxSidebar(
            id = "correct_heavy_sliders",
            HTML("R<sup>2</sup> filter. Evaluate correction only for models R<sup>2</sup> > 0.9"),
            sliderInput(
              inputId = "rsquared_filter",
              label = "R squared filter",
              value = 0.9,
              min = 0,
              max = 2,
              step = 0.001
            ),

            #### Correction Factor Sliders -----------
            HTML("This sliders are correction factors to accound for differences between measured and calculated values."),
            sliderInput(inputId = "Pr", label = "Pr correction", value = 1/0.918503242748121, min = 0.1, max = 2),
            sliderInput(inputId = "Nd", label = "Nd correction", value = 1/0.964163780696138 , min = 0.1, max = 2),
            sliderInput(inputId = "Sm", label = "Sm correction", value = 1/1.03474021036956 , min = 0.1, max = 2),
            sliderInput(inputId = "Gd", label = "Gd correction", value = 1/1.08353863509898, min = 0.1, max = 2),
            sliderInput(inputId = "Tb", label = "Tb correction", value =  1/1.12453248979803 , min = 0.1, max = 2),
            sliderInput(inputId = "Dy", label = "Dy correction", value = 1/1.13717202794484 , min = 0.1, max = 2),
            sliderInput(inputId = "Ho", label = "Ho correction", value = 1/1.06914181064329, min = 0.1, max = 2),
            sliderInput(inputId = "Er", label = "Er correction", value = 1/0.982456334111282 , min = 0.1, max = 2),
            sliderInput(inputId = "Tm", label = "Tm correction", value = 1/0.973645612359077 , min = 0.1, max = 2),
            sliderInput(inputId = "Yb", label = "Yb correction", value = 1/0.848963529664848 , min = 0.1, max = 2),
            sliderInput(inputId = "Lu", label = "Lu correction", value = 1/0.774442796976892 , min = 0.1, max = 2),
            sliderInput(inputId = "Y", label = "Y correction", value = 1/0.713075166441028, min = 0.1, max = 2),




          )
        ) ## here another box
      ) ,
      fluidRow(
        box(title = HTML('<b>Summary statistics</b>'),
            width = 12,
            reactableOutput('ratio_table')
        )
      )## here another fluidrow
    ),
    ### Data Tab ------
    tabItem(
      tabName = "Data",
      fluidRow(
        box(
          headerBorder = F,
          collapsible = F,
          solidHeader = T,
          plotOutput("REE_plot"),
          sidebar = boxSidebar(
            id = "Strain_check",
            checkboxInput(inputId = "strain", label = "Use lattice strain expression?", value = F),
            checkboxInput(inputId = "complete_lines", label = "Complete pattern when data is missing?", value = F),
            checkboxInput(inputId = "hide_original", label = "Hide Original", value = F),
            checkboxInput(inputId = "hide_calculated", label = "Hide Calculated", value = F),
            checkboxInput(inputId = "hide_legend", label = "Hide Legend", value = F)
          )
        ),
        box(
          headerBorder = F,
          collapsible = F,
          solidHeader = T,
          reactableOutput("REE")
        )
      ),
      fluidRow(
        box(
          rightBorder = F,
          headerBorder = F,
          collapsible = F,
          solidHeader = T,
          reactableOutput("Result"), width = 12
        )
      )
    ),
    ### Download Tab ------
    tabItem(
      tabName = "Download",
      HTML('<h2>Description of Downloaded Data to be added</h2>'),
      box(title = HTML('<h3>Impute Values?</h3>'),
          HTML('Ticking this option will add a column for each REE in the format <code>Imputed_REEname</code>,
             where missing values are replaced for those calculated from this regression. Measured values are kept.'),
          HTML('The R<sup>2</sup> slider, allows to set a threshold for imputation. Models with a fitting lower than the threshold are not imputed'),
          sliderInput(inputId = 'R_filter_export',label = HTML("R<sup>2</sup> filter"), value = 0.9, min = 0.01, max = 1,step = 0.01  ),
          tooltip(
            title = "Should imputed values be included in the Output?",
            placement = "right",
            tag = checkboxInput(
              inputId = "impute",
              label = "Impute?",
              value = FALSE
            )
          )

      ),
      box(title = HTML('<h3>Include Chondrite nomalized values?</h3>'),
          HTML('Ticking this option will add a column for each REE in the format <code>Normalized_REEname</code> and <code>NormalizedCalc_REEname</code> to include the Chondrite normalized values using measured and calculated concentrations, respectively.'),

        tooltip(
        title = "Should Chondrite normalized values be included in the Output?",
        placement = "right",
        tag = checkboxInput(
          inputId = "include_norm_values",
          label = "Include Chondrite Normalized Values?",
          value = FALSE
        ))

      ),

      box(Title = HTML('<h2>Click below to download</h2>'),
        downloadButton(outputId = "donwload_data")
      )

    ),
    ### References Tab ------
    tabItem(
      tabName = "References",
      box(includeMarkdown('Reference.md'), width = 9)
    ),
    ### Changelog Tab ------
    tabItem(
      tabName = "Changelog",
      box(includeMarkdown('Changelog.md'), width = 9)
    ),
    tabItem(
      ### Contact Tab ------
      tabName = "Contact",
      box(includeMarkdown('Contact.md'), width = 9)
    )
  )
)




# UI function ############
ui <- dashboardPage(header, sidebar, body, title = "ImputeREEapp", help = T)

# server Section -----------
# Define server logic required to draw a histogram
server <- function(input, output) {
  thematic::thematic_shiny() ## makes plots to match html theme



  ## Load Testing data ------
  sample_data <- reactive({
    testing_data %>%
      # slice(1:500) %>%
      select(matches(paste0("Zr_", REE_plus_Y_Elements, "_ppm")),Zr_P_ppm) %>%
      rename_with(~ str_remove_all(.x, "^Zr_|_ppm$"))
  })


  ## Prepare Data ------

  base_data <- reactive({


    # req(input$newFile$datapath)


    if (is.null(input$newFile)) {
      data <- sample_data()
    } else {
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


    normalized_data <- data %>% imputeREE:::Element_norm(chondrite = !!sym(input$chondrite_values))
    ### Model Section Data ------
    withProgress(
      message = "Fitting Models",
      data <- data %>% model_REE(., exclude = REE_to_model, chondrite = !!sym(input$chondrite_values), correct_heavy = F, method = input$method)
    )

    complete_data <- left_join(data, normalized_data, by = "rowid") %>%
      rename(
        `R.squared` = model_r.squared,
        `adj.R.squared` = model_adj.r.squared,
        `Intercept` = estimate_Intercept,
        `Slope` = estimate_Slope,
      )

    return(complete_data)
  })



  ## Apply Corrections  ------
  corrected_data <- reactive({
    data <- base_data() %>% imputeREE:::correct_heavy(
      Y_correction_fact = input$Y,
      Yb_correction_fact = input$Yb,
      Lu_correction_fact = input$Lu,
      Ho_correction_fact = input$Ho,
      Er_correction_fact = input$Er,
      Tm_correction_fact = input$Tm
  ) %>%  imputeREE:::correct_middle(
    Nd_correction_fact = input$Nd,
    Sm_correction_fact = input$Sm,
    Gd_correction_fact = input$Gd,
    Tb_correction_fact = input$Tb,
    Dy_correction_fact = input$Dy,
    Pr_correction_fact = input$Pr

  )

    return(data)
  })

  ##  Create long format with normalized values ----------------

  norm_table <- reactive({
    data <- corrected_data() %>%
      select(rowid, `R.squared` , matches("Normalized")) %>%
      rename_with(.cols = matches("Normalized$"), ~ paste0("Normalized_", str_remove_all(.x, "_Normalized$"))) %>%
      pivot_longer(cols = matches("Normalized"), names_to = c(".value", "Element_name"), names_sep = "_") %>%
      imputeREE:::add_IonicRadii() %>%
      rename(`Ionic Radius` = ShannonRadiiVIII_Coord_3plus) %>%
      mutate(Ratio = NormalizedCalc / Normalized)

    data_long <- data %>%
      select(-Ratio) %>%
      pivot_longer(cols = c("NormalizedCalc", "Normalized"), names_to = "Data Type", values_to = "Chondrite Normalized")
    return(list(data_long, data))
  })


## This is the data used for the table that is shown in Data tab ----------------

  modelled_data <- reactive({
    data <- norm_table()[[2]] %>%
      pivot_wider(id_cols = 'rowid', names_from = 'Element_name', names_prefix = 'Ratio_', values_from = 'Ratio')

    data <- left_join(corrected_data(),data,by ='rowid') %>%
      mutate(`Ce/Ce*` = Ce/ppmCalc_Ce ,
      `Eu/Eu*` = Eu/ppmCalc_Eu ) %>%
      relocate(
        rowid, model_nree,
        R.squared,
        adj.R.squared,
        Intercept,
        Slope,
        `Ce/Ce*`,
        `Eu/Eu*`,
        REE_plus_Y_Elements,
        matches("_Normalized$"), matches("^ppmCalc_"), matches("^NormalizedCalc_"), matches("^Ratio_")
      )
  })


  ## List of excluded REE ----------------
  output$Rees <- renderText({
    paste("Excluded from model:", paste(" ", REE_plus_Y_Elements[REE_plus_Y_Elements %notin% input$NormalizeMethod], collapse = " , "))
  })
  ## tables ----------------

  ###  reactable for the results ##############

  output$Result <- renderReactable({
    data <- modelled_data()

    if (input$include_norm_values == FALSE) {
      data <- data %>% select(!matches("^NormalizedCalc|Normalized"))
    }
    # if (input$impute) {
    #   data <- data %>% impute_REE()
    # }

    reactable(data %>%  select(-dplyr::last_col(15):-last_col()),
              defaultPageSize = 25,
              resizable = T,
              showPageSizeOptions = T,
              defaultColDef = colDef(format = reactable::colFormat(digits = 3)),
              columns = list(
                rowid = colDef(name = "ID", format = colFormat(digits = 0))

              ),
              highlight = T,
              onClick = "select",
              selection = "single",
              filterable = T,
              defaultSelected = 1,
              theme = reactablefmtr::flatly()
    )
  })

  ###  Summary table in upload ##############
  output$REE <- renderReactable({
    req(getReactableState("Result")$selected)
    id_num <- getReactableState("Result")$selected

    # req(getReactableState("Result")$selected)


    data <- norm_table()[[2]] %>% filter(rowid == id_num)
    reactable(data,
              resizable = T,
              # showPageSizeOptions = T,
              # defaultPageSize = 8,
              defaultColDef = colDef(format = reactable::colFormat(digits = 3)),
              highlight = T,
              theme = reactablefmtr::flatly(),
              columns = list(
                rowid = colDef(name = "ID", format = colFormat(digits = 0))
              ),
    )
  })


  output$ratio_table <- renderReactable({
    data <- norm_table()[[2]] %>%  arrange(desc(`Ionic Radius`))
    data <- data %>%
      select(Element_name, Ratio) %>%
      group_by(Element_name) %>%
      summarize(mean = mean(Ratio,na.rm = T),
                sd = sd(Ratio,na.rm = T),
                median = median(Ratio,na.rm = T),
                IQR = IQR(Ratio,na.rm = T),
                MAD = mad(Ratio,na.rm = T)) %>%
      imputeREE:::add_IonicRadii() %>%
      arrange(desc(ShannonRadiiVIII_Coord_3plus)) %>%
      select(-ShannonRadiiVIII_Coord_3plus)

    reactable(data ,
              pagination = F,
              highlight = T,

              defaultColDef = colDef(format = reactable::colFormat(digits = 3)),
              theme = flatly())
  })

  # Plots ######


  output$REE_plot <- renderPlot({

    breaks <- imputeREE::Element_Data %>% filter(Element_name %in% REE_plus_Y_Elements)
    labels <- breaks$Element_name
    breaks <- breaks$ShannonRadiiVIII_Coord_3plus
    req(getReactableState("Result")$selected)

    id_num <- getReactableState("Result")$selected
    data <- norm_table()[[1]]



    if (input$hide_original) {
      data <- data %>% filter(`Data Type` != "Normalized")
    }
    if (input$hide_calculated) {
      data <- data %>% filter(`Data Type` != "NormalizedCalc")
    }

    if (input$complete_lines) {
      data <- data %>% filter(!is.na(`Chondrite Normalized`))
    }


    if (input$strain) {
      data <- data %>% mutate(`Ionic Radius` = (`Ionic Radius` / 3 + 0.84 / 6) * (`Ionic Radius` - 0.84)^2)
      breaks <- (breaks / 3 + 0.84 / 6) * (breaks - 0.84)^2
    }

    data %>%
      filter(rowid == id_num) %>%
      ggplot(aes(x = `Ionic Radius`, y = `Chondrite Normalized`, group = `Data Type`, color = `Data Type`)) +
      {
        if (input$strain) xlab("ri/3 + r0/6)*(ri-r0)^2")
      } +
      # theme_bw(base_size = 15) +
      theme(
        legend.position = c(0.95, 0.05),
        legend.justification = c(0.95, 0.05)
      ) +
      {
        if (input$hide_legend) theme(legend.position = "none")
      } +
      geom_point(size = 3) +
      geom_line() +
      scale_y_log10(
        # labels = scales::label_number(accuracy = 0.00001)
      )+
      scale_x_reverse(breaks = breaks, labels = labels, minor_breaks = NULL)
  })

  ###### Rsquared Histogram
  output$R2_plot <- renderPlot({
    var <- "R.squared"
    if (input$AdjustedR2) {
      var <- "adj.R.squared"
    }


    modelled_data() %>%
      ggplot(aes_string(var)) +
      geom_histogram() +
      {
        if (!input$hide_reference_line) geom_vline(xintercept = input$reference_line_R2)
      } +
      scale_x_continuous(trans = "logit")
  })


  #### Ratio_plots
  output$ratio_plot <- renderPlot({
    breaks <- imputeREE::Element_Data %>% filter(Element_name %in% REE_plus_Y_Elements)
    labels <- breaks$Element_name
    breaks <- breaks$ShannonRadiiVIII_Coord_3plus




    norm_table()[[2]] %>%  filter(R.squared > input$rsquared_filter) %>%
      arrange(desc(`Ionic Radius`)) %>%
      ggplot(aes(y = Ratio, x = forcats::fct_inorder(Element_name))) +
      geom_boxplot(
        # aes(fill = forcats::fct_inorder(Element_name))
      ) +
      geom_hline(yintercept = 1, lty = 2, alpha = 0.8)+
      # scale_fill_viridis_d() +
      theme(legend.position = 'none')+
      scale_y_log10(
        # labels = scales::label_number(accuracy = 0.00001)
      )
  })


  download_data <- reactive(
    {
      data <- modelled_data()


      if (input$include_norm_values == FALSE) {
        data <- data %>% select(!matches("^NormalizedCalc|Normalized"))
      }
      if (input$impute) {
        data <- data %>% impute_REE(rsquared = input$R_filter_export)
      }
      return(data)
    }
  )

  #### downloads ####

  output$donwload_data <- downloadHandler(


    filename = function() {
      paste0(input$dataset, "calc.csv")
    },
    content = function(file) {
      write.csv(download_data(), file)
    }
  )



}

# Run the application
shinyApp(ui = ui, server = server)
