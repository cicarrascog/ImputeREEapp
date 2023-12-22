options(scipen = 100000)
library(shiny)
library(thematic)
library(plotly)
library(tidyverse)
library(imputeREE)
library(reactable)
library(reactablefmtr)
library(markdown)
library(bslib)




`%notin%` <- Negate(`%in%`)

card_Welcome1<- card(
  card_header(HTML("<h2>Instructions</h2>")),
  full_screen = F,
  includeMarkdown("Instructions_card1.md"))
card_Welcome2<- card(
  card_header(html("<h2>Quick Start (tl;dr)</h2>")),
  full_screen = F,
  includeMarkdown("Instructions_card2.md"))

card_References <- card(
  # card_header("Welcome"),
  full_screen = TRUE,
  includeMarkdown("Reference.md"))
card_changelog <- card(
  # card_header("Welcome"),
  full_screen = TRUE,
  includeMarkdown("Changelog.md"))
card_license <- card(
  # card_header("Welcome"),
  full_screen = TRUE,
  includeMarkdown("license.md"))
card_contact <- card(
  # card_header("Welcome"),
  full_screen = TRUE,
  includeMarkdown("Contact.md"))


### Upload Tab

card_fileupload <- card( #--------------
  # card_header("File upload") ,
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
)

card_model_Settings <- card(# model settings --------------


    textOutput("Rees"),
    checkboxGroupInput(
      inputId = "NormalizeMethod",
      label = "Chondrite Values",
      inline = T,
      choices = REE_plus_Y_Elements,
      selected = REE_plus_Y_Elements[REE_plus_Y_Elements %notin%
                                       c("La", "Ce", "Pr", "Eu", "Y")]
    )

)

card_model_Settings2 <- card(# model settings --------------
radioButtons(
  inputId = "method",
  label = "Method",
  choices = c(
    `Chondrite-Onuma` = 3,
    `Chondrite-Lattice` = 1,
    `Zhong et al. (2019)` = 2
  )))


card_r2_histogram <- card(##r2 histogram--------------
  card_header(HTML("R<sup>2</sup> histogram")),
    layout_sidebar(
    sidebar = sidebar(
      title = 'Plot Settings',
      position = "right",
      sliderInput(
        inputId = "reference_line_R2",
        label = "Reference Line",
        value = 0.95,
        min = 0.5,
        max = 0.999,
        step = 0.005
      ),
      checkboxInput(
        inputId = "hide_reference_line",
        label = "Hide line"
      ),
      checkboxInput(
        inputId = "AdjustedR2",
        label = "Use adjusted R2"
      )
    ),
    plotOutput("R2_plot")
   ))


card_boxplot <- card(## card_boxplot--------------
  card_header("Ratio Boxplot"),
    layout_sidebar(

    sidebar = sidebar(
      title = 'Plot Settings',
      position = "right",
      open = 'desktop',
      sliderInput(
      inputId = "rsquared_filter",
      label = "R squared filter",
      value = 0.9,
      min = 0,
      max = 2,
      step = 0.001
    )),
    plotOutput("ratio_plot")
  )
  )

card_summstats <- card( ## card_summstats-------------
  card_header(HTML("Summary statistics")),max_height = '30%',
  reactableOutput("ratio_table")
)



# Data Tab--------------
card_Reactable <- card(##card_Reactable-----------------
                       full_screen = T,fill = T,
  reactableOutput("Result")
)

card_REE_plot <- card(##card_REE_plot-------------
  max_height = '350',
                      card_header('Chondrite-Normalized pattern'),
  layout_sidebar(
    sidebar = sidebar(
      title = 'Plot Settings',
      position = "right",
      checkboxInput(inputId = "strain", label = "Use misfit expression?", value = F),
      checkboxInput(inputId = "complete_lines", label = "Complete pattern when data is missing?", value = F),
      checkboxInput(inputId = "hide_original", label = "Hide Original", value = F),
      checkboxInput(inputId = "hide_calculated", label = "Hide Calculated", value = F),
      checkboxInput(inputId = "hide_legend", label = "Hide Legend", value = F),
      shiny::numericInput(inputId = "min_plot_value", value = NULL, label = "min y axis"),
      shiny::numericInput(inputId = "max_plot_value", value = NULL, label = "max y axis")),
    plotOutput("REE_plot")
  )
)


card_scatterplot_plotly <-card(##card_scatterplot_plotly-------------
  max_height = '400',
                               card_header('Scatterplot'),
                                layout_sidebar(
                                  sidebar = sidebar(
                                    title = 'Plot Settings',
                                    position = "right",
                                    uiOutput("scatterycol"),
                                    uiOutput("scatterxcol"),
                                    checkboxInput(inputId = "scatter_log_y", label = "log y", value = F),
                                    checkboxInput(inputId = "scatter_log_x", label = "log x", value = T),
                                      #
                                    ),
                                  plotly::plotlyOutput("scatterplot"),

                                  ))


# Downliad Tab =========================


card_Download_settings <- card( ## card_Download_settings ======

  card_header(HTML("<h4>Impute Values?</h4>")),
  markdown("Ticking this option will add a column for each REE in the format `Imputed_[REEname]`,
             where missing values are replaced for those calculated from the model. Measured values are kept."),
  markdown("The slider allows to set a threshold for imputation so models with R<sup>2</sup> lower than the threshold are not imputed"),
  HTML("Check important info in the welcome tab for more details."),
  sliderInput(inputId = "R_filter_export", label = HTML("R<sup>2</sup> filter"), value = 0.99, min = 0.9, max = 1, step = 0.001),
  checkboxInput(
    inputId = "impute",
    label = "Impute?",
    value = FALSE
  )
)


card_include_norm <- card(
  card_header(HTML("<h4>Include Chondrite nomalized values?</h4>")),

  markdown("Ticking this option will add a column for each REE in the format `[REEname]_Normalized` and `NormalizedCalc_[REEname]` to include the Chondrite normalized values using measured and calculated concentrations, respectively."),
  checkboxInput(
    inputId = "include_norm_values",
    label = "Include Chondrite Normalized Values?",
    value = FALSE)
)


  Download_card <- card(
    markdown('**Download Info**'),
    markdown('Ce/Ce* and Eu/Eu* are calculaded using Ce* from the regression models (ppmCalc_Ce and ppmCalc_Eu, respectively).'),
    markdown('If the Chondrite-Lattice method is used, Intercept and slope are returned.'),
    markdown('If the Chondrite-Onuma method is used, parabola parameters (a,b, and intercept) are returned.'),
    markdown('Columns starting with `ppmCalc_` are calculated values in ppm.'),
    markdown('Columns starting with `NormalizedCalc_` are calculated chondrite-normalized values'),
    markdown('Columns starting with `Ratio_` are ratio between calculated and measured values (not applicable when data are missing).'),
    markdown("**Click below to download**"),
    downloadButton(outputId = "donwload_data")
  )

ui <- page_navbar( # UI ----------

  theme = bs_theme(),
  title = "imputeREEapp",
  gap = validateCssUnit(5) ,
  padding = validateCssUnit(5) ,
  fillable = T,
  # sidebar = color_by,
  nav_panel(title = 'Welcome',
            includeMarkdown("Instructions.md"),
            layout_columns(card_Welcome2, card_Welcome1)),
  nav_panel(title = "Upload",

            layout_columns(card_fileupload, card_model_Settings,card_model_Settings2, gap = validateCssUnit(5), height = '30%'),
            layout_columns(card_r2_histogram, card_boxplot, gap = validateCssUnit(5)),
            card_summstats),
  nav_panel(title = "Data",
            bslib::layout_sidebar(
              sidebar = sidebar(

                card_REE_plot,
                card_scatterplot_plotly,
                width= validateCssUnit("45%")),
              reactableOutput("Result", height = '100%'))),

  nav_panel(title = "Download", page_sidebar(
    card_Download_settings,
    card_include_norm,
    sidebar = sidebar(Download_card, open ='always', width = "50%"))),

  nav_panel(title = "References", card_References),

  nav_panel(title = "Changelog", card_changelog),

  nav_panel(title = "License", card_license),

  nav_panel(title = "Contact", card_contact),
  nav_spacer(),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ))




# server Section -----------
# Define server logic required to draw a histogram
server <- function(input, output) {
thematic::thematic_shiny(font = font_spec(scale = 1.5))




    sample_data <- reactive({
    Ballard_et_al_Zircon %>%
      # slice(1:500) %>%
      select(matches(paste0("Zr_", REE_plus_Y_Elements, "_ppm")), Zr_P_ppm) %>%
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
    if (input$method == 1) {
      withProgress(
        message = "Fitting Models",
        data <- data %>%
          modelChondrite_lattice(.,
                                 exclude = REE_to_model,
                                 chondrite = !!sym(input$chondrite_values),
                                 Calibrate = T
          )
      )
    }

    ### Here conditional for the three methods
    withProgress(
      message = "Fitting Models",
      if (input$method == 2) {
        data <- data %>%
          modelZhong(.,
                     exclude = REE_to_model,
                     chondrite = !!sym(input$chondrite_values)
          )
      }
    )

    withProgress(
      message = "Fitting Models",
      if (input$method == 3) {
        data <- data %>%
          modelChondrite_Onuma(.,
                               exclude = REE_to_model,
                               chondrite = !!sym(input$chondrite_values),
                               Calibrate = T
          )
      }
    )


    ### HERE A CONDITIONAL FOR CHONDRITE ONUMA METHOD

    if (input$method == 3) {
      complete_data <- left_join(data, normalized_data, by = "rowid") %>%
        rename(
          `R.squared` = model_r.squared,
          `adj.R.squared` = model_adj.r.squared,
          `Intercept` = estimate_Intercept,
          `a (parabola)` = estimate_X1,
          `b (parabola)` = estimate_X2
        )
    } else {
      complete_data <- left_join(data, normalized_data, by = "rowid") %>%
        rename(
          `R.squared` = model_r.squared,
          `adj.R.squared` = model_adj.r.squared,
          `Intercept` = estimate_Intercept,
          `Slope` = estimate_Slope
        )
    }




    return(complete_data)
  })



  ## Apply Corrections  ------
  # corrected_data <- reactive({
  #   data <- base_data() %>%
  #     imputeREE:::correct_heavy(
  #       Y_correction_fact = input$Y,
  #       Yb_correction_fact = input$Yb,
  #       Lu_correction_fact = input$Lu,
  #       Ho_correction_fact = input$Ho,
  #       Er_correction_fact = input$Er,
  #       Tm_correction_fact = input$Tm
  #     ) %>%
  #     imputeREE:::correct_middle(
  #       Nd_correction_fact = input$Nd,
  #       Sm_correction_fact = input$Sm,
  #       Gd_correction_fact = input$Gd,
  #       Tb_correction_fact = input$Tb,
  #       Dy_correction_fact = input$Dy,
  #       Pr_correction_fact = input$Pr
  #     )

  #   return(data)
  # })

  ##  Create long format with normalized values ----------------

  norm_table <- reactive({
    data <- base_data() %>%
      select(rowid, `R.squared`, matches("Normalized")) %>%
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
      pivot_wider(id_cols = "rowid", names_from = "Element_name", names_prefix = "Ratio_", values_from = "Ratio")

    ### HERE ANOTHER CONDITIONAL FOR METHOD

    data <- left_join(base_data(), data, by = "rowid") %>%
      mutate(
        `Ce/Ce*` = Ce / ppmCalc_Ce,
        `Eu/Eu*` = Eu / ppmCalc_Eu
      ) %>%
      relocate(
        rowid, model_nree,
        R.squared,
        adj.R.squared,
        matches("Intercept|Slope|(parabola)"),
        # Slope,
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

if (input$dark_mode == 'dark') {
  reactable(data %>% select(!matches("model_.+$|std.error_.+$|statistic_.+$|p.value_.+$")),
            # %>%  select(-dplyr::last_col(15):-last_col()),
            defaultPageSize = 15,
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
            theme = reactablefmtr::slate())
} else{
    reactable(data %>% select(!matches("model_.+$|std.error_.+$|statistic_.+$|p.value_.+$")),
              # %>%  select(-dplyr::last_col(15):-last_col()),
              defaultPageSize = 15,
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
              defaultSelected = 1)}


  })




  output$ratio_table <- renderReactable({
    data <- norm_table()[[2]] %>% arrange(desc(`Ionic Radius`))
    data <- data %>%
      select(Element_name, Ratio) %>%
      group_by(Element_name) %>%
      summarize(
        mean = mean(Ratio, na.rm = T),
        sd = sd(Ratio, na.rm = T),
        median = median(Ratio, na.rm = T),
        IQR = IQR(Ratio, na.rm = T),
        MAD = mad(Ratio, na.rm = T)
      ) %>%
      imputeREE:::add_IonicRadii() %>%
      arrange(desc(ShannonRadiiVIII_Coord_3plus)) %>%
      select(-ShannonRadiiVIII_Coord_3plus)
if (input$dark_mode == 'dark') {
  reactable(data,

            pagination = F,
            highlight = T,
            defaultColDef = colDef(format = reactable::colFormat(digits = 3)),
            theme = reactablefmtr::dark()
  )
} else {
  reactable(data,

            pagination = F,
            highlight = T,
            defaultColDef = colDef(format = reactable::colFormat(digits = 3))
  )

}

  })

  # Plots ######

  ### REE plot ##########

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
      scale_y_log10(minor_breaks = NULL

        # labels = scales::label_number(accuracy = 0.00001)
      ) +
      scale_x_reverse(breaks = breaks, labels = labels, minor_breaks = NULL) +
      scale_color_viridis_d(labels = c('Measured','Calculated')) +
      annotation_logticks(sides = 'l') +
      coord_cartesian(ylim = c(input$min_plot_value, input$max_plot_value))
  })



  ### scatterpot ##########

  output$scatterxcol <- renderUI({
    data <- modelled_data()

    if (input$include_norm_values == FALSE) {
      data <- data %>% select(!matches("^NormalizedCalc|Normalized"))
    }
    data <- data %>%
      select(!matches("model_.+$|std.error_.+$|statistic_.+$|p.value_.+$")) %>%
      select(where(is.numeric))


    choices <- names(data)

    selectInput("xcol", "X Variable", choices = choices, selected = "La", multiple = F, selectize = T)
  })

  output$scatterycol <- renderUI({
    data <- modelled_data()

    if (input$include_norm_values == FALSE) {
      data <- data %>% select(!matches("^NormalizedCalc|Normalized"))
    }
    data <- data %>%
      select(!matches("model_.+$|std.error_.+$|statistic_.+$|p.value_.+$")) %>%
      select(where(is.numeric))


    choices <- names(data)

    selectInput("ycol", "Y Variable", choices = choices, selected = "Pr", multiple = F, selectize = T)
  })




  output$scatterplot <- plotly::renderPlotly({
    req(modelled_data())
    data <- modelled_data()

    if (input$include_norm_values == FALSE) {
      data <- data %>% select(!matches("^NormalizedCalc|Normalized"))
    }

    data <- data %>% select(!matches("model_.+$|std.error_.+$|statistic_.+$|p.value_.+$"))


    fig <- plotly::plot_ly(data, x = ~.data[[input$xcol]], y = ~.data[[input$ycol]], text = ~paste('ID:',.data[['rowid']]))


    fig <- fig %>%  plotly::layout(xaxis = list(title = as.character(input$xcol)),
                            yaxis = list(title = as.character(input$ycol)))


    #

    if (input$scatter_log_x) {
      fig <- fig %>%  plotly::layout( xaxis = list(type = "log"))
    }
    if (input$scatter_log_y) {
      fig <- fig %>%  plotly::layout( yaxis = list(type = "log"))
    }
    ## this is the dark mode for plotly ----------------
    if(input$dark_mode=='dark'){
    fig <- fig %>% plotly::layout(plot_bgcolor = '#444', paper_bgcolor = '#303030', font = list(color = 'white'), xaxis = list(gridcolor = '#303030'),yaxis = list(gridcolor = '#303030'))} else {fig}
  })


  ### Rsquared Histogram #########
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
      scale_x_continuous(trans = "logit", breaks = c(0.5, 0.9, 0.95, 0.99, 0.999))
  })


  ### Ratio_plots ###########
  output$ratio_plot <- renderPlot({
    breaks <- imputeREE::Element_Data %>% filter(Element_name %in% REE_plus_Y_Elements)
    labels <- breaks$Element_name
    breaks <- breaks$ShannonRadiiVIII_Coord_3plus




    norm_table()[[2]] %>%
      filter(R.squared > input$rsquared_filter) %>%
      # filter(Element_name %in% REE_Elements[-c(1,2,3,6)]) %>%
      arrange(desc(`Ionic Radius`)) %>%
      ggplot(aes(y = Ratio, x = forcats::fct_inorder(Element_name))) +
      geom_boxplot(
        # aes(fill = forcats::fct_inorder(Element_name))
      ) +
      xlab("Element") +
      geom_hline(yintercept = 1, lty = 2, alpha = 0.8) +
      # scale_fill_viridis_d() +
      theme(legend.position = "none") +
      scale_y_log10(
        # labels = scales::label_number(accuracy = 0.00001)
      )
  })


  download_data <- reactive({
    data <- modelled_data()
    data <-data %>%  mutate(model_r.squared = R.squared)

    if (input$include_norm_values == FALSE) {
      data <- data %>% select(!matches("^NormalizedCalc|Normalized"))
    }
    if (input$impute) {
      data <- data %>% impute_REE(rsquared = input$R_filter_export)
      data <- data %>%  select(-model_r.squared)
       }
    data <- data %>% select(!matches("model_.+$|std.error_.+$|statistic_.+$|p.value_.+$"))
    return(data)
  })

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
