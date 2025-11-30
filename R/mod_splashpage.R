#' splashpage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


library(shinyWidgets)
mod_splashpage_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
fluidRow(column(6, bs4Dash::box( title = "Select species", width = 12,status = "primary", solidHeader = TRUE,
  fluidRow(column(5,shiny::selectInput(inputId = ns("first_box_input"), selected = "BeF2", choices = vec, label = "species 1")),
           column(5,shiny::selectInput(inputId = ns("second_box_input"), selected = "LiF", choices = vec, label = "species 2")),
           column(2,style = "margin-top: 30px;", shinyWidgets::actionBttn(inputId = ns("plot_button"), label = "Plot", style = "material-flat", color="default", block = FALSE, class = "green-button"))),
          shiny::radioButtons(inputId = ns("temp_button"), choices = c("Kelvin", "Celsius"), inline = TRUE, selected = "Kelvin", label = "Temperature units"),
DT::DTOutput(outputId =ns("summary_table")))),
         column(6,bs4Dash::box(width=NULL, title = "Phase diagram plot",status = "primary", solidHeader = TRUE, highcharter::highchartOutput(outputId = ns("phase_equilibria_plot")) |> shinycssloaders::withSpinner(type=3,color.background = "#2e8b57",color = "#2e8b57")))),

bs4Dash::box(title = "Phase equilibria data", status = "primary", solidHeader = TRUE, DT::DTOutput(outputId = ns("phase_equilibria_table")) |> shinycssloaders::withSpinner(type=3,color.background = "#2e8b57",color = "#2e8b57"), width=NULL),

tags$style(HTML(".dt-button {
                background-color: #2e8b57;}" ))
    ))}

vec <- data.table::fread("data/list_choices.csv")$V1


#' splashpage Server Functions
#'
#' @noRd
mod_splashpage_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    source_data = reactiveVal(dplyr::tibble())
    fetch_data = function() {

      dat <- data.table::fread("./data/phase_equilibria/pe1.csv")
      if (input$second_box_input == "" & input$first_box_input == "") {

      } else if (!input$second_box_input == "" & !input$first_box_input == "") {
        dat = dat |>
          dplyr::filter(`Species 1` %in% c(input$first_box_input, input$second_box_input),
                        `Species 2` %in% c(input$first_box_input, input$second_box_input))
      }else if (input$first_box_input == "") {
        dat = dat |>
          dplyr::filter(`Species 1` %in% c(input$second_box_input) |
                          `Species 2` %in% c(input$second_box_input))
      }else if (input$second_box_input == "") {
        dat = dat |>
          dplyr::filter(`Species 1` %in% c(input$first_box_input) |
                          `Species 2` %in% c(input$first_box_input))
      }
      source_data(dat)
    }

# get the models
    source_model = reactiveVal(dplyr::tibble())
    fetch_model = function() {
      dat_model <- data.table::fread("./data/mstdb_models/mstdb-models.csv")

      if (input$second_box_input == "" &
          input$first_box_input == "") {

      } else if (!input$second_box_input == "" &
                 !input$first_box_input == "") {
        dat_model = dat_model |>
          dplyr::filter(
            `Species 1` %in% c(input$first_box_input, input$second_box_input),
            `Species 2` %in% c(input$first_box_input, input$second_box_input)
          )
      } else if (input$first_box_input == "") {
        dat_model = dat_model |>
          dplyr::filter(
            `Species 1` %in% c(input$second_box_input) |
              `Species 2` %in% c(input$second_box_input)
          )
      } else if (input$second_box_input == "") {
        dat_model = dat_model |>
          dplyr::filter(
            `Species 1` %in% c(input$first_box_input) |
              `Species 2` %in% c(input$first_box_input)
          )
      }
      source_model(dat_model)
    }

 # render the entire highchart with models + data
output$phase_equilibria_plot <-  highcharter::renderHighchart({

  if (input$temp_button == "Kelvin"){plot_unit <- "K"} else {plot_unit <- "°C"}


  isolate({
    first <- input$first_box_input
    second <- input$second_box_input
    choices_1 <- dplyr::tibble(x = c(first, second)) |> dplyr::arrange(x) |> dplyr::pull(x)
  })

    if (input$temp_button == "Celsius") {
    source_data = source_data() |> dplyr::mutate(`Temp (K)` = `Temp (K)`-273)
    source_model = source_model() |> dplyr::mutate(Y = Y-273)
  } else {
      source_data = source_data()
      source_model= source_model()
    }
      highcharter::highchart() |>
      highcharter::hc_chart(zoomType = "xy", backgroundColor = "white"
                            ) |>
      highcharter::hc_add_series("spline", data = source_model,
      highcharter::hcaes(X, Y, group = group),
                          color = "black",
                          showInLegend = FALSE
      ) |>
      highcharter::hc_add_series(
        data = source_data,
        type = "scatter",
        highcharter::hcaes(x = `Mole frac species 2`, y = `Temp (K)`, group = Author)
      ) |>
        highcharter::hc_tooltip(style = list(fontSize = "10px",
                                             padding = "5px")) |>
        highcharter::hc_tooltip(useHTML = TRUE,
                                formatter = htmlwidgets::JS(
                                  glue::glue(
                                    "function() {{
           return '<b>Mole fraction {choices_1[2]}:</b> ' + this.x.toFixed(2) + '<br>' +
                  '<b>Temperature ({plot_unit}):</b> ' + this.y.toFixed(1) + '<br>' +
           '<b>Author:</b> ' + this.point.Author;
         }}"
                                  )
                                )) |>
        highcharter::hc_xAxis(title = list(text = paste("Mole fraction", choices_1[2]))) |>
        highcharter::hc_yAxis(title = list(text = paste0("Temperature (", plot_unit, ")"))) |>
        highcharter::hc_title(text = paste0(choices_1[1], "&nbsp; – &nbsp;", choices_1[2])) |>
        highcharter::hc_exporting(enabled = TRUE,
                                  chartOptions = list(
                                    chart = list(backgroundColor = "#FFFFFF")),
                                                 buttons = list(contextButton = list(
                                                   menuItems = c(
                                                     "viewFullscreen",
                                                     "separator",
                                                     "downloadPNG",
                                                     "downloadSVG"
                                                   )
                                                 ))
                                    )
                                    })

# input_button reactivity
  observeEvent(input$plot_button, {
    fetch_data()
    fetch_model()
  })


# data_table
    output$phase_equilibria_table <- DT::renderDataTable({
      source_data() |>
        dplyr::select(!type) |>
        DT::datatable(style = "bootstrap5",
          extensions = 'Buttons',
          options = list(
            paging = TRUE,
            pageLength = 100,
            scrollX = TRUE,
            scrollY = "400px",
            scrollCollapse = TRUE,
            buttons = c('copy', 'csv'),
            dom = 'Bfrtip',
            fixedHeader = list(top = TRUE),
            responsive = TRUE,
            columnDefs = list(list(
              className = 'dt-center', targets = "_all"
            ))
          ),
          selection = "multiple",
          rownames = FALSE
        )
    }, server = FALSE)

### second Author List table
  output$summary_table <- DT::renderDataTable({
   source_data() |>
      dplyr::select(Author, Link) |>
      dplyr::distinct(Author, Link) |>
      dplyr::mutate(Link = dplyr::case_when(stringr::str_detect(Link |> stringr::str_to_lower(), "doi.org") ~ paste0("<a href='https://", Link, "'>", Link, "</a>"),
                                            stringr::str_detect(Link |> stringr::str_to_lower(), "osti.gov") ~ paste0("<a href='https://", Link, "'>", Link, "</a>"),
                                            TRUE ~ Link)) |>
      DT::datatable(style = "bootstrap5",
                    extensions = 'Buttons',
                    options = list(
                      paging = TRUE,
                      pageLength = 100,
                      scrollX = TRUE,
                      scrollY = "400px",
                      scrollCollapse = TRUE,
                      dom = 't',
                      fixedHeader = list(top = TRUE),
                      responsive = TRUE,
                      columnDefs = list(list(
                        className = 'dt-center', targets = "_all"
                      ))
                    ),
                    selection = "multiple",
                    rownames = FALSE,
                    escape = FALSE
      )
  }, server = FALSE)


  }
)}

## To be copied in the UI
# mod_splashpage_ui("splashpage_1")

## To be copied in the server
# mod_splashpage_server("splashpage_1")
