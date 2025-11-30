#' ternary_phase_equilibria UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_ternary_phase_equilibria_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(column(4, bs4Dash::box(title = "Upload ternary phase equilibria", width = 12,status = "primary", solidHeader = TRUE,
                            shiny::fileInput(inputId = ns("fig_input"), label = "Upload your own FactSage figure (.fig)", accept = ".fig", placeholder = "KF-LiF-NaF.fig"),
                            shiny::fileInput(inputId = ns("lit_data_input"), label = "Upload your own literature data (.csv)", accept = ".csv", placeholder = "lit_data-KF-LiF-NaF.csv"),
                            shinyWidgets::actionBttn(inputId = ns("plot_button"), label = "plot", style = "material-flat", color=NULL, block = FALSE, class = "green-button")
      )), column(8, bs4Dash::box(width = 12, title = "Instructions", status = "primary", solidHeader = TRUE, HTML("

    <h3>Use this page to visualise ternary <code>.fig</code> files in 3D.</h3><br>

    <h4>Guidelines:</h4><br>

    <h5>Data File (<code>.fig</code>):</h5>
    <ul>
        <li>Ensure the species order matches that of the literature data file. To maintain consistency, ordering species alphabetically is recommended.</li>
    </ul>

    <h5>Literature Data File (<code>.csv</code>):</h5>
    <ul>
        <li>The <code>.csv</code> file must include the following headers: <b>A</b>, <b>B</b>, <b>C</b>, <b>X</b>, <b>Y</b>, <b>Z</b>, <b>temp_k</b>, and <b>Author</b>.</li>
        <li><b>A, B, C</b>: Species names (e.g., LiF, NaF, KF).</li>
        <li><b>X, Y, Z</b>: Mole fractions of <b>A, B,</b> and <b>C</b>, respectively.</li>
        <li><b>temp_k</b>: Temperature in Kelvin.</li>
        <li><b>Author</b>: Dataset author.</li>
    </ul>

    <p>If no <code>.fig</code> or literature data file is uploaded, the plot will use a default dataset for FLiNaK.</p>


                ")))),

      fluidRow(bs4Dash::box(width=12,height = NULL,title ="plot", status = "primary", solidHeader = TRUE, plotly::plotlyOutput(width = NULL,height = NULL,outputId = ns("ternary_plot")) |>
                              shinycssloaders::withSpinner(type=3,color.background = "#2e8b57",color = "#2e8b57")))),

    tags$head(
      tags$style(HTML("
    .green-button {
      background-color: #2e8b57 !important;
      color: white !important;
    }
    .green-button:hover {
      background-color: #276747 !important;
    }
  "))
    )

  )}



# plotly::plotlyOutput(outputId = ns("tern_plot"))
#' ternary_phase_equilibria Server Functions
#'
#' @noRd
mod_ternary_phase_equilibria_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$plot_button, {
      shinyWidgets::show_toast(title = "Plotting...", text = "Please be patient",timer = 6000,width = 800,type = "info",timerProgressBar = FALSE,position = "bottom")

      tryCatch({
        fetch_data()
        fetch_model()
      }, error = \(e) {
        shinyWidgets::show_toast(
          title = "Error",
          text = e$message,
          timer = 5000,
          timerProgressBar = T,
          position = "center",
          type = "error"
        )
      })
    })

source_data = reactiveVal(dplyr::tibble())
source_model = reactiveVal(dplyr::tibble())

fetch_model = function() {
  if (is.null(input$fig_input)) {
    #fig_data <- read_fig_to_dataframe("data/tern_PE/KCl-NaCl-ZrCl4_1.fig")
    fig_data <- read_fig_to_dataframe("data/tern_PE/FLiNaK.fig")

    } else {
    fig_data <- read_fig_to_dataframe(input$fig_input$datapath)
  }
  source_model(fig_data)
}


fetch_data = function() {
  if (is.null(input$lit_data_input)) {
    lit_data <-  data.table::fread("data/tern_PE/FLiNaK_lit_dat.csv")
  } else {
   lit_data <- data.table::fread(input$lit_data_input$datapath)
  }
  source_data(lit_data)
}

output$ternary_plot <-  plotly::renderPlotly({
 tryCatch({fig_data <- source_model()
  df_lit_data_raw <- source_data()

  cartesian_coords <- lapply(1:nrow(fig_data), function(i) {
    Ternary::CoordinatesToXY(c(fig_data$X[i], fig_data$Y[i], fig_data$Z[i]))
  })

  cartesian_df <- data.frame(
    X = fig_data$X,
    Y = fig_data$Y,
    Z = fig_data$Z,
    temp_k = fig_data$temperature,
    x = sapply(cartesian_coords, `[[`, 1),
    y = sapply(cartesian_coords, `[[`, 2)
  )

  d <- cartesian_df |>  dplyr::filter(!temp_k =="NA")


    df_lit_data <- df_lit_data_raw |>
      dplyr::rename(Y = Z,
             Z = Y)
    lit_data_cartesian_coords_list <- lapply(1:nrow(df_lit_data), function(i) {
      Ternary::CoordinatesToXY(c(df_lit_data$X[i], df_lit_data$Y[i], df_lit_data$Z[i]))
    })

    cartesian_df_lit_data <- data.frame(
      X = df_lit_data$X,
      Y = df_lit_data$Y,
      Z = df_lit_data$Z,
      temp_k = df_lit_data$temp_k,
      x = sapply(lit_data_cartesian_coords_list, `[[`, 1),
      y = sapply(lit_data_cartesian_coords_list, `[[`, 2)
    )

    labelX <- d |> dplyr::arrange(X) |> tail(n=1)
    labelY <- d |> dplyr::arrange(Y) |> tail(n=1)
    labelZ <- d |> dplyr::arrange(Z) |> tail(n=1)


    annotations <- data.frame(
      x=c(labelX$x, labelY$x, labelZ$x),
      y=c(labelX$y,labelY$y, labelZ$y),
      z=c(labelX$temp_k+50,labelY$temp_k+50, labelZ$temp_k+50),
      text = c(df_lit_data$A[1], df_lit_data$C[1], df_lit_data$B[1]))

    d <- d |> dplyr::mutate(dplyr::across(c(X, Y, Z, temp_k), ~round(., 2)))
    df_lit_data <- df_lit_data |> dplyr::mutate(dplyr::across(c(X,Z,Y, temp_k), ~round(., 2)))

    A <- df_lit_data$A[1]
    B <- df_lit_data$B[1]
    C <- df_lit_data$C[1]


    combined_plot <- plotly::plot_ly() |>
      plotly::add_trace(
        x = d$x, y = d$y, z = d$temp_k,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = d$temp_k, opacity = 0.3, colorscale = "Rainbow"),
        name = "Fig",
        text = paste(paste(A, "=", d$X),
                     paste(B, "=", d$Z),
                     paste(C, "=", d$Y),
                     paste(" "),
                     paste("Temp =", d$temp_k),
                     sep = "\n"),
        hoverinfo = "text"
      ) |>
      plotly::add_trace(
        x = cartesian_df_lit_data$x, y = cartesian_df_lit_data$y, z = cartesian_df_lit_data$temp_k,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = 'black'),
        name = "Lit. data",
        text = paste(paste(A, "=", df_lit_data$X),
                     paste(B, "=", df_lit_data$Z),
                     paste(C, "=", df_lit_data$Y),
                     paste(" "),
                     paste("Temp =", df_lit_data$temp_k),
                     sep = "\n"),
        hoverinfo = "text"
      ) |>
      plotly::add_trace(
        data = annotations,
        x = ~x, y = ~y, z = ~z,
        text = ~text,
        type = "scatter3d",
        mode = "text",
        showlegend = FALSE
      ) |>
      plotly::layout(xaxis= list(showticklabels = FALSE),
                      height = 1080) |>
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     responsive = TRUE,
                     modeBarButtons = list(list("toImage", "resetScale2d")),
                     toImageButtonOptions = list(format = "svg", filename = "3D_plot"),
                     modeBarButtonsToRemove = list()
                     )

    shinyWidgets::show_toast(title = "Plotted", text = "",timer = 4000,width = 800,type = "success",timerProgressBar = TRUE,position = "bottom")

    combined_plot
}#, error = function(e){
  #shinyWidgets::show_toast(title = "Error", text = e$message,timer =30000,width = 800,type = "success",timerProgressBar = TRUE,position = "bottom")
#})
)
})



# translating fig file function
read_fig_to_dataframe <- function(file_path) {
  lines <- readLines(file_path)
  lines_filtered <- grep("^130EXP|^801LIN", lines, value = TRUE)
  data_list <- list()
  group_id <- 0
  temperature <- NA

  for (line in lines_filtered) {
    if (stringr::str_detect(line, "^801LIN")) {
      group_id <- group_id + 1
      temp_match <- stringr::str_extract(line, "\\d+\\s*K")
      if (!is.na(temp_match)) {
        temperature <- as.numeric(stringr::str_extract(temp_match, "\\d+"))
      }
    } else if (stringr::str_detect(line, "^130EXP")) {
      parts <- stringr::str_split(line, "\\s+")[[1]]
      x <- as.numeric(parts[3])
      y <- as.numeric(parts[4])
      z <- as.numeric(parts[5])
      data_list[[length(data_list) + 1]] <- data.frame(
        X = x,
        Y = y,
        Z = z,
        group = group_id,
        temperature = temperature
      )
    }
  }

  final_data <- do.call(rbind, data_list)
  return(final_data)
}
  })
}

## To be copied in the UI
# mod_ternary_phase_equilibria_ui("ternary_phase_equilibria_1")

## To be copied in the server
# mod_ternary_phase_equilibria_server("ternary_phase_equilibria_1")
