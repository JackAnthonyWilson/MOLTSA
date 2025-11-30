#' cp_solver UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cp_solver_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(column(4, bs4Dash::box(title = "Heat capacity solver", "", width=12, status = "primary", solidHeader = TRUE,
                                           fluidRow(
                                             column(6, sliderTextInput(
                                               inputId = ns("break1"),
                                               label = "Break 1:",
                                               choices = seq(0, 1000, 50),
                                               selected = 100,
                                               grid = TRUE
                                             )),
                                             column(6, sliderTextInput(
                                               inputId = ns("break2"),
                                               label = "Break 2:",
                                               choices = seq(0, 2000, 50),
                                               selected = 1500,
                                               grid = TRUE
                                             ),
                                             )
                                           ),
                                           shinyWidgets::actionBttn(inputId = ns("calc_button"), label = "Calculate parameters and update plot", style = "material-flat", color="primary", class = "green-button", block = TRUE),
                                           rhandsontable::rHandsontableOutput(ns("cp_input_table"))
    )), column(8, bs4Dash::box(width = 12,title = "Instructions", status = "primary", solidHeader = TRUE, withMathJax(HTML("

 <h3>Use this page to determine Maier-Kelley polynomial coefficients for heat capacity data.</h3><br>

    <h4>Guidelines:</h4>

    <ul>
        <li>Paste your heat capacity data into the table (units are Kelvin and \\( JK^{-1}mol^{-1}\\))</li>
        <li>Choose up to 2 breaks in the data — the algorithm will fit a stepwise function, using the breaks to define the steps.</li>
        <li>Click the button to update the plot and generate the parameter table.</li>
        <li>Vary the breaks using the slider inputs to obtain a good fit. As an example, using the default dataset, see that break 1 = 350, and break 2 = 1000 produce a great fit of the data.</li>
    </ul>

"
    ))), bs4Dash::box(width = 12, title = "Cp plot", status = "primary", solidHeader = TRUE, plotly::plotlyOutput(outputId = ns("cp_plot"), height = "400px", width = "100%")),
    bs4Dash::box(width = 12, status = "primary", solidHeader = TRUE, title = withMathJax("Parameter table. Cp is given in the form \\( a + b \\times T + c \\times T^{-2} + d \\times T^{2} \\)"), tableOutput(ns("param_table"))))),

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
    ))


}

#' cp_solver Server Functions
#'
#' @noRd
mod_cp_solver_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Initial dummy data
    data <- reactiveVal(data.frame(
      K = c(298,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000,
            1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,
            2300,2400,2500,2600,2700,2800,2900,3000,250,300,350,400,450,500,
            600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000),
      Cp = c(98.2264166,98.33859911,100.6225404,102.2006346,103.3397788,104.1872448,
             104.8318163,105.3308996,105.7236477,106.0377755,106.2933089,106.5047483,
             106.6823685,106.8330289,106.9606926,107.066769,107.066988,107.2419839,
             107.3756904,107.4801478,107.5632897,107.6305243,107.6856465,107.7313867,
             107.7697518,107.802245,107.8300095,107.8539266,107.8746827,107.8928164,
             107.9087525,107.9228262,107.9353013,107.9463839,107.9562321,107.9649638,
             107.9726631,94.5,97.9,100.3,101.9,103.1,104,105.2,105.9,106.4,106.8,
             107,107.2,107.3,107.4,107.5,107.6,107.7,107.7,107.8,107.8,107.8)
    ))

    output$cp_input_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(data(), useTypes = TRUE, stretchH = "all")
    })

    dat <- eventReactive(input$calc_button, {
      req(input$cp_input_table)
      df <- rhandsontable::hot_to_r(input$cp_input_table)
      validate(need(all(c("K", "Cp") %in% names(df)),
                    "Table must have 'K' and 'Cp' columns"))
      df
    })

    # Optimisation once: return functions + parameters
    fit_results <- eventReactive(input$calc_button, {
      data <- dat()
      req(data, input$break1, input$break2)

      break_1 <- data$K[which.min(abs(as.numeric(input$break1) - data$K))]
      break_2 <- data$K[which.min(abs(as.numeric(input$break2) - data$K))]

      block_1 <- dplyr::filter(data, K <= break_1)
      block_2 <- dplyr::filter(data, K >= break_1 & K <= break_2)
      block_3 <- dplyr::filter(data, K >= break_2)

      # Model form
      cp_model <- function(K, p) p[1] + p[2]*K + p[3]*K^(-2) + p[4]*K^2

      # Objective functions with constraints preserved
      objective_function_1 <- function(params, K, Y, target_X, target_Y) {
        predicted_Y <- cp_model(K, params)
        RSS <- sum((predicted_Y - Y)^2)
        constraint_violation_1 <- (500 - target_Y)^2
        RSS + constraint_violation_1
      }
      objective_function_2 <- function(params, K, Y, target_X, target_Y) {
        predicted_Y <- cp_model(K, params)
        RSS <- sum((predicted_Y - Y)^2)
        constraint_violation_2 <- (cp_model(target_X, res1$par) - target_Y)^2
        RSS + constraint_violation_2
      }
      objective_function_3 <- function(params, K, Y, target_X, target_Y) {
        predicted_Y <- cp_model(K, params)
        RSS <- sum((predicted_Y - Y)^2)
        constraint_violation <- (cp_model(target_X, res2$par) - target_Y)^2
        RSS + constraint_violation
      }

      # Targets
      target_X1 <- tail(block_1$K, 1); target_Y1 <- tail(block_1$Cp, 1)
      res1 <- optim(c(100, 0.05, -450000, -0.00005), objective_function_1,
                    K = block_1$K, Y = block_1$Cp,
                    target_X = target_X1, target_Y = target_Y1,
                    control = list(maxit = 100000))

      target_X2 <- tail(block_1$K, 1); target_Y2 <- tail(block_1$Cp, 1)
      res2 <- optim(c(100, 0.05, -450000, -0.00005), objective_function_2,
                    K = block_2$K, Y = block_2$Cp,
                    target_X = target_X2, target_Y = target_Y2,
                    control = list(maxit = 10000000))

      target_X3 <- tail(block_2$K, 1); target_Y3 <- tail(block_2$Cp, 1)
      res3 <- optim(c(100, 0.05, -450000, -0.00005), objective_function_3,
                    K = block_3$K, Y = block_3$Cp,
                    target_X = target_X3, target_Y = target_Y3,
                    control = list(maxit = 10000000))

      # Functions
      f1 <- function(K) cp_model(K, res1$par)
      f2 <- function(K) cp_model(K, res2$par)
      f3 <- function(K) cp_model(K, res3$par)

      params <- dplyr::bind_rows(
        data.frame(Start = min(block_1$K), End = max(block_1$K),
                   a = res1$par[1], b = res1$par[2],
                   c = res1$par[3], d = res1$par[4]),
        data.frame(Start = min(block_2$K), End = max(block_2$K),
                   a = res2$par[1], b = res2$par[2],
                   c = res2$par[3], d = res2$par[4]),
        data.frame(Start = min(block_3$K), End = max(block_3$K),
                   a = res3$par[1], b = res3$par[2],
                   c = res3$par[3], d = res3$par[4])
      )

      list(params = params,
           functions = list(f1, f2, f3))
    })

    # Table displays exact coefficients used for plot
    output$param_table <- renderTable({
      req(fit_results())
      fit_results()$params %>%
        dplyr::mutate(
          Start = formatC(Start, format = "f", digits = 9),
          End   = formatC(End,   format = "f", digits = 9),
          a     = formatC(a, format = "f", digits = 9),
          b     = formatC(b, format = "f", digits = 9),
          c     = formatC(c, format = "f", digits = 9),
          d     = formatC(d, format = "f", digits = 9)
        )
    })

    # Plotly plot (unchanged themes, just fed functions from fit_results)
    output$cp_plot <- plotly::renderPlotly({
      req(dat(), fit_results())
      data <- dat()
      funcs <- fit_results()$functions
      params <- fit_results()$params

      p <- ggplot2::ggplot(data, ggplot2::aes(K, Cp)) +
        ggplot2::geom_point() +
        ggplot2::stat_function(fun = funcs[[1]],
                               colour="orange", linewidth=1,
                               xlim = c(params$Start[1], params$End[1])) +
        ggplot2::stat_function(fun = funcs[[2]],
                               colour="yellow", linewidth=1,
                               xlim = c(params$Start[2], params$End[2])) +
        ggplot2::stat_function(fun = funcs[[3]],
                               colour="dodgerblue", linewidth=1,
                               xlim = c(params$Start[3], params$End[3])) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(axis.text = ggplot2::element_text(colour="black"),
                       panel.border = ggplot2::element_rect(colour = "black", linewidth = 1),
                       legend.title = ggplot2::element_blank(),
                       legend.background = ggplot2::element_rect(colour="black"),
                       linewidth = 0.5) +
        ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~.,labels = NULL), n.breaks = 10) +
        ggplot2::scale_x_continuous(sec.axis = ggplot2::sec_axis(~.,labels = NULL),n.breaks = 10)+
        ggplot2::labs(x = "Temperature (K)",
                      y = "Cp (J/Kmol)")

      plotly::ggplotly(p, dynamicTicks = TRUE) |>
        plotly::config(displayModeBar = TRUE,
                       modeBarButtonsToRemove = c("zoom2d", "pan2d", "autoScale2d",
                                                  "select2d", "lasso2d",
                                                  "hoverCompareCartesian","hoverClosestCartesian"),
                       displaylogo = FALSE)
    })
  })
}



## To be copied in the UI
# mod_cp_solver_ui("cp_solver_1")

## To be copied in the server
# mod_cp_solver_server("cp_solver_1")
