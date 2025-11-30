#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
      bs4Dash::box(
        width = 12,
        title = "Welcome",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        fluidRow(
          column(
            width = 8,
            tags$img(
              src = "www/binaries_matrix.svg",
              width = "100%",
              style = "border-radius: 12px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
            )
          ),
          column(
            width = 4,
            p(HTML("<b>Welcome to moltsa.com</b>. The graphic displays the pseudo-binary systems included in version 3.1 of the <i>Molten Salt Thermal Properties Database – Thermochemical</i> (MSTDB–TC). The MSTDB–TC itself is developed by the <a href='https://gacenterusc.org/' target='_blank'>General Atomics Center</a> at the University of South Carolina and is accessible at <a href='https://mstdb.ornl.gov' target='_blank'>mstdb.ornl.gov</a>."
                   ))
          #   tags$img(
          #     src = "www/just_logo.svg",
          #     width = "55%",
          #     style = "border-radius: 12px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
          # )
        )
      ),
      p(HTML("<hr>")),
      fluidRow(column(width = 7,
                      p(HTML("

                             <b><u>What is this web app for?</b></u><br>

                             This web app is designed to make it easier to explore and compare phase equilibria, enthalpy of mixing data, and phase diagram models relevant to molten salt research. Built with an open-science mindset, it removes the need for specialised thermochemical software by offering a clean, intuitive interface for researchers, educators, and industry professionals. Users can visually compare experimental data with MSTDB–TC model predictions, making it a practical tool for quality checks and model validation. The platform also encourages collaboration by making it simple to share findings and refine models. Additional features include access to optimisation files, 3D liquidus surface visualisations, and tools for calculating parameters of interest in molten salt systems. The name moltsa.com – a portmanteau of MOLTen SAlt – reflects the app’s focus on thermochemical data and modelling in this field.
                             <br><br>

                             <b><u>MSTDB–TC data tabs</b></u> <br>
                             <li><b>Phase equilibria</b>: Assessed pseudo-binary phase diagrams from MSTDB–TC with phase equilibria data from literature.</li>
                             <li><b>Enthalpy of mixing</b>: Enthalpy of mixing for pseudo-binary systems with values from literature.</li>
                             <li><b>Contribute data</b>: Contribute your own phase equilibria or enthalpy of mixing data to the project.</b></li>
                             <br>
                             <b><u>Research tools tabs</b></u><br>
                             <li><b>3D ternary fig viewer</b>: View your own liquidus project .fig files in 3D.</li>
                             <li><b>Optimiser files</b>: .json files used to optimise chemical systems in CALPHAD optimisation software.</li>
                             <li><b>Parametric</b>: Correlational and parametric calculations for salts.</li>
                             <li><b>Cp solver</b>: Fit Maier-Kelley coefficients to heat capacity data.</li>
                             <li><b>DSC calibration</b>: Employ DSC temperature corrections and perform error analysis.</li>
                             <br>
                             <b><u>Miscellaneous tabs</b></u><br>
                             <li><b>Contributors</b></li>

                             "))),
               column(width = 4,
                      p(HTML("")))),
      fluidRow()),
      bs4Dash::box(
        width = 12,
        collapsible = FALSE,
        closable = FALSE,
        title = "Disclaimer", status = "primary", solidHeader = TRUE,
        p(HTML("This web application was developed independently, in part to support the use and visualisation of MSTDB–TC data, and is not an officially sponsored product."))),



  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
