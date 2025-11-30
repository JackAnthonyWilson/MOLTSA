#' vapor_pressure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vapor_pressure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Vapor pressures")

  )
}

#' vapor_pressure Server Functions
#'
#' @noRd
mod_vapor_pressure_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_vapor_pressure_ui("vapor_pressure_1")

## To be copied in the server
# mod_vapor_pressure_server("vapor_pressure_1")
