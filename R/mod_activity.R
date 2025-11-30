#' activity UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_activity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Activities")
  )
}

#' activity Server Functions
#'
#' @noRd
mod_activity_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_activity_ui("activity_1")

## To be copied in the server
# mod_activity_server("activity_1")
