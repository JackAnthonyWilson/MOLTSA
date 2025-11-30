#' database_file_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_database_file_comparison_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' database_file_comparison Server Functions
#'
#' @noRd 
mod_database_file_comparison_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_database_file_comparison_ui("database_file_comparison_1")
    
## To be copied in the server
# mod_database_file_comparison_server("database_file_comparison_1")
