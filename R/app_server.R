#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#'
#' @noRd

library(shinymanager)
shinymanager::set_labels("en", "Please authenticate" ="")

credentials = data.frame(user = c("USC"),
                        password = c("c2NyeXB0ABEAAAAIAAAAATNV+I14gNFd1WNTWTOBAzHj0VnR5RtskTdsKSQt69cA/J3ICuEr3ipQfEF/9mO4XlTqbaIdFyisTkvGmLYmTAHzazoang5pyKjaGJkfVf0m"),
                        admin = c(TRUE),
                        comment = "",
                        is_hashed_password = TRUE,
                        stringsAsFactors = FALSE)

app_server <- function(input, output, session) {
  res_auth = shinymanager::secure_server(timeout = 0,
                                         keep_token = TRUE,
                                         check_credentials = shinymanager::check_credentials(credentials))
output$auth_output = renderPrint({
  reactiveValuesToList(res_auth)
})
    # Your application server logic
  mod_splashpage_server("splashpage_1")
  mod_h_mix_server("h_mix_1")
  mod_activity_server("activity_1")
  mod_vapor_pressure_server("vapor_pressure_1")
  mod_ternary_phase_equilibria_server("ternary_phase_equilibria_1")
  mod_upload_server("upload_1")
  mod_dsc_server("dsc_1")
  mod_json_server("json_1")
  mod_parametric_server("parametric_1")
  mod_credits_server("credits_1")
  mod_home_server("home_1")
  mod_cp_solver_server("cp_solver_1")
  mod_database_file_comparison_server("database_file_comparison_1")
}



