#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
library(shinymanager)

my_theme <- fresh::create_theme(
  fresh::bs4dash_status(
    #primary = "#73000a",  # Change all primary buttons to red
    primary = "#2e8b57",
    success = "#FFF2E3",  # Success buttons to green
    danger = "#CC2E40"    # Danger buttons to orange
  )
)

tags$head(tags$style(HTML("
  .green-button {
    background-color: #2e8b57 !important; /* Green color */
    color: white !important; /* Text color */
    border-color: #2e8b57 !important; /* Optional: border */
  }
  .green-button:hover {
    background-color: 236a42ff !important; /* Darker green on hover */
  }
")))

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(

      fullscreen = FALSE,
      help = NULL,
      dark = NULL,
      freshTheme = TRUE,
      header = bs4Dash::dashboardHeader(
        title = tags$a(
          href = "https://moltsa.com",
          class = "brand-link",
          style = "display: flex; align-items: center;",
          tags$img(
            src = "www/just_logo.svg",
            height = "30px",
            class = "brand-image",
            style = "margin-right: 20px; margin-left: 17px;"
          ),
          tags$span(class = "brand-text font-weight-light", HTML("<b>MOLTSA</b>"))
        )
      ),
      sidebar = bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          bs4Dash::menuItem("Home", tabName = "home", icon = icon("house-chimney")),
          tags$strong("MSTDB-TC data"),
          bs4Dash::menuItem("Phase diagrams", tabName = "phase_equ", icon = icon("shapes")),
          bs4Dash::menuItem("Enthalpy of mixing", tabName = "H_mix", icon = icon("vials")),
          bs4Dash::menuItem("Contribute data", tabName = "upload", icon = icon("arrow-up-from-bracket")),
        #  bs4Dash::menuItem("Activities", tabName = "activity", icon = icon("atom")), COMING SOON
         # bs4Dash::menuItem("Vapor Pressures", tabName = "vapor_pressure", icon = icon("mug-hot")), COMING SOON
        tags$strong("Research tools"),
          bs4Dash::menuItem("3D ternary fig viewer", tabName = "tern", icon = icon("mountain")),

        bs4Dash::menuItem("Optimiser files", tabName = "json", icon = icon("file-lines")),
      bs4Dash::menuItem("Parametric", tabName = "parametric", icon = icon("diagram-project")),
      bs4Dash::menuItem("Cp solver", tabName = "cp_solver", icon = icon("calculator")),
        bs4Dash::menuItem("DSC calibration", tabName = "dsc", icon = icon("chart-column")),
      #bs4Dash::menuItem("Database QC", tabName = "database_file_comparison", icon = icon("database")), COMING SOON
      tags$strong("Miscellaneous"),
      bs4Dash::menuItem("Contributors", tabName = "credits", icon = icon("user-group"))
        )
      ),
      body = bs4Dash::dashboardBody(
        fresh::use_theme(my_theme),
        bs4Dash::tabItems(bs4Dash::tabItem("phase_equ", mod_splashpage_ui("splashpage_1")),
                          bs4Dash::tabItem("H_mix", mod_h_mix_ui("h_mix_1")),
                       #   bs4Dash::tabItem("activity", mod_activity_ui("activity_1")),
                        #  bs4Dash::tabItem("vapor_pressure", mod_vapor_pressure_ui("vapor_pressure_1")),
                          bs4Dash::tabItem("tern", mod_ternary_phase_equilibria_ui("ternary_phase_equilibria_1")),
                          bs4Dash::tabItem("upload", mod_upload_ui("upload_1")),
                          bs4Dash::tabItem("dsc",mod_dsc_ui("dsc_1")),
                       bs4Dash::tabItem("json", mod_json_ui("json_1")),
                       bs4Dash::tabItem("parametric", mod_parametric_ui("parametric_1")),
                       bs4Dash::tabItem("credits", mod_credits_ui("credits_1")),
                       bs4Dash::tabItem("home", mod_home_ui("home_1")),
                       bs4Dash::tabItem("cp_solver", mod_cp_solver_ui("cp_solver_1"))
                      # bs4Dash::tabItem("data_base_file_comparison", mod_database_file_comparison_ui("database_file_comparison_1")) COMING SOON
                       )


      )
    ),
 # The following bit suppresses the javascript error message for the highcharter plot when no variable is selected
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),

 )
}

# Valid themes are: cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti.

library(shinythemes)

# app_ui = shinymanager::secure_app(
#   app_ui,
#   theme = shinythemes::shinytheme("lumen"),
#   status = "primary",
#   head_auth = tags$head(
#     tags$link(rel = "shortcut icon"),
#     tags$style(HTML("
#       /* Target the login button in shinymanager */
#       .btn-primary, .auth-wrapper .btn-primary {
#         background-color: #2e8b57 !important; /* Garnet color */
#         color: white !important; /* Text color */
#         border-color: #2e8b57 !important; /* Optional: border */
#       }
#       .btn-primary:hover, .auth-wrapper .btn-primary:hover {
#         background-color: #236a42ff !important; /* Darker garnet on hover */
#       }
#     "))
#   ),
#    tags_top = tagList( tags$img(
#     src = "https://moltsa.com/favicon.png",
#     # height = "30px",
#     width = 120, #240
#     style = "margin-bottom: 50px;"
#     ),
#   tags$p(HTML("Welcome to <strong>moltsa.com</strong>.<br>If you do not have a username and password, please contact the site administrator.")
# ))
# )








#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(

    tags$link(rel = "shortcut icon", href = "www/favicon.ico", type = "image/x-icon"),

    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MoltenSalts"
    )
  )
}
