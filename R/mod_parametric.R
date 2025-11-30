#' parametric UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parametric_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(column(6, bs4Dash::box(title = "Parametric approach for molten salts", "", width=12, status = "primary", solidHeader = TRUE,
                                           shiny::uiOutput(outputId = ns("para_output_ui"))
    )), column(6, bs4Dash::box(width = 12,title = "Instructions", status = "primary", solidHeader = TRUE, withMathJax(HTML("

 <h3>Use this page to calculate parametric quantities for salts.</h3><br>

    <h4>Guidelines:</h4><br>

    <h5>\\( \\delta_{12} \\)</h5>
    <ul>
        <li>\\( \\delta_{12} \\), which is readily calculable based on Shannon radii differences, provides an empirical method to predict \\( \\Delta_{mix}H \\) in pseudo-binary salt systems.</li>
        <li>The theory was first used by H. T. Davis, “<i>Theory of Heats of Mixing of Certain Charge‐Unsymmetrical Fused Salts</i>”, The Journal of Chemical Physics, vol. 41, no. 9, pp. 2761–2766, Nov. 1964, doi: 10.1063/1.1726349.</li>
        <li>It is defined as \\( \\delta_{12}= \\frac{\\left( r_1^++r_1^- \\right) - \\left( r_2^++r_2^- \\right)}{\\left( r_1^++r_1^- \\right) \\times \\left( r_2^++r_2^- \\right)} \\)  and has units of \\(  Å^{-1} \\).</li>
        <li>Shannon radii utilised here originate from R. D. Shannon, “<i>Revised effective ionic radii and systematic studies of interatomic distances in halides and chalcogenides</i>,” Acta Cryst A, vol. 32, no. 5, pp. 751–767, Sep. 1976, doi: 10.1107/S0567739476001551, and were retrieved from the <i>Database of Ionic Radii</i> maintained by the Atomistic Simulation Group, Materials Department, Imperial College (http://abulafia.mt.ic.ac.uk/shannon/ (accessed 2025-02-15).</li>
    </ul>

"
)))),

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



  )
}

#' parametric Server Functions
#'
#' @noRd
mod_parametric_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$calc_button, {
      showNotification(id = "calculating",ui = "Calculating...",type = "message", duration = NULL)
      tryCatch({
              input$para_output |>
        rhandsontable::hot_to_r() |>
        janitor::clean_names() |>
        dplyr::rowwise() |>
        dplyr::mutate(delta = get_delta_from_scratch(species_1, species_2)) |>
        table_data()
        shiny::removeNotification(id = "calculating")
        showNotification(ui = "Calculated",type = "warning", duration = 5)
      }, error = \(e){
        showNotification(ui = "Error: check species",type = "error", duration = 5)
      })
    })

    table_data = reactiveVal(
      dplyr::tibble(species_1 = rep("LiF", 15),
                    species_2 = c("LaF3", "CeF3", "PrF3", "NdF3", "PmF3", "SmF3", "EuF3", "GdF3", "TbF3", "DyF3", "HoF3", "ErF3", "TmF3", "YbF3", "LuF3"),
                    delta = rep("", 15))
    )
    output$para_output_ui <- renderUI({
      tagList(
        shinyWidgets::actionBttn(inputId = ns("calc_button"), label = "Calculate", style = "material-flat", color="primary", class = "green-button", block = TRUE),
        rhandsontable::rHandsontableOutput(ns("para_output"))
      )
    })
    output$para_output <- rhandsontable::renderRHandsontable({
      table_data() |>
        convert_column_names() |>
        rhandsontable::rhandsontable() |>
        rhandsontable::hot_col("Delta", format = "0.00000") |>
        rhandsontable::hot_col("Species 1", renderer = javascript_element_formatting) |>
        rhandsontable::hot_col("Species 2", renderer = javascript_element_formatting) |>
        rhandsontable::hot_table(stretchH = "all")
    })

    dat <- data.table::fread("data/parametric/ionic_radii.csv", skip=17)
    coord_VI_dat <- dat |> dplyr::filter(Coordination == "VI") |>
      dplyr::group_by(Ion, Charge, Coordination) |>
      dplyr::slice(1)

    parse_formula <- function(formula) {
      m <- stringr::str_match_all(formula, "([A-Z][a-z]?)(\\d*)")[[1]]
      data.frame(
        element = m[, 2],
        count   = as.integer(ifelse(m[, 3] == "", 1L, m[, 3])),
        stringsAsFactors = FALSE
      )
    }
    calculate_delta <- function(r_plus_1, r_minus_1, r_plus_2, r_minus_2) {
      return(
        ((r_plus_1+r_minus_1)-(r_plus_2+r_minus_2))/((r_plus_1+r_minus_1)*(r_plus_2+r_minus_2))
      )
    }

    valid_elements <- c("H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne",
                        "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca",
                        "Sc", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn",
                        "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr",
                        "Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn",
                        "Sb", "Te", "I", "Xe", "Cs", "Ba", "La", "Ce", "Pr", "Nd",
                        "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb",
                        "Lu", "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg",
                        "Tl", "Pb", "Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th",
                        "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm",
                        "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds",
                        "Rg", "Cn", "Nh", "Fl", "Mc", "Lv", "Ts", "Og")

    get_delta_from_scratch <- function(species_1, species_2){
      element_table_1 <- parse_formula(species_1)
      element_table_2 <- parse_formula(species_2)

      T_plus_1 <- coord_VI_dat |> dplyr::filter(Ion==element_table_1[1,1],
                                                Charge == element_table_1[2,2])
      T_minus_1 <- coord_VI_dat |> dplyr::filter(Ion == element_table_1[2,1],
                                                 Charge == -element_table_1[1,2])
      T_plus_2 <- coord_VI_dat |> dplyr::filter(Ion==element_table_2[1,1],
                                                Charge == element_table_2[2,2])
      T_minus_2 <- coord_VI_dat |> dplyr::filter(Ion == element_table_2[2,1],
                                                 Charge == -element_table_2[1,2])

      table_used_to_caluculate_delta <- dplyr::bind_rows(T_plus_1, T_minus_1, T_plus_2, T_minus_2)

      delta_parameter <- -(calculate_delta(T_plus_1$`Ionic Radius`,
                                           T_minus_1$`Ionic Radius`,
                                           T_plus_2$`Ionic Radius`,
                                           T_minus_2$`Ionic Radius`))

      return(delta_parameter)
    }

    #end functions
javascript_element_formatting <- "
                               function (instance, td, row, col, prop, value, cellProperties) {
                                 const elements = [
                                   'H','He','Li','Be','B','C','N','O','F','Ne','Na','Mg','Al','Si','P','S','Cl','Ar',
                                   'K','Ca','Sc','Ti','V','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se',
                                   'Br','Kr','Rb','Sr','Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn',
                                   'Sb','Te','I','Xe','Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy',
                                   'Ho','Er','Tm','Yb','Lu','Hf','Ta','W','Re','Os','Ir','Pt','Au','Hg','Tl','Pb',
                                   'Bi','Po','At','Rn','Fr','Ra','Ac','Th','Pa','U','Np','Pu','Am','Cm','Bk','Cf',
                                   'Es','Fm','Md','No','Lr','Rf','Db','Sg','Bh','Hs','Mt','Ds','Rg','Cn','Nh','Fl',
                                   'Mc','Lv','Ts','Og'];

                                  td.innerText = value ?? '';

                                  if (value === null || value === undefined || value === '') {
                                    td.style.background = '';
                                    td.style.setProperty('color', 'black', 'important');
                                    return td;
                                  }


                                 td.innerText = value;  // Ensure text is shown
                                  td.style.setProperty('color', 'black', 'important');

                                 const match = value.match(/^([A-Z][a-z]?)([A-Z][a-z]?)(\\d?)$/);
                                 if (!match) {
                                   td.style.background = 'red';
                                   return td;
                                 }

                                 const el1 = match[1];
                                 const el2 = match[2];

                                 if (!elements.includes(el1) || !elements.includes(el2)) {
                                   td.style.background = 'red';
                                  td.style.setProperty('color', 'black', 'important');
                                 } else {
                                   td.style.background = '';  // Clear background if valid
                                   td.style.color = 'black';
                                 }

                                 return td;
                               }
                               "
  })
}

## To be copied in the UI
# mod_parametric_ui("parametric_1")

## To be copied in the server
# mod_parametric_server("parametric_1")
