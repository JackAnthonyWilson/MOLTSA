#' json UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_json_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(column(4,
       bs4Dash::box(title = "Search", width = 12, status = "primary", solidHeader = TRUE,
                    shiny::selectInput(inputId = ns("select_elements"), label="Elements", choices = elements, multiple = TRUE),
                    shiny::checkboxInput(inputId = ns("strict_search"), label = "Strict search"),
      shiny::fluidRow(
        bs4Dash::actionButton(inputId = ns("search"), label = "Search database"),
        shiny::downloadButton(outputId = ns("download_results"), label = "Download selected files"))
                    ),
       bs4Dash::box(title = "Output", width = 12, status = "primary", solidHeader = TRUE,
                    DT::DTOutput(ns("output_table")))),
       column(8, bs4Dash::box(width = 12,title = "Instructions", status = "primary", solidHeader = TRUE, HTML("

                      <h3>Use this page to download ready-made experiment files (<code>.exp</code>) for CALPHAD optimisations.</h3><br><br>
                      Use the search bar on the left to filter by specific elements. <br>
                      Enable <b>Strict search</b> to exclude files elements not selected.
                      "))))
  )
}

elements <- c(
  "H","He","Li","Be","B","C","N","O","F","Ne",
  "Na","Mg","Al","Si","P","S","Cl","Ar","K","Ca",
  "Sc","Ti","V","Cr","Mn","Fe","Co","Ni","Cu","Zn",
  "Ga","Ge","As","Se","Br","Kr","Rb","Sr","Y","Zr",
  "Nb","Mo","Tc","Ru","Rh","Pd","Ag","Cd","In","Sn",
  "Sb","Te","I","Xe","Cs","Ba","La","Ce","Pr","Nd",
  "Pm","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb",
  "Lu","Hf","Ta","W","Re","Os","Ir","Pt","Au","Hg",
  "Tl","Pb","Bi","Po","At","Rn","Fr","Ra","Ac","Th",
  "Pa","U","Np","Pu","Am","Cm","Bk","Cf","Es","Fm",
  "Md","No","Lr","Rf","Db","Sg","Bh","Hs","Mt","Ds",
  "Rg","Cn","Nh","Fl","Mc","Lv","Ts","Og"
)

#' json Server Functions
#'
#' @noRd
mod_json_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    data = reactiveVal(dplyr::tibble())
    experiments = reactiveVal(dplyr::tibble())

    observeEvent(input$search, {
      if (input$strict_search) {

        element_table |>
          dplyr::mutate(contains = value %in% input$select_elements) |>
          dplyr::group_by(file_path) |>
          dplyr::filter(all(contains)) |>
          dplyr::summarize(elements = paste(unique(value), collapse = ", ")) |>
          dplyr::distinct_all() |>
          data()

      } else {
        element_table %>%
          dplyr::group_by(file_path) %>%
          dplyr::filter(all(input$select_elements %in% value)) %>%
          dplyr::summarize(elements = paste(unique(value), collapse = ", ")) |>
          dplyr::distinct_all() |>
          data()
      }
    })

    output$output_table <- DT::renderDT({
      data() |>
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
                      rownames = FALSE)
    })

    element_table <- list.files("./data/json/", full.names = TRUE) |>
    lapply(\(e) {
      json_file_path <- e
      e1 <- jsonlite::read_json(e)
      #e1 <- jsonlite::read_json("./data/json/Eutectic_LiF_LiBr_Sangster.exp")
      formulas <- e1[[1]]$components |> lapply(\(f) {
        f |> purrr::pluck("formula")}) |>
        unlist()
      extracted_elements <- stringr::str_extract_all(formulas, "[A-Z][a-z]?") |>
        unlist() |>
        unique() |>
        dplyr::as_tibble() |>
        dplyr::mutate(file_path = json_file_path)

      }) |>
      dplyr::bind_rows() |>
      dplyr::select(file_path, value) |>
      dplyr::mutate(file_path = stringr::str_extract(file_path, pattern = "[^/]+$"))

    output$download_results <- downloadHandler(
      filename = function() {
        paste0("Selected_Experiments_", Sys.Date(), ".zip")
      },
      content = function(file) {
        my_files <- data() %>%
          dplyr::pull(file_path)
        full_paths <- file.path("data", "json", my_files)
        tmpdir <- file.path(tempdir(), "temp_zip_folder")
        if (dir.exists(tmpdir)) unlink(tmpdir, recursive = TRUE)
        dir.create(tmpdir)
        file.copy(from = full_paths, to = tmpdir, overwrite = TRUE)
        old_wd <- setwd(tmpdir)
        on.exit({
          setwd(old_wd)
          unlink(tmpdir, recursive = TRUE)
        })
        utils::zip(zipfile = file, files = my_files)
      }
    )

  })
}

## To be copied in the UI
# mod_json_ui("json_1")

## To be copied in the server
# mod_json_server("json_1")
