#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(column(4, bs4Dash::box(title = "Download templates", "", width=12,status = "primary", solidHeader = TRUE,
                 shiny::selectInput(label = "Select template", inputId = ns("template"), choices = list("Select data type", "Binary" = c("Phase equilibria", "Enthalpy of mixing"))),
                 shiny::downloadButton(outputId = ns("template_download"), label = "Download")),
    bs4Dash::box(title = "Upload dataset", width = 12,status = "primary", solidHeader = TRUE,
                 shiny::selectInput(label = "Select data type", inputId = ns("dropdown"),
                                    choices = list("Select data type", "Binary" = c("Phase equilibria", "Enthalpy of mixing"))),
                 shiny::fileInput(inputId = ns("uploaded_file"), accept = ".csv", label = "Upload data"),
                 shinyWidgets::actionBttn(inputId = ns("validate"), label = "Validate", style = "material-flat", color="default", block = FALSE, class = "green-button"))


  ), column(8, bs4Dash::box(width = 12,title = "Instructions",status = "primary", solidHeader = TRUE, HTML("

If you want your data to appear on <i>moltsa.com</i> or <i>moltensalts.net</i>, use this page to upload it. Templates are provided on the left, and the column headers are explained below:
<br></br>
<li><b>Species 1</b>: Provide the chemical formula of species 1 (e.g., NaCl, KCl, UCl<sub>3</sub>).</li>
<li><b>Species 2</b>: Provide the chemical formula of species 2 (e.g., NaCl, KCl, UCl<sub>3</sub>).</li>
<li><b>Mole frac species 1</b>: Enter the mole fraction of species 1 (e.g., 0.00–1.00). The total of all species must sum to 1.00.</li>
<li><b>Mole frac species 2</b>: Enter the mole fraction of species 2.</li>
<li><b>Temp (K)</b>: Enter the temperature in Kelvin.</li>
<li><b>deltaH</b>: Enter the enthalpy of mixing in Jmol<sup>–1</sup>.</li>
<li><b>Author</b>: Provide the author(s) of the dataset, ensuring correct spelling and the format <i>Last Name</i> (<i>year</i>). For two authors, use both names separated by “and,” for three or more, use “et al.” (e.g., Smith et al. (2017)).</li>
<li><b>Type</b>: Provide additional detail about the data (e.g., “liquidus” or “solidus”), or leave it blank.</li>
<li><b>Link</b>: Provide a DOI link if possible so the data can be verified. Otherwise, include a link to where the dataset is hosted (e.g., ResearchGate).</li>
<br></br>
The templates are designed to accommodate multiple datasets from different authors and chemical systems, allowing you to upload them all at once. Files must be in .csv format with unaltered column headers. After filling in your data, use the file upload system on the left to submit your dataset.
<br></br>
<u>What happens next?</u>
<br></br>
Your data will be verified, including checks for duplicates and correct formatting, and we will reference the original papers where possible. Once verified, your dataset will be added to the <i>moltsa.com</i>/<i>moltensalts.net</i> database.")))),
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


)
}

#' upload Server Functions
#'
#' @noRd
mod_upload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$validate, {
      shiny::req(input$uploaded_file)
      tryCatch({
        if (input$dropdown == "Select data type") {stop("Must select data type")}
        uploaded_file <- data.table::fread(input$uploaded_file$datapath)
        required_headers <- c("Species 1", "Species 2", "Mole frac species 1", "Mole frac species 2", "Temp (K)", "Author", "Type", "Link")
        needed = required_headers[which(!required_headers %in% colnames(uploaded_file))]
        if (length(needed) > 0) {stop(paste0("Missing headers: "), needed |> paste(collapse = ", "))}

        output$data_validate_ui = shiny::renderUI({
          length <- uploaded_file |> nrow()
          authors <- uploaded_file |> dplyr::distinct(Author) |> dplyr::arrange(Author)|> dplyr::pull(Author) |> paste(collapse = ", ")
          tagList(
            glue::glue("You are about to upload {length} rows from the following authors: {authors}. <br><br>

                       <i>Submitted datasets and associated contributor metadata are stored for scientific curation and dissemination within MOLTSA. Personal information is limited to what is necessary for attribution and platform operation. By submitting data, you confirm your consent in accordance with the <a href='https://moltsa.com/privacy-policy.html' target='_blank' rel='noopener noreferrer'>MOLTSA Privacy Policy</a>. </i>
<br><br>
<b>By clicking upload, you are agreeing to the MOLTSA Privacy Policy and confirm you have the right to submit these data.</b>
                       ") |> HTML(),
            shiny::textInput(ns("email_address"), label ="Email address", placeholder = "JohnSmith@sc.edu")
            )
        })

        shiny::showModal(shiny::modalDialog(title = "Ready to upload?", easyClose = FALSE,fade = TRUE,
                                            footer = tagList(shinyWidgets::actionBttn(ns("upload"), "Upload", style = "material-flat", block=FALSE, color = "primary"),
                                                             shinyWidgets::actionBttn(ns("close_modal"), "Cancel", style = "material-flat", block=FALSE)
                                                             #shiny::modalButton("Cancel")
                                                             ),
                                            shiny::uiOutput(ns("data_validate_ui"))))

        }, error = function(i){
        shinyWidgets::show_toast(title ="Error", text = i$message, width = 900,type = "error",timer = 20000)
      })

    })


    observeEvent(input$close_modal, {
      shiny::removeModal()
    })

    observeEvent(input$upload, {
      tryCatch( {
        shiny::req(input$email_address)
        if (input$email_address == "") {stop("Email field is empty")}
        if (!stringr::str_detect(input$email_address, "^[A-Za-z0-9._%+\\-]+@[A-Za-z0-9.\\-]+\\.[A-Za-z]{2,}$")) {stop("Email address not valid.")}
        body = glue::glue("
                          ### Moltsa.com confirmation <br><br>
                          To: {input$email_address}<br><br>

                          This confirms your submission of {input$dropdown} data to Moltsa.com on {date()}<br><br>
                          Kind Regards<br><br>
                          The Moltsa.com team
                          ")

        email <- blastula::compose_email(
                                body = blastula::md(body),
                                footer = "Please do not reply to this email",
                                title = "Comfirmation of submission") |>
          blastula::add_attachment(
            file = input$uploaded_file$datapath,
            filename = glue::glue("{input$dropdown}-{Sys.Date()}.csv")
          )

        email |> blastula::smtp_send(to = "jackwilson.mchem@gmail.com",#input$email_address,
                                     from = "moltensalts.net.noreply@gmail.com",
                                     subject = glue::glue("Submission successful {Sys.Date()}"),
                                     bcc = "moltensalts.net.noreply@gmail.com",
                                     credentials = blastula::creds_file("gmail_key"))
        shinyWidgets::show_toast(title = "Success", text = "Thanks for your submission. Check your email", width=900, type="success", timer=20000)

              }, error = function(i){
        shinyWidgets::show_toast(title = "Error", text = i$message, width=900, type="error", timer=20000)
      })
    })

    output$template_download <- shiny::downloadHandler(
      filename = function(){
        paste0(input$template, ".csv")
      },
      content = function(e){
        tryCatch({
          if (input$template == "Select data type") {stop("Must select data type")}
          if (input$template == "Phase equilibria") {
            template = dplyr::tibble("Species 1" = "",
                                     "Species 2" = "",
                                     "Mole frac species 1" ="",
                                     "Mole frac species 2" = "",
                                     "Temp (K)" = "",
                                     "Author" = "",
                                     "Type" = "",
                                     "Link" = "")
            }
          if (input$template == "Enthalpy of mixing") {
            template = dplyr::tibble("Species 1" = "",
                                     "Species 2" = "",
                                     "Mole frac species 1" ="",
                                     "Mole frac species 2" = "",
                                     "Temp (K)" = "",
                                     "deltaH" = "",
                                     "Author" = "",
                                     "Type" = "",
                                     "Link" = "")
          }
          data.table::fwrite(template, e, na = "", row.names = FALSE)
                    }, error = function(i){
          shinyWidgets::show_toast(title ="Error", text = i$message, width = 700,type = "error")
        }
        )
      }
    )


  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")
