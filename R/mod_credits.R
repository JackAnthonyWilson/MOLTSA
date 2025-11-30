#' credits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_credits_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # credit for JACK ANTHONY WILSON
       bs4Dash::userBox(
      label = bs4Dash::bs4Badge("", rounded = TRUE, color = "secondary"),
      collapsible = FALSE,
      boxToolSize = "xs",
      title = bs4Dash::userDescription(
        title = "Jack Anthony Wilson, PhD",
        subtitle = tagList(
          div("MOLTSA Lead Developer", style = "float: left;"),
          div("University of South Carolina", style = "float: right;"),
        ),
        image = "www/JAW.png",
        imageElevation = 3,
        backgroundImage = "www/background.svg",
        type = 2
      ),
      status = "primary",
      closable = FALSE,
      maximizable = FALSE,
      width = 12,
      p(tagList(HTML(
        "<a href='https://linkedin.com/in/jackanthonywilson' target='_blank'>
         <img src='www/linkedin.png' height='20px' style='vertical-align:middle;'>
       </a>
       <a href='https://orcid.org/0000-0001-8656-1463' target='_blank'>
         <img src='www/orcid_id.png' height='20px' style='vertical-align:middle;'>
       </a>
       <a href='https://www.researchgate.net/profile/Jack_Wilson41' target='_blank'>
         <img src='www/researchgate.png' height='20px' style='vertical-align:middle;'>
       </a>
       <a href='https://scholar.google.com/citations?user=1GsR3FkAAAAJ&hl=en' target='_blank'>
         <img src='www/google_scholar.png' height='20px' style='vertical-align:middle;'>
       </a>"
      )))
  ),
 # CREDIT for ROY EDWARD WILSON
                    bs4Dash::userBox(
                      label = ,
       collapsible = FALSE,
       boxToolSize = "xs",
       title = bs4Dash::userDescription(
         imageElevation = 3,
         title = "Roy Edward Wilson, CGMA Adv Dip MA, MSc",
         subtitle = tagList(
           div("Data Solutions Architect & Consultant", style = "float: left;"),
           #div("Medical Resource Partners", style = "float: right;")
         ),
         type = 2,
         image = "www/REW.jpg",
         backgroundImage = "www/particle.svg"
       ),
       status = "primary",
       width = 12,
       closable = FALSE,
       maximizable = FALSE,
       p(tagList(
         HTML(
           "<a href='https://www.linkedin.com/in/roywilsonuk/' target='_blank'>
         <img src='www/linkedin.png' height='20px' style='vertical-align:middle;'>
       </a>
       <a href='https://orcid.org/0000-0003-4902-7215' target='_blank'>
         <img src='www/orcid_id.png' height='20px' style='vertical-align:middle;'>
       </a>
       <a href='https://www.researchgate.net/profile/Roy-Wilson-6' target='_blank'>
         <img src='www/researchgate.png' height='20px' style='vertical-align:middle;'>
       </a>"
         )
       ))
),
# CREDIT 3
       bs4Dash::userBox(
         label = ,
         collapsible = FALSE,
         boxToolSize = "xs",
         title = bs4Dash::userDescription(
           imageElevation = 3,
           title = "Jorge Paz Soldan Palma, PhD",
           subtitle = tagList(
             div("Contributor to MOLTSA", style = "float: left;"),
             div("University of South Carolina", style = "float: right;"),
           ),
           type = 2,
           image = "www/JPSP.jpg",
           backgroundImage = "www/background_example_4.svg"
         ),
         status = "primary",
         width = 12,
         closable = FALSE,
         maximizable = FALSE,
         p(tagList(
           HTML(
             "
       <a href='https://orcid.org/0000-0002-9972-3692' target='_blank'>
         <img src='www/orcid_id.png' height='20px' style='vertical-align:middle;'>
       </a>
       <a href='https://www.researchgate.net/profile/Jorge-Paz-Soldan-Palma-2' target='_blank'>
         <img src='www/researchgate.png' height='20px' style='vertical-align:middle;'>
       </a>"
           )
         ))
       ),

# CREDIT 4
bs4Dash::userBox(
  label = ,
  collapsible = FALSE,
  boxToolSize = "xs",
  title = bs4Dash::userDescription(
    imageElevation = 3,
    title = "Lianna M. Eaton",
    subtitle = tagList(
      div("Contributor to MOLTSA", style = "float: left;"),
      div("University of South Carolina", style = "float: right;"),
    ),
    type = 2,
    image = "www/LME.jpeg",
    backgroundImage = "www/chemistry_header_Lianna.svg"
  ),
  status = "primary",
  width = 12,
  closable = FALSE,
  maximizable = FALSE,
  p(tagList(
    HTML(
      "<a href='https://www.linkedin.com/in/liannaeaton/' target='_blank'>
         <img src='www/linkedin.png' height='20px' style='vertical-align:middle;'>
       </a>"
    )
  ))
),

# CREDIT 5

#p(HTML("<u>Contributors</u>"))

bs4Dash::userBox(
  label = ,
  collapsible = FALSE,
  boxToolSize = "xs",
  title = bs4Dash::userDescription(
    imageElevation = 3,
    title = "Zachary K. Gardiner",
    subtitle = tagList(
      div("Contributor to MOLTSA", style = "float: left;"),
      div("University of South Carolina", style = "float: right;"),
    ),
    type = 2,
    image = "www/ZKG.jpg",
    backgroundImage = "www/particle.svg"
  ),
  status = "primary",
  width = 12,
  closable = FALSE,
  maximizable = FALSE,
  p(tagList(
    HTML(
      "<a href='https://www.linkedin.com/in/zachary-gardiner/' target='_blank'>
         <img src='www/linkedin.png' height='20px' style='vertical-align:middle;'>
       </a>"
    )
  ))
),


  )
}

#' credits Server Functions
#'
#' @noRd
mod_credits_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_credits_ui("credits_1")

## To be copied in the server
# mod_credits_server("credits_1")
