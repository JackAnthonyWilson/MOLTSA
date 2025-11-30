#' dsc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dsc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(column(4, bs4Dash::box(title = "Download templates", "", width=12, status = "primary", solidHeader = TRUE, # first box left
                                           shiny::selectInput(label = "Select template", inputId = ns("template"), choices = c("Data file", "Calibration file")),
                                           shiny::downloadButton(outputId = ns("template_download"), label = "Download template")),
                              bs4Dash::box(title = "Upload dataset", width = 12, status = "primary", solidHeader = TRUE, # second box left

                                        shiny::fileInput(inputId = ns("upload_dat"), accept = ".csv", label = "Upload your own data file (.csv)"),
                                        shiny::fileInput(inputId = ns("upload_cal_heat"), accept = ".csv", label = "Upload your own calibration file (.csv)")
                                        #shiny::fileInput(inputId = ns("upload_cal_cool"), accept = ".csv", label = "Upload your own cooling calibration file (.csv)"),
                                        #shinyWidgets::actionBttn(inputId = ns("plot"), label = "Plot calibration", style = "material-flat", color="primary", class = "green-button"),
                                        #shiny::downloadButton(outputId = ns("results_download"), label = "Download results")
                                        ),
                           bs4Dash::box(title = "Measured transitions plot", width = 12, status = "primary", solidHeader = TRUE,
                                        plotly::plotlyOutput(outputId = ns("cal_plot"))),
                           bs4Dash::box(title = "Calibration plot", width = 12, status = "primary", solidHeader = TRUE,
                                        plotly::plotlyOutput(outputId = ns("calibration_plot")))

    ), column(8, bs4Dash::box(width = 12, status = "primary", solidHeader = TRUE,title = "Instructions", HTML("

<h3>Process DSC Data Using the IUPAC Zero-Rate Method with Error Analysis</h3><br>
This page processes raw differential scanning calorimetry (DSC) data. It extracts peak temperatures measured at multiple heating and cooling rates, applies a calibration based on known melting point standards, and computes an error bar for each measured transition.<br><br>

<u><b>Templates</b></u><br>
Download templates for the data and calibration files using the panel on the left.<br><br>

<u><b>Input File Schema</b></u><br><br>

<i>Data file</i>:

<li><b>mol_frac_X</b>: Mole fraction of interest. This field is flexible and depends on how you wish to visualise the data. It is recommended to use the species that comes later alphabetically, in line with typical phase diagram conventions.</li> <li><b>rate</b>: Ramping rate for each set of measurements. Typical values include 16, 8, 4, and 1.6, or 20, 10, 5, and 2 (default units: °C min<sup>–1</sup>).</li> <li><b>P1, P2, P3...</b>: Peak temperatures (in °C). Add or remove peak columns as needed. If the same peak is detected at multiple rates, list all values in the same column.</li> <li><b>ramp_direction</b>: Temperature ramp direction. Accepts <code>heating</code> or <code>cooling</code>.</li> <li><b>keep</b>: Optional logical column (<code>TRUE</code>/<code>FALSE</code>). Measurements with <code>keep = FALSE</code> are excluded. Useful for omitting data requiring separate calibrations (e.g. measurements from a different instrument).</li><br>
<i>Calibration file</i>:<br>

<li><b>species</b>: Chemical formula of the calibrant (e.g. NaCl, KCl).</li> <li><b>R16, R8, R4, R1.6...</b>: Measured transition temperatures for each ramp rate (°C min<sup>–1</sup>).</li> <li><b>temp_c_temperature</b>: Literature melting point (in °C).</li> <li><b>error_in_literature</b>: Reported uncertainty in the melting point.</li> <li><b>literature_reference</b>: Citation or source of the literature value.</li><br><br>
<u><b>Testing with Default Datasets</b></u><br>

<li>If no data file is uploaded, a default dataset will be loaded for demonstration. If no calibration file is uploaded, the latest calibration file for the Netzsch Pegasus instrument will be applied.</li><br><br>
<u><b>Plots Explained</b></u><br>

<li><code>Measured transitions plot</code>: Shows corrected transition temperatures with error bars representing the total standard error.</li> <li><code>Calibration plot</code>: Shows the calibration model used to correct the raw data.</li> <li><code>Temperature calibration & error analysis</code>: A downloadable table of all processed data.</li><br><br>
<u><b>Temperature Calibration Explained</b></u><br>

<ul> <li>For each calibrant <i>i</i>, the deviation is defined as: \\( \\Delta T_i = T_i^{measured} - T_i^{literature} \\), where \\( T_i^{measured} \\) is the extrapolated zero-rate onset temperature, and \\( T_i^{literature} \\) is the known melting point.</li> <li>These values \\( \\Delta T_i \\) are linearly interpolated to define the calibration function, which estimates the temperature correction \\( T_{correction} \\) as a function of \\( T_{measured} \\).</li> </ul><br><br>
<u><b>Error Analysis Explained</b></u><br>
Error sources include:

<li>Linear extrapolation to zero rate for sample transitions (standard error of the Y-intercept).</li> <li>Linear extrapolation to zero rate for calibrant transitions (standard error of the Y-intercept).</li> <li>Uncertainty in calibrants’ literature melting points (reported standard error).</li><br>
These standard errors are combined as orthogonal vectors to compute the total standard error.<br><br>

The <code>CALPHAD_weighting</code> variable is proportional to the inverse of the total standard error and can be used to apply selective weighting in CALPHAD optimisations.<br><br>



"),

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

)),
bs4Dash::box(title = "Temperature calibration & error analysis", width = 12, status = "primary", solidHeader = TRUE, DT::DTOutput(outputId = ns("final_output_table")) |> shinycssloaders::withSpinner(type=3,color.background = "#2e8b57",color = "#2e8b57"))
  )
}

#' dsc Server Functions
#'
#' @noRd
mod_dsc_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

# Downloading templates
    output$template_download <- shiny::downloadHandler(
      filename = function(){
        paste0(input$template, ".csv")
      },
      content = function(e){
        tryCatch({
          if (input$template == "Select data type") {stop("Must select data type")}
          if (input$template == "Data file") {
            template = dplyr::tibble("mol_frac_X" = "",
                                     "rate" = "",
                                     "P1" ="",
                                     "P2" = "",
                                     "P3" = "",
                                     "ramp_direction" = "")

          }
          if (input$template == "Calibration file") {
            template = dplyr::tibble("species" = "",
                                     "R16" = "",
                                     "R8" ="",
                                     "R4" = "",
                                     "temp_c_temperature" = "",
                                     "error_in_literature" = "",
                                     "literature_reference" = "")

          }
          data.table::fwrite(template, e, na = "", row.names = FALSE)
        }, error = function(i){
          shinyWidgets::show_toast(title ="Error", text = i$message, width = 700,type = "error")
        }
        )
      }
    )


    fetch_dat_file <- reactive({
      if (is.null(input$upload_dat)) {
        dat_raw <- data.table::fread("./data/dsc_cal/example_dat_file.csv")
      } else {
        dat_raw <- data.table::fread(input$upload_dat$datapath)
      }
      dat_raw
    })

    # Fetch calibration file - use example if not uploaded
    fetch_cal_file_heat = reactive({
      if (is.null(input$upload_cal_heat)) {
        dat_h <- data.table::fread("./data/dsc_cal/unwrangled_calibration_data_heating.csv")
      } else {
        dat_h <- data.table::fread(input$upload_cal_heat$datapath)
      }
      dat_h
    })


    processed_data <- reactive({
      dat_raw <- fetch_dat_file()
      dat_raw <- data.table::as.data.table(dat_raw)

       if ("keep" %in% colnames(dat_raw)) {
         print("keep column is defined; filtering data")
         dat_raw <- dat_raw[dat_raw$keep, ]
       } else{
         print("Keep not defined; Keeping all")}

       dat_raw <- dat_raw  |> select(mol_frac_X, rate, ramp_direction, starts_with("P"))

       dat_tidy <- dat_raw |>
         tidyr::pivot_longer(
           cols = starts_with("P"),
           names_to = "peak",
           values_to = "temp_c"
         ) |>
         dplyr::filter(!is.na(temp_c))

       dat_heating <- dat_tidy |> dplyr::filter(ramp_direction == "heating")
       dat_cooling <- dat_tidy |> dplyr::filter(ramp_direction == "cooling")


              # Calibrations
       ### Heating calibration

       dat_h <- fetch_cal_file_heat()


       cal_heat <- dat_h |>
         tidyr::pivot_longer(
           cols = tidyselect::matches("^R"),
           names_to = "rate",
           values_to = "temp_c"
         ) |>
         dplyr::filter(!is.na(temp_c)) |>
         dplyr::mutate(
           rate = base::as.numeric(base::sub("^R", "", rate))
         ) |>

         # Group by species and fit model
         dplyr::group_by(species) |>
         dplyr::filter(dplyr::n() >= 2) |>
         dplyr::summarise(
           model = list(stats::lm(temp_c ~ rate)),
           .groups = "drop"
         ) |>

         dplyr::mutate(
           tidy_model = purrr::map(model, broom::tidy),
           Intercept = purrr::map_dbl(tidy_model, \(df) df |>
                                        dplyr::filter(term == "(Intercept)") |>
                                        dplyr::pull(estimate)),

           SE_Intercept = purrr::map_dbl(tidy_model, \(df) df |>
                                           dplyr::filter(term == "(Intercept)") |>
                                           dplyr::pull(std.error))
         ) |>

         # Join back literature metadata
         dplyr::left_join(
           dat_h |>
             dplyr::select(species, temp_c_literature, error_in_literature, literature_reference) |>
             dplyr::distinct(),
           by = "species"
         ) |>

         # Final column formatting
         dplyr::transmute(
           species,
           temp_c = Intercept,
           SE_Y_intercept = SE_Intercept,
           CI_literature = base::as.numeric(error_in_literature),
           temp_c_literature = base::as.numeric(temp_c_literature),
           delta_T = temp_c_literature - Intercept,
           literature_reference
         )
       cal_cool <- cal_heat

       # Processing lab data
       ### Heating data
       list_of_rates <- unique(dat_heating$rate)
       peak_list <- names(dat_raw)[grep("^P", names(dat_raw))]
       max_number_of_peaks <- length(peak_list)
       mol_fractions <- unique(dat_heating$mol_frac_X)

       measurement_data_wrangled_heating <- purrr::map_df(mol_fractions, function(v) {
         d1 <- dat_heating |>  filter(mol_frac_X == v)
         purrr::map_df(peak_list, function(v2) {
           d2 <- d1 |>  filter(peak == v2) |>  tidyr::drop_na()

           if (nrow(d2) > 0) {
             m1 <- lm(temp_c ~ rate, data = d2)
             coef_summary <- summary(m1)$coefficients
             data.frame(
               mol_frac_X = v,
               peak = v2,
               temp_c = coef_summary["(Intercept)", "Estimate"],
               SE_from_labdata_extrapolation = coef_summary["(Intercept)", "Std. Error"]
             )
           }
         })
       })
       measurement_data_wrangled_heating
       heating_data_ready <- lapply(seq(1,nrow(measurement_data_wrangled_heating),1), function(v){
         mdw_row <- measurement_data_wrangled_heating[v,]
         sorted_calibration <- cal_heat |>  dplyr::mutate(diff_sort = abs(temp_c_literature - mdw_row$temp_c))
         sorted_calibration
         cal3 <- sorted_calibration[order(sorted_calibration$diff_sort), ]
         # linear equation for the two nearest calibrants - Delta only
         m <- round((cal3$delta_T[2] - cal3$delta_T[1]) / (cal3$temp_c_literature[2] - cal3$temp_c_literature[1]),6)
         c <- cal3$delta_T[1] - m * cal3$temp_c_literature[1]
         fun1 <- function(r){
           return(m*r+c)}
         delta <- fun1(mdw_row$temp_c)
         # considering sum_CI_from_cal
         m_ci_lit <- (cal3$CI_literature[2] - cal3$CI_literature[1]) / (cal3$temp_c_literature[2] - cal3$temp_c_literature[1])
         c_ci_lit <- cal3$CI_literature[1] - cal3$temp_c_literature[1] * m_ci_lit
         CI_cal_lit <- mdw_row$temp_c*m_ci_lit + c_ci_lit

         m_ci <- (cal3$SE_Y_intercept[2] - cal3$SE_Y_intercept[1]) / (cal3$temp_c_literature[2] - cal3$temp_c_literature[1])
         c_ci <- cal3$SE_Y_intercept[1] - m_ci * cal3$temp_c_literature[1]
         CI_cal_zero_rate <- mdw_row$temp_c*m_ci + c_ci

         mdw_row |>
           dplyr::mutate(delta = delta,
                  SE_cal_zero_rate = CI_cal_zero_rate,
                  SE_cal_lit = CI_cal_lit
           )

       }) |>
         dplyr::bind_rows() %>%
         select(!peak)
       heating_data_ready

       ####
       # cooling data
       list_of_rates_c <- unique(dat_cooling$rate)
       peak_list_c <- names(dat_raw)[grep("^P", names(dat_raw))]
       max_number_of_peaks <- length(peak_list_c)
       mol_fractions <- unique(dat_cooling$mol_frac_X)

       measurement_data_wrangled_cooling <- purrr::map_df(mol_fractions, function(v) {
         d1 <- dat_cooling |>  filter(mol_frac_X == v)
         purrr::map_df(peak_list_c, function(v2) {
           d2 <- d1 |>  filter(peak == v2) %>% tidyr::drop_na()

           if (nrow(d2) > 0) {
             m1 <- lm(temp_c ~ rate, data = d2)
             coef_summary <- summary(m1)$coefficients
             data.frame(
               mol_frac_X = v,
               peak = v2,
               temp_c = coef_summary["(Intercept)", "Estimate"],
               SE_from_labdata_extrapolation = coef_summary["(Intercept)", "Std. Error"]
             )
           }
         })
       })
       measurement_data_wrangled_cooling


       cooling_data_ready <- lapply(seq(1,nrow(measurement_data_wrangled_cooling), 1), function(v){
         mdw_row <- measurement_data_wrangled_cooling[v,]
         sorted_calibration <- cal_cool |>  mutate(diff_sort = abs(temp_c_literature - mdw_row$temp_c))
         cal3 <- sorted_calibration[order(sorted_calibration$diff_sort), ]
         # linear equation for the two nearest calibrants - Delta only
         m <- round((cal3$delta_T[2] - cal3$delta_T[1]) / (cal3$temp_c_literature[2] - cal3$temp_c_literature[1]),6)
         c <- cal3$delta_T[1] - m * cal3$temp_c_literature[1]
         fun1 <- function(r){
           return(m*r+c)}
         delta <- fun1(mdw_row$temp_c)
         # considering sum_CI_from_cal
         m_ci_lit <- (cal3$CI_literature[2] - cal3$CI_literature[1]) / (cal3$temp_c_literature[2] - cal3$temp_c_literature[1])
         c_ci_lit <- cal3$CI_literature[1] - cal3$temp_c_literature[1] * m_ci_lit
         CI_cal_lit <- mdw_row$temp_c*m_ci_lit + c_ci_lit

         m_ci <- (cal3$SE_Y_intercept[2] - cal3$SE_Y_intercept[1]) / (cal3$temp_c_literature[2] - cal3$temp_c_literature[1])
         c_ci <- cal3$SE_Y_intercept[1] - m_ci * cal3$temp_c_literature[1]
         CI_cal_zero_rate <- mdw_row$temp_c*m_ci + c_ci
         mdw_row |>
           dplyr::mutate(delta = delta,
                  SE_cal_zero_rate = CI_cal_zero_rate,
                  SE_cal_lit = CI_cal_lit
           )

       }) |>
         dplyr::bind_rows() %>%
         select(!peak)
       cooling_data_ready
       ###

       data_output <- dplyr::bind_rows(cooling_data_ready |>  dplyr::mutate(ramp_direction = "cooling"),
                                heating_data_ready |>  dplyr::mutate(ramp_direction = "heating"))

       data_output_processed <- data_output |>
         rename(temp_c_raw = temp_c) |>
         mutate(temp_K_corrected = temp_c_raw + delta + 273.15,
                SE_total=sqrt(SE_from_labdata_extrapolation^2 + SE_cal_zero_rate^2 + SE_cal_lit^2),
                CALPHAD_weighting = 5/SE_total) |>
         select(temp_c_raw, SE_from_labdata_extrapolation, SE_cal_zero_rate, SE_cal_lit, delta, ramp_direction, mol_frac_X, temp_K_corrected, SE_total,CALPHAD_weighting)


      list(#dat_heating = dat_heating,
           #dat_cooling = dat_cooling,
           #dat_raw = dat_raw,
           #dat_tidy = dat_tidy,
           #dat_h = dat_h,
           cal_heat = cal_heat,
           #heating_data_ready = heating_data_ready,
           #cooling_data_ready = cooling_data_ready,
           data_output = data_output,
           data_output_processed = data_output_processed)
    })


    output$test_print <- renderPrint({
      processed_data()
    })





    output$cal_plot <-  plotly::renderPlotly({
      req(processed_data())
      data_frame_data_output <- processed_data()$data_output
      req(nrow(data_frame_data_output) > 0)
      p <- data_frame_data_output |>
        ggplot2::ggplot(ggplot2::aes(mol_frac_X, temp_c+delta+273.15, fill=ramp_direction, shape = ramp_direction)) + ggplot2::geom_point(size=3)+
        ggplot2::scale_fill_manual(values = c("dodgerblue", "red"))+
        ggplot2::scale_shape_manual(values = c(25, 24)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = temp_c+273.15+delta-(sqrt(SE_from_labdata_extrapolation^2 + SE_cal_zero_rate^2 + SE_cal_lit^2)),
                          ymax = temp_c+273.15+delta+(sqrt(SE_from_labdata_extrapolation^2 + SE_cal_zero_rate^2 + SE_cal_lit^2)), width=0.01)) +
        ggplot2::theme_bw(base_size = 15)+
        ggplot2::theme(axis.text = ggplot2::element_text(colour="black"),
              panel.border = ggplot2::element_rect(linewidth = 1),
              axis.ticks.length = ggplot2::unit(-0.15, "cm"),
              #panel.grid = ggplot2::element_blank(),
              legend.position = "none",
              legend.title = ggplot2::element_blank(),
              legend.background = ggplot2::element_rect(colour="black"), linewidth = 0.5) +
        ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~.,labels = NULL), n.breaks = 15) +
        ggplot2::scale_x_continuous(sec.axis = ggplot2::sec_axis(~.,labels = NULL), breaks = seq(0,1,0.1))+
        ggplot2::labs(x = "mole fraction X",
             y = "Temperature (K)")+
        ggplot2::coord_cartesian(xlim = c(0,1),
                                 ylim = c(400, 1500))
      p |> plotly::ggplotly() |>
        plotly::config(displayModeBar = TRUE, modeBarButtonsToRemove = c("zoom2d", "pan2d", "autoScale2d", "select2d", "lasso2d", "hoverCompareCartesian", "hoverClosestCartesian")) |>
        plotly::config(displaylogo = FALSE)

    })

    output$calibration_plot <- plotly::renderPlotly({
      req(processed_data())
      cal_heat <- processed_data()$cal_heat
      p_cal <- ggplot2::ggplot() +
        ggplot2::geom_line(data = cal_heat, ggplot2::aes(temp_c+273, delta_T), linewidth=2, color="red")+
        ggplot2::geom_errorbar(data = cal_heat, width=50, ggplot2::aes(x = temp_c+273,
                                                     ymax = delta_T + sqrt(cal_heat$SE_Y_intercept^2 + cal_heat$CI_literature^2),
                                                     ymin = delta_T - sqrt(cal_heat$SE_Y_intercept^2 + cal_heat$CI_literature^2)))+

        ggplot2::geom_point(data = cal_heat, ggplot2::aes(temp_c+273, delta_T), shape = 21, size=3, color="black", fill="white", stroke=0.5) +
        ggplot2::geom_text(data = cal_heat, ggplot2::aes(temp_c+350, delta_T+0.5, label = species))+
        ggplot2::theme_bw(base_size = 15) +
        ggplot2::coord_cartesian(ylim = c(-8,5),
                        xlim = c(400,1500))+
        ggplot2::theme(axis.text = ggplot2::element_text(colour="black"),
              panel.border = ggplot2::element_rect(colour = "black", linewidth = 1),
              axis.ticks.length = ggplot2::unit(-0.15, "cm"),
              plot.background = ggplot2::element_blank(),
              legend.title = ggplot2::element_blank(),
              legend.background = ggplot2::element_rect(colour="black"), linewidth = 0.5) +
        ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~.,labels = NULL, breaks = seq(-20,20, 1)), breaks = seq(-20,20, 1)) +
        ggplot2::scale_x_continuous(sec.axis = ggplot2::sec_axis(~.,labels = NULL), n.breaks = 6)+
        ggplot2::labs(
          x = "T[measured] (K)",
          y = "T[correction] (K)"
        )
      p_cal |> plotly::ggplotly()|>
        plotly::config(displayModeBar = TRUE, modeBarButtonsToRemove = c("zoom2d", "pan2d", "autoScale2d", "select2d", "lasso2d", "hoverCompareCartesian", "hoverClosestCartesian")) |>
        plotly::config(displaylogo = FALSE)

    })

    output$final_output_table <- DT::renderDataTable({
      data_output_processed <- processed_data()$data_output_processed
      req(data_output_processed)
      data_output_processed <- data_output_processed |>
        dplyr::mutate(dplyr::across(c(temp_c_raw, temp_K_corrected, delta, CALPHAD_weighting), ~ round(.x, 2))) |>
        dplyr::mutate(dplyr::across(c(SE_from_labdata_extrapolation, SE_cal_zero_rate, SE_cal_lit, mol_frac_X, SE_total), ~ round(.x, 3))) |>
        dplyr::rename_with(~ paste0(toupper(substr(.x, 1, 1)), substr(.x, 2, nchar(.x))))

      DT::datatable(data = as.data.frame(data_output_processed),
                    style = "bootstrap5",
                    extensions = 'Buttons',
                    options = list(
                      paging = TRUE,
                      pageLength = 100,
                      scrollX = TRUE,
                      scrollY = "400px",
                      scrollCollapse = TRUE,
                      buttons = c('copy', 'csv'),
                      dom = 'Bftip',
                      fixedHeader = list(top = TRUE),
                      responsive = TRUE,
                      columnDefs = list(list(
                        className = 'dt-center', targets = "_all"
                      ))
                    ),
                    selection = "multiple",
                    rownames = FALSE,
                    escape = FALSE
      )
    })
  })
}

## To be copied in the UI
# mod_dsc_ui("dsc_1")

## To be copied in the server
# mod_dsc_server("dsc_1")
