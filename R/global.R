
# This is a safe space for global functions
convert_column_names <- function(df) {
  colnames(df) <- colnames(df) |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()
  return(df)
}

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
