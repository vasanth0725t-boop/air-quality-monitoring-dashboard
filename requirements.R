required_packages <- c("shiny", "bslib", "ggplot2", "dplyr", "plotly", "DT")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

still_missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(still_missing) > 0) {
  stop("Unable to install required packages: ", paste(still_missing, collapse = ", "))
}

message("All required packages are installed.")
