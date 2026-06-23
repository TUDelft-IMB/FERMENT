source("renv/activate.R")

# Disable .RData auto-save and remove stale .RData if it exists
# only runs in interactive sessions i.e. not when sourcing scripts
if (interactive()) {
  # Remove existing .RData file if present
  rdata_file <- file.path(getwd(), ".RData")
  if (file.exists(rdata_file)) {
    file.remove(rdata_file)
    cat("Removed stale .RData file\n")
  }
  # Disable automatic .RData saving going forward
  options(save.image = FALSE)
  # check for TT_EXCEL_DIR availability from .Renviron
  if (Sys.getenv("TT_EXCEL_DIR") == "") {
    cat("TT_EXCEL_DIR is NOT set in .Renviron", "\n")
  } else {
    cat("TT_EXCEL_DIR set to:", Sys.getenv("TT_EXCEL_DIR"), "\n")
    # Double-check in case .Renviron doesn't load
    Sys.getenv("TT_EXCEL_DIR")
  }
}
