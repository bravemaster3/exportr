#' Enhanced fwrite with automatic metadata tracking
#'
#' This function works exactly like \code{data.table::fwrite()} but automatically
#' adds metadata about the source script to the exported file. The metadata is
#' stored in file system properties, extended attributes, or sidecar files
#' depending on the platform.
#'
#' @param x A data.frame or data.table to write
#' @param file Output file path
#' @param ... Additional arguments passed to \code{data.table::fwrite()}
#' @param metadata_mode Character. Method for storing metadata: "attribute" (default),
#'   "sidecar", "log", "comment", or "none"
#' @param create_log Logical. Whether to create a log file (default FALSE)
#'
#' @details
#' The function first saves the file using \code{data.table::fwrite()}, then
#' attempts to add metadata including:
#' \itemize{
#'   \item Full path to the source R script (PRIORITY 1)
#'   \item Timestamp of file creation
#'   \item User information
#'   \item Data dimensions and characteristics
#' }
#'
#' Metadata modes in order of preference:
#' \itemize{
#'   \item "attribute": Write to file system properties/extended attributes
#'   \item "sidecar": Create hidden companion file (.filename.csv.exportr)
#'   \item "log": Create/append to log file (filename_log.csv)
#'   \item "comment": Add metadata as comments in CSV file
#'   \item "none": No metadata tracking
#' }
#'
#' @return Same return value as \code{data.table::fwrite()}
#'
#' @examples
#' \dontrun{
#' # Basic usage - writes script path to file properties
#' library(data.table)
#' dt <- data.table(x = 1:10, y = letters[1:10])
#' fwrite_with_metadata(dt, "output.csv")
#'
#' # Use with built-in datasets
#' fwrite_with_metadata(iris, "iris.csv")
#' fwrite_with_metadata(mtcars, "cars.csv")
#'
#' # Alternative metadata modes
#' fwrite_with_metadata(trees, "trees.csv", metadata_mode = "sidecar")
#' fwrite_with_metadata(airquality, "air.csv", metadata_mode = "log")
#' fwrite_with_metadata(ChickWeight, "chicks.csv", metadata_mode = "none")
#' }
#'
#' @seealso \code{\link{write_csv_with_metadata}}, \code{\link{saveRDS_with_metadata}},
#'   \code{\link{verify_file_metadata}}
#'
#' @export
fwrite_with_metadata <- function(x, file, ...,
                                 metadata_mode = "attribute",
                                 create_log = FALSE) {

  # Check if data.table is available
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package is required for fwrite_with_metadata")
  }

  # STEP 1: Save the file first using original fwrite
  # This ensures the file contents are exactly what fwrite() would produce
  result <- data.table::fwrite(x, file, ...)

  # STEP 2: Add metadata if requested (not "none" mode)
  if (metadata_mode != "none") {
    cat("File saved. Adding metadata to:", file, "\n")

    # Get script information (PRIORITY 1: script path)
    script_info <- get_script_info()

    # Create comprehensive metadata
    metadata <- create_metadata(x, file, script_info)

    # Write metadata using specified mode
    success <- switch(metadata_mode,
                      "attribute" = write_file_attributes(metadata, file),
                      "sidecar" = write_sidecar_metadata(metadata, file),
                      "log" = append_to_log(metadata, paste0(tools::file_path_sans_ext(file), "_log.csv")),
                      "comment" = write_comment_metadata(metadata, file),
                      {
                        warning("Unknown metadata_mode: ", metadata_mode, ". Using sidecar fallback.")
                        write_sidecar_metadata(metadata, file)
                      }
    )

    # Report results
    if (success) {
      cat("✓ Metadata added successfully\n")
      if (!is.null(script_info$script_path)) {
        cat("  Script path recorded:", script_info$script_path, "\n")
      }
      cat("  Metadata mode:", metadata_mode, "\n")
    } else {
      cat("✗ Primary metadata method failed, trying fallback...\n")
      # Fallback to sidecar if primary method fails
      fallback_success <- write_sidecar_metadata(metadata, file)
      if (fallback_success) {
        cat("✓ Fallback sidecar metadata written\n")
      } else {
        cat("✗ All metadata methods failed\n")
      }
    }

    # Create log if requested
    if (create_log) {
      log_file <- paste0(tools::file_path_sans_ext(file), "_log.csv")
      append_to_log(metadata, log_file)
    }
  }

  return(result)
}

#' Write metadata as comments to CSV file (discouraged method)
#' @param metadata Metadata object
#' @param file File path
#' @return TRUE if successful, FALSE otherwise
write_comment_metadata <- function(metadata, file) {
  tryCatch({
    # Read existing file
    lines <- readLines(file)

    # Create comment lines
    comments <- c(
      paste("# File created by:", metadata$source_info$script_path %||% "Unknown script"),
      paste("# Created on:", metadata$file_info$created_date),
      paste("# User:", metadata$source_info$user),
      paste("# Data dimensions:", metadata$data_info$nrows, "rows x", metadata$data_info$ncols, "cols"),
      ""  # Empty line before data
    )

    # Prepend comments to file
    new_lines <- c(comments, lines)
    writeLines(new_lines, file)

    return(TRUE)
  }, error = function(e) {
    warning("Failed to write comment metadata: ", e$message)
    return(FALSE)
  })
}
