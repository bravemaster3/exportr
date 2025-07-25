#' Enhanced write.csv with automatic metadata tracking
#'
#' This function works exactly like \code{utils::write.csv()} but automatically
#' adds metadata about the source script to the exported file. The metadata is
#' stored in file system properties, extended attributes, or sidecar files
#' depending on the platform.
#'
#' @param x A data.frame to write
#' @param file Output file path
#' @param ... Additional arguments passed to \code{utils::write.csv()}
#' @param metadata_mode Character. Method for storing metadata: "attribute" (default),
#'   "sidecar", "log", "comment", or "none"
#' @param create_log Logical. Whether to create a log file (default FALSE)
#'
#' @details
#' The function first saves the file using \code{utils::write.csv()}, then
#' attempts to add metadata including the full path to the source R script
#' as the primary piece of information.
#'
#' @return Invisibly returns NULL (same as \code{utils::write.csv()})
#'
#' @examples
#' \dontrun{
#' # Basic usage - writes script path to file properties
#' write_csv_with_metadata(iris, "iris.csv")
#' write_csv_with_metadata(mtcars, "cars.csv")
#'
#' # Alternative metadata modes
#' write_csv_with_metadata(trees, "trees.csv", metadata_mode = "sidecar")
#' write_csv_with_metadata(airquality, "air.csv", metadata_mode = "log")
#' }
#'
#' @seealso \code{\link{fwrite_with_metadata}}, \code{\link{saveRDS_with_metadata}}
#'
#' @export
write_csv_with_metadata <- function(x, file, ...,
                                    metadata_mode = "attribute",
                                    create_log = FALSE) {

  # STEP 1: Save the file first using original write.csv
  # This ensures the file contents are exactly what write.csv() would produce
  result <- utils::write.csv(x, file, ...)

  # STEP 2: Add metadata if requested (not "none" mode)
  if (metadata_mode != "none") {
    cat("CSV file saved. Adding metadata to:", file, "\n")

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
