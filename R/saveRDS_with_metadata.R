#' Enhanced saveRDS with automatic metadata tracking
#'
#' This function works exactly like \code{base::saveRDS()} but automatically
#' adds metadata about the source script to the exported file. The metadata is
#' stored in file system properties, extended attributes, or sidecar files
#' depending on the platform.
#'
#' @param object R object to serialize
#' @param file Output file path
#' @param ... Additional arguments passed to \code{base::saveRDS()}
#' @param metadata_mode Character. Method for storing metadata: "attribute" (default),
#'   "sidecar", "log", or "none"
#' @param create_log Logical. Whether to create a log file (default FALSE)
#'
#' @details
#' The function first saves the file using \code{base::saveRDS()}, then
#' attempts to add metadata including the full path to the source R script
#' as the primary piece of information.
#'
#' Note: "comment" mode is not available for RDS files as they are binary.
#'
#' @return Invisibly returns NULL (same as \code{base::saveRDS()})
#'
#' @examples
#' \dontrun{
#' # Basic usage - writes script path to file properties
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' saveRDS_with_metadata(model, "car_model.rds")
#'
#' # Save other R objects
#' my_list <- list(data = iris, model = lm(Sepal.Length ~ Petal.Length, data = iris))
#' saveRDS_with_metadata(my_list, "analysis_results.rds")
#'
#' # Alternative metadata modes
#' saveRDS_with_metadata(mtcars, "cars_data.rds", metadata_mode = "sidecar")
#' saveRDS_with_metadata(fitted_model, "model.rds", metadata_mode = "log")
#' }
#'
#' @seealso \code{\link{fwrite_with_metadata}}, \code{\link{write_csv_with_metadata}}
#'
#' @export
saveRDS_with_metadata <- function(object, file, ...,
                                  metadata_mode = "attribute",
                                  create_log = FALSE) {

  # STEP 1: Save the file first using original saveRDS
  # This ensures the file contents are exactly what saveRDS() would produce
  result <- saveRDS(object, file, ...)

  # STEP 2: Add metadata if requested (not "none" mode)
  if (metadata_mode != "none") {
    cat("RDS file saved. Adding metadata to:", file, "\n")

    # Get script information (PRIORITY 1: script path)
    script_info <- get_script_info()

    # Create comprehensive metadata for RDS objects
    metadata <- create_metadata(object, file, script_info)

    # Write metadata using specified mode (no "comment" for RDS files)
    if (metadata_mode == "comment") {
      warning("Comment mode not available for RDS files. Using sidecar mode instead.")
      metadata_mode <- "sidecar"
    }

    success <- switch(metadata_mode,
                      "attribute" = write_file_attributes(metadata, file),
                      "sidecar" = write_sidecar_metadata(metadata, file),
                      "log" = append_to_log(metadata, paste0(tools::file_path_sans_ext(file), "_log.csv")),
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
