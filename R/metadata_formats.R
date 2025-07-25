#' Metadata creation and writing functions
#'
#' This file contains functions for creating comprehensive metadata objects
#' and writing them to various storage formats (file attributes, sidecar files, logs)

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create comprehensive metadata object
#'
#' @param x Data object being exported
#' @param file File path where data is saved
#' @param script_info Script information from get_script_info()
#' @return List containing comprehensive metadata
#' @export
create_metadata <- function(x, file, script_info) {

  # File information
  file_info <- list(
    output_file = normalizePath(file, winslash = "/", mustWork = FALSE),
    created_date = Sys.time(),
    file_size_bytes = if (file.exists(file)) file.info(file)$size else NA
  )

  # Data information (depends on object type)
  if (is.data.frame(x) || is.matrix(x)) {
    data_info <- list(
      nrows = nrow(x),
      ncols = ncol(x),
      column_names = if (is.data.frame(x)) names(x) else colnames(x),
      data_classes = if (is.data.frame(x)) sapply(x, class) else class(x),
      object_size_bytes = as.numeric(object.size(x))
    )
  } else {
    # For other R objects (lists, models, etc.)
    data_info <- list(
      object_class = class(x),
      object_type = typeof(x),
      object_length = length(x),
      object_size_bytes = as.numeric(object.size(x))
    )
  }

  # R session information
  r_session <- list(
    r_version = R.version.string,
    platform = R.version$platform,
    packages_loaded = .packages(),
    locale = Sys.getlocale()
  )

  # Combine all metadata
  metadata <- list(
    exportr_version = "0.1.0",
    file_info = file_info,
    source_info = script_info,
    data_info = data_info,
    r_session = r_session
  )

  return(metadata)
}

#' Write metadata to file system properties/extended attributes
#'
#' @param metadata Metadata object from create_metadata()
#' @param file File path
#' @return TRUE if successful, FALSE otherwise
#' @export
write_file_attributes <- function(metadata, file) {

  if (!file.exists(file)) {
    warning("File does not exist: ", file)
    return(FALSE)
  }

  success <- FALSE

  # Try platform-specific methods
  if (.Platform$OS.type == "windows") {
    success <- write_windows_attributes(metadata, file)
  } else if (Sys.info()["sysname"] == "Darwin") {
    success <- write_macos_attributes(metadata, file)
  } else {
    success <- write_linux_attributes(metadata, file)
  }

  return(success)
}

#' Write metadata using Windows alternate data streams
#' @param metadata Metadata object
#' @param file File path
#' @return TRUE if successful, FALSE otherwise
write_windows_attributes <- function(metadata, file) {

  tryCatch({
    # PRIORITY 1: Write script path to alternate data stream
    if (!is.null(metadata$source_info$script_path)) {
      ads_script <- paste0(file, ":exportr.script_path")
      writeLines(metadata$source_info$script_path, ads_script)
    }

    # Write other key metadata
    if (!is.null(metadata$source_info$execution_time)) {
      ads_time <- paste0(file, ":exportr.created_time")
      writeLines(as.character(metadata$source_info$execution_time), ads_time)
    }

    if (!is.null(metadata$source_info$user)) {
      ads_user <- paste0(file, ":exportr.user")
      writeLines(metadata$source_info$user, ads_user)
    }

    if (!is.null(metadata$source_info$detection_method)) {
      ads_method <- paste0(file, ":exportr.detection_method")
      writeLines(metadata$source_info$detection_method, ads_method)
    }

    # Write data dimensions if available
    if (!is.null(metadata$data_info$nrows)) {
      ads_dims <- paste0(file, ":exportr.dimensions")
      writeLines(paste(metadata$data_info$nrows, "x", metadata$data_info$ncols), ads_dims)
    }

    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Write metadata using macOS extended attributes
#' @param metadata Metadata object
#' @param file File path
#' @return TRUE if successful, FALSE otherwise
write_macos_attributes <- function(metadata, file) {

  tryCatch({
    # PRIORITY 1: Write script path using xattr
    if (!is.null(metadata$source_info$script_path)) {
      cmd <- sprintf('xattr -w com.exportr.script_path "%s" "%s"',
                     metadata$source_info$script_path, file)
      result <- system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      if (result == 0) {
        # Add other metadata if script path was successful
        if (!is.null(metadata$source_info$execution_time)) {
          cmd <- sprintf('xattr -w com.exportr.created_time "%s" "%s"',
                         metadata$source_info$execution_time, file)
          system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
        }

        if (!is.null(metadata$source_info$user)) {
          cmd <- sprintf('xattr -w com.exportr.user "%s" "%s"',
                         metadata$source_info$user, file)
          system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
        }

        if (!is.null(metadata$source_info$detection_method)) {
          cmd <- sprintf('xattr -w com.exportr.detection_method "%s" "%s"',
                         metadata$source_info$detection_method, file)
          system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
        }

        # Write data dimensions if available
        if (!is.null(metadata$data_info$nrows)) {
          dims <- paste(metadata$data_info$nrows, "x", metadata$data_info$ncols)
          cmd <- sprintf('xattr -w com.exportr.dimensions "%s" "%s"', dims, file)
          system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
        }

        return(TRUE)
      }
    }
    return(FALSE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Write metadata using Linux extended attributes
#' @param metadata Metadata object
#' @param file File path
#' @return TRUE if successful, FALSE otherwise
write_linux_attributes <- function(metadata, file) {

  tryCatch({
    # PRIORITY 1: Write script path using setfattr
    if (!is.null(metadata$source_info$script_path)) {
      cmd <- sprintf('setfattr -n user.exportr.script_path -v "%s" "%s"',
                     metadata$source_info$script_path, file)
      result <- system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      if (result == 0) {
        # Add other metadata if script path was successful
        if (!is.null(metadata$source_info$execution_time)) {
          cmd <- sprintf('setfattr -n user.exportr.created_time -v "%s" "%s"',
                         metadata$source_info$execution_time, file)
          system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
        }

        if (!is.null(metadata$source_info$user)) {
          cmd <- sprintf('setfattr -n user.exportr.user -v "%s" "%s"',
                         metadata$source_info$user, file)
          system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
        }

        if (!is.null(metadata$source_info$detection_method)) {
          cmd <- sprintf('setfattr -n user.exportr.detection_method -v "%s" "%s"',
                         metadata$source_info$detection_method, file)
          system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
        }

        # Write data dimensions if available
        if (!is.null(metadata$data_info$nrows)) {
          dims <- paste(metadata$data_info$nrows, "x", metadata$data_info$ncols)
          cmd <- sprintf('setfattr -n user.exportr.dimensions -v "%s" "%s"', dims, file)
          system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
        }

        return(TRUE)
      }
    }
    return(FALSE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Write metadata to hidden sidecar file
#'
#' @param metadata Metadata object from create_metadata()
#' @param file File path
#' @return TRUE if successful, FALSE otherwise
#' @export
write_sidecar_metadata <- function(metadata, file) {

  tryCatch({
    # Create hidden sidecar file path
    sidecar_path <- paste0(".", basename(file), ".exportr")
    sidecar_dir <- dirname(file)
    sidecar_full_path <- file.path(sidecar_dir, sidecar_path)

    # Write as JSON if jsonlite is available, otherwise as readable text
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      json_content <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE)
      writeLines(json_content, sidecar_full_path)
    } else {
      # Fallback to human-readable text format
      text_content <- c(
        "=== exportr Metadata ===",
        paste("Created:", Sys.time()),
        "",
        "=== Source Information ===",
        paste("Script path:", metadata$source_info$script_path %||% "Unknown"),
        paste("Script name:", metadata$source_info$script_name %||% "Unknown"),
        paste("Detection method:", metadata$source_info$detection_method),
        paste("User:", metadata$source_info$user),
        paste("Working directory:", metadata$source_info$working_directory),
        "",
        "=== File Information ===",
        paste("Output file:", metadata$file_info$output_file),
        paste("File size:", metadata$file_info$file_size_bytes, "bytes"),
        "",
        "=== Data Information ===",
        if (!is.null(metadata$data_info$nrows)) {
          c(paste("Rows:", metadata$data_info$nrows),
            paste("Columns:", metadata$data_info$ncols),
            paste("Column names:", paste(metadata$data_info$column_names, collapse = ", ")))
        } else {
          c(paste("Object class:", paste(metadata$data_info$object_class, collapse = ", ")),
            paste("Object type:", metadata$data_info$object_type),
            paste("Object length:", metadata$data_info$object_length))
        },
        paste("Object size:", metadata$data_info$object_size_bytes, "bytes"),
        "",
        "=== R Session ===",
        paste("R version:", metadata$r_session$r_version),
        paste("Platform:", metadata$r_session$platform),
        paste("Packages loaded:", paste(metadata$r_session$packages_loaded, collapse = ", "))
      )

      writeLines(text_content, sidecar_full_path)
    }

    return(TRUE)
  }, error = function(e) {
    warning("Failed to write sidecar metadata: ", e$message)
    return(FALSE)
  })
}

#' Append metadata to log file
#'
#' @param metadata Metadata object from create_metadata()
#' @param log_file Path to log file
#' @return TRUE if successful, FALSE otherwise
#' @export
append_to_log <- function(metadata, log_file) {

  tryCatch({
    # Create log entry as a single row
    log_entry <- data.frame(
      timestamp = as.character(metadata$file_info$created_date),
      output_file = metadata$file_info$output_file,
      script_path = metadata$source_info$script_path %||% "Unknown",
      script_name = metadata$source_info$script_name %||% "Unknown",
      detection_method = metadata$source_info$detection_method,
      user = metadata$source_info$user,
      working_directory = metadata$source_info$working_directory,
      file_size_bytes = metadata$file_info$file_size_bytes %||% NA,
      nrows = metadata$data_info$nrows %||% NA,
      ncols = metadata$data_info$ncols %||% NA,
      object_size_bytes = metadata$data_info$object_size_bytes,
      r_version = metadata$r_session$r_version,
      platform = metadata$r_session$platform,
      stringsAsFactors = FALSE
    )

    # Write header if file doesn't exist
    if (!file.exists(log_file)) {
      utils::write.csv(log_entry, log_file, row.names = FALSE)
    } else {
      # Append to existing file
      utils::write.table(log_entry, log_file, sep = ",",
                         append = TRUE, row.names = FALSE,
                         col.names = FALSE)
    }

    return(TRUE)
  }, error = function(e) {
    warning("Failed to write to log file: ", e$message)
    return(FALSE)
  })
}

#' Write metadata to separate file
#'
#' @param metadata Metadata object from create_metadata()
#' @param file Original file path
#' @param format Format for metadata file ("json", "yaml", "txt")
#' @return TRUE if successful, FALSE otherwise
#' @export
write_metadata_file <- function(metadata, file, format = "json") {

  # Create metadata file path
  base_name <- tools::file_path_sans_ext(file)
  metadata_file <- switch(format,
                          "json" = paste0(base_name, "_metadata.json"),
                          "yaml" = paste0(base_name, "_metadata.yaml"),
                          "txt" = paste0(base_name, "_metadata.txt"),
                          paste0(base_name, "_metadata.txt")  # default
  )

  tryCatch({
    if (format == "json" && requireNamespace("jsonlite", quietly = TRUE)) {
      # Write as JSON
      json_content <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE)
      writeLines(json_content, metadata_file)
    } else if (format == "yaml" && requireNamespace("yaml", quietly = TRUE)) {
      # Write as YAML
      yaml_content <- yaml::as.yaml(metadata)
      writeLines(yaml_content, metadata_file)
    } else {
      # Write as human-readable text
      text_content <- c(
        "exportr Metadata File",
        paste("Generated:", Sys.time()),
        paste("Original file:", file),
        "",
        "=== SOURCE SCRIPT INFORMATION ===",
        paste("Script path:", metadata$source_info$script_path %||% "Not detected"),
        paste("Script name:", metadata$source_info$script_name %||% "Not detected"),
        paste("Detection method:", metadata$source_info$detection_method),
        paste("Execution time:", metadata$source_info$execution_time),
        paste("User:", metadata$source_info$user),
        paste("Working directory:", metadata$source_info$working_directory),
        "",
        "=== FILE INFORMATION ===",
        paste("Output file:", metadata$file_info$output_file),
        paste("Created:", metadata$file_info$created_date),
        paste("File size:", metadata$file_info$file_size_bytes, "bytes"),
        "",
        "=== DATA INFORMATION ===",
        if (!is.null(metadata$data_info$nrows)) {
          c(paste("Data dimensions:", metadata$data_info$nrows, "rows x", metadata$data_info$ncols, "columns"),
            paste("Column names:", paste(metadata$data_info$column_names, collapse = ", ")),
            paste("Column classes:", paste(metadata$data_info$data_classes, collapse = ", ")))
        } else {
          c(paste("Object class:", paste(metadata$data_info$object_class, collapse = ", ")),
            paste("Object type:", metadata$data_info$object_type),
            paste("Object length:", metadata$data_info$object_length))
        },
        paste("Object size:", metadata$data_info$object_size_bytes, "bytes"),
        "",
        "=== R SESSION INFORMATION ===",
        paste("R version:", metadata$r_session$r_version),
        paste("Platform:", metadata$r_session$platform),
        paste("Locale:", metadata$r_session$locale),
        paste("Packages loaded:", paste(metadata$r_session$packages_loaded, collapse = ", "))
      )

      writeLines(text_content, metadata_file)
    }

    cat("Metadata file created:", metadata_file, "\n")
    return(TRUE)
  }, error = function(e) {
    warning("Failed to write metadata file: ", e$message)
    return(FALSE)
  })
}
