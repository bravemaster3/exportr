#' Utility functions for exportr package
#'
#' This file contains utility functions for checking metadata presence,
#' reading metadata, configuring package options, and cross-platform compatibility

#' Check if file has exportr metadata
#'
#' @param file Path to the file to check
#' @return TRUE if file has exportr metadata, FALSE otherwise
#' @export
has_exportr_metadata <- function(file) {

  if (!file.exists(file)) {
    warning("File does not exist: ", file)
    return(FALSE)
  }

  # Check for sidecar file first (most reliable)
  sidecar_path <- file.path(dirname(file),
                            paste0(".", basename(file), ".exportr"))

  if (file.exists(sidecar_path)) {
    return(TRUE)
  }

  # Check platform-specific metadata
  if (.Platform$OS.type == "windows") {
    return(has_windows_metadata(file))
  } else if (Sys.info()["sysname"] == "Darwin") {
    return(has_macos_metadata(file))
  } else {
    return(has_linux_metadata(file))
  }
}

#' Check for Windows alternate data streams
#' @param file File path
#' @return TRUE if metadata found, FALSE otherwise
has_windows_metadata <- function(file) {
  tryCatch({
    # Check for main script path stream
    ads_file <- paste0(file, ":exportr.script_path")
    return(file.exists(ads_file))
  }, error = function(e) {
    return(FALSE)
  })
}

#' Check for macOS extended attributes
#' @param file File path
#' @return TRUE if metadata found, FALSE otherwise
has_macos_metadata <- function(file) {
  tryCatch({
    cmd <- sprintf('xattr -l "%s"', file)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    return(length(result) > 0 && any(grepl("com.exportr", result)))
  }, error = function(e) {
    return(FALSE)
  })
}

#' Check for Linux extended attributes
#' @param file File path
#' @return TRUE if metadata found, FALSE otherwise
has_linux_metadata <- function(file) {
  tryCatch({
    cmd <- sprintf('getfattr -d "%s" 2>/dev/null', file)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    return(length(result) > 0 && any(grepl("user.exportr", result)))
  }, error = function(e) {
    return(FALSE)
  })
}

#' Read exportr metadata from file
#'
#' This function attempts to read metadata from various storage locations
#' and returns a comprehensive metadata object.
#'
#' @param file_path Path to the file to read metadata from
#' @return List containing metadata, or empty list if no metadata found
#' @export
read_exportr_metadata <- function(file_path) {

  if (!file.exists(file_path)) {
    warning("File does not exist: ", file_path)
    return(list())
  }

  metadata <- list()

  # Try sidecar file first (most comprehensive)
  sidecar_path <- file.path(dirname(file_path),
                            paste0(".", basename(file_path), ".exportr"))

  if (file.exists(sidecar_path)) {
    metadata <- read_sidecar_metadata(sidecar_path)
  }

  # If no sidecar, try platform-specific metadata
  if (length(metadata) == 0) {
    if (.Platform$OS.type == "windows") {
      metadata <- read_windows_metadata(file_path)
    } else if (Sys.info()["sysname"] == "Darwin") {
      metadata <- read_macos_metadata(file_path)
    } else {
      metadata <- read_linux_metadata(file_path)
    }
  }

  return(metadata)
}

#' Read sidecar metadata file
#' @param sidecar_path Path to sidecar file
#' @return Metadata list
read_sidecar_metadata <- function(sidecar_path) {
  tryCatch({
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      # Try to read as JSON first
      content <- readLines(sidecar_path)
      if (any(grepl("^\\s*[{\\[]", content))) {
        return(jsonlite::fromJSON(sidecar_path))
      }
    }

    # Read as text format
    lines <- readLines(sidecar_path)
    metadata <- list(sidecar_content = lines)

    # Try to parse key information from text
    script_line <- grep("Script path:", lines, value = TRUE)
    if (length(script_line) > 0) {
      metadata$script_path <- sub("Script path:\\s*", "", script_line[1])
    }

    return(metadata)
  }, error = function(e) {
    return(list())
  })
}

#' Read Windows alternate data streams
#' @param file_path File path
#' @return Metadata list
read_windows_metadata <- function(file_path) {
  metadata <- list()

  tryCatch({
    # Read script path
    ads_script <- paste0(file_path, ":exportr.script_path")
    if (file.exists(ads_script)) {
      metadata$script_path <- readLines(ads_script)[1]
    }

    # Read other metadata
    ads_time <- paste0(file_path, ":exportr.created_time")
    if (file.exists(ads_time)) {
      metadata$created_time <- readLines(ads_time)[1]
    }

    ads_user <- paste0(file_path, ":exportr.user")
    if (file.exists(ads_user)) {
      metadata$user <- readLines(ads_user)[1]
    }

    ads_method <- paste0(file_path, ":exportr.detection_method")
    if (file.exists(ads_method)) {
      metadata$detection_method <- readLines(ads_method)[1]
    }

    ads_dims <- paste0(file_path, ":exportr.dimensions")
    if (file.exists(ads_dims)) {
      metadata$dimensions <- readLines(ads_dims)[1]
    }

  }, error = function(e) {
    # Failed to read Windows metadata
  })

  return(metadata)
}

#' Read macOS extended attributes
#' @param file_path File path
#' @return Metadata list
read_macos_metadata <- function(file_path) {
  metadata <- list()

  tryCatch({
    # Read script path
    cmd <- sprintf('xattr -p com.exportr.script_path "%s" 2>/dev/null', file_path)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) > 0 && !grepl("No such xattr", result[1])) {
      metadata$script_path <- result[1]
    }

    # Read other metadata
    cmd <- sprintf('xattr -p com.exportr.created_time "%s" 2>/dev/null', file_path)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) > 0) {
      metadata$created_time <- result[1]
    }

    cmd <- sprintf('xattr -p com.exportr.user "%s" 2>/dev/null', file_path)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) > 0) {
      metadata$user <- result[1]
    }

    cmd <- sprintf('xattr -p com.exportr.detection_method "%s" 2>/dev/null', file_path)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) > 0) {
      metadata$detection_method <- result[1]
    }

    cmd <- sprintf('xattr -p com.exportr.dimensions "%s" 2>/dev/null', file_path)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) > 0) {
      metadata$dimensions <- result[1]
    }

  }, error = function(e) {
    # Failed to read macOS metadata
  })

  return(metadata)
}

#' Read Linux extended attributes
#' @param file_path File path
#' @return Metadata list
read_linux_metadata <- function(file_path) {
  metadata <- list()

  tryCatch({
    # Read script path
    cmd <- sprintf('getfattr --only-values -n user.exportr.script_path "%s" 2>/dev/null', file_path)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) > 0) {
      metadata$script_path <- result[1]
    }

    # Read other metadata
    cmd <- sprintf('getfattr --only-values -n user.exportr.created_time "%s" 2>/dev/null', file_path)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) > 0) {
      metadata$created_time <- result[1]
    }

    cmd <- sprintf('getfattr --only-values -n user.exportr.user "%s" 2>/dev/null', file_path)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) > 0) {
      metadata$user <- result[1]
    }

    cmd <- sprintf('getfattr --only-values -n user.exportr.detection_method "%s" 2>/dev/null', file_path)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) > 0) {
      metadata$detection_method <- result[1]
    }

    cmd <- sprintf('getfattr --only-values -n user.exportr.dimensions "%s" 2>/dev/null', file_path)
    result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) > 0) {
      metadata$dimensions <- result[1]
    }

  }, error = function(e) {
    # Failed to read Linux metadata
  })

  return(metadata)
}

#' Verify metadata for a file (similar to photo verification)
#'
#' This function displays metadata information in a user-friendly format,
#' similar to the verify_metadata function in the photo script.
#'
#' @param file_path Path to the file to verify
#' @return Invisibly returns the metadata list
#' @export
verify_file_metadata <- function(file_path) {

  if (!file.exists(file_path)) {
    cat("File not found:", file_path, "\n")
    return(invisible(NULL))
  }

  cat("Metadata for", basename(file_path), ":\n")

  metadata <- read_exportr_metadata(file_path)

  if (length(metadata) > 0) {
    # Script information (PRIORITY 1)
    if (!is.null(metadata$script_path)) {
      cat("  Script path:", metadata$script_path, "\n")
    } else if (!is.null(metadata$source_info$script_path)) {
      cat("  Script path:", metadata$source_info$script_path, "\n")
    }

    # Creation info
    if (!is.null(metadata$created_time)) {
      cat("  Created:", metadata$created_time, "\n")
    } else if (!is.null(metadata$source_info$execution_time)) {
      cat("  Created:", metadata$source_info$execution_time, "\n")
    }

    # User info
    if (!is.null(metadata$user)) {
      cat("  User:", metadata$user, "\n")
    } else if (!is.null(metadata$source_info$user)) {
      cat("  User:", metadata$source_info$user, "\n")
    }

    # Detection method
    if (!is.null(metadata$detection_method)) {
      cat("  Detection method:", metadata$detection_method, "\n")
    } else if (!is.null(metadata$source_info$detection_method)) {
      cat("  Detection method:", metadata$source_info$detection_method, "\n")
    }

    # Data dimensions
    if (!is.null(metadata$dimensions)) {
      cat("  Data dimensions:", metadata$dimensions, "\n")
    } else if (!is.null(metadata$data_info)) {
      if (!is.null(metadata$data_info$nrows)) {
        cat("  Data dimensions:", metadata$data_info$nrows, "rows x", metadata$data_info$ncols, "columns\n")
      }
    }

    # File size
    if (!is.null(metadata$file_info$file_size_bytes)) {
      cat("  File size:", metadata$file_info$file_size_bytes, "bytes\n")
    }

    # Platform info
    if (!is.null(metadata$r_session$platform)) {
      cat("  Platform:", metadata$r_session$platform, "\n")
    }

  } else {
    cat("  No exportr metadata found\n")
  }

  cat("\n")
  return(invisible(metadata))
}

#' Package configuration functions
#'
#' Set and get package-wide options for exportr

# Global package options
.exportr_options <- new.env()

#' Set exportr package options
#'
#' @param default_metadata_mode Default metadata mode for all functions
#' @param verbose Whether to print verbose output
#' @param fallback_to_sidecar Whether to fallback to sidecar files if attributes fail
#' @export
set_exportr_options <- function(default_metadata_mode = "attribute",
                                verbose = TRUE,
                                fallback_to_sidecar = TRUE) {

  .exportr_options$default_metadata_mode <- default_metadata_mode
  .exportr_options$verbose <- verbose
  .exportr_options$fallback_to_sidecar <- fallback_to_sidecar

  invisible(NULL)
}

#' Get exportr package options
#'
#' @param option_name Name of option to get, or NULL for all options
#' @return Option value or list of all options
#' @export
get_exportr_options <- function(option_name = NULL) {

  # Set defaults if not already set
  if (!exists("default_metadata_mode", envir = .exportr_options)) {
    set_exportr_options()
  }

  if (is.null(option_name)) {
    return(as.list(.exportr_options))
  } else {
    return(.exportr_options[[option_name]])
  }
}

#' Helper function to check if command exists on system
#'
#' @param cmd Command to check
#' @return TRUE if command exists, FALSE otherwise
command_exists <- function(cmd) {
  tryCatch({
    result <- system(paste("which", cmd), intern = TRUE, ignore.stderr = TRUE)
    return(length(result) > 0 && result != "")
  }, error = function(e) {
    return(FALSE)
  })
}

#' Check system capabilities for metadata storage
#'
#' @return List of capabilities by platform
#' @export
check_system_capabilities <- function() {

  capabilities <- list(
    platform = .Platform$OS.type,
    os_name = Sys.info()["sysname"]
  )

  if (.Platform$OS.type == "windows") {
    capabilities$alternate_data_streams <- TRUE  # Assume available on Windows
    capabilities$recommended_mode <- "attribute"
  } else if (Sys.info()["sysname"] == "Darwin") {
    capabilities$xattr_available <- command_exists("xattr")
    capabilities$recommended_mode <- if (capabilities$xattr_available) "attribute" else "sidecar"
  } else {
    capabilities$setfattr_available <- command_exists("setfattr")
    capabilities$getfattr_available <- command_exists("getfattr")
    capabilities$recommended_mode <- if (capabilities$setfattr_available && capabilities$getfattr_available) "attribute" else "sidecar"
  }

  return(capabilities)
}

#' Display system information and recommendations
#'
#' @export
system_info <- function() {

  cat("=== exportr System Information ===\n")

  caps <- check_system_capabilities()

  cat("Platform:", caps$platform, "\n")
  cat("OS:", caps$os_name, "\n")
  cat("Recommended metadata mode:", caps$recommended_mode, "\n")

  if (caps$platform == "windows") {
    cat("Alternate data streams: Available\n")
  } else if (caps$os_name == "Darwin") {
    cat("xattr command:", if (caps$xattr_available) "Available" else "Not found", "\n")
    if (!caps$xattr_available) {
      cat("Note: Extended attributes not available. Will use sidecar files.\n")
    }
  } else {
    cat("setfattr command:", if (caps$setfattr_available) "Available" else "Not found", "\n")
    cat("getfattr command:", if (caps$getfattr_available) "Available" else "Not found", "\n")
    if (!caps$setfattr_available || !caps$getfattr_available) {
      cat("Note: Extended attributes not fully available. Will use sidecar files.\n")
    }
  }

  # Test script detection
  cat("\n=== Script Detection Test ===\n")
  script_info <- get_script_info()
  cat("Detection method:", script_info$detection_method, "\n")
  cat("Script path detected:", !is.null(script_info$script_path) && script_info$script_path != "", "\n")

  invisible(caps)
}
