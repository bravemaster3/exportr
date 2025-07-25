#' Intelligent script detection across environments
#'
#' This function attempts to detect the path of the R script that is currently
#' being executed using multiple methods across different environments.
#'
#' @return A list with script information including:
#' \itemize{
#'   \item script_name: Name of the script file
#'   \item script_path: Full path to the script file (PRIORITY 1)
#'   \item working_directory: Current working directory
#'   \item user: Username
#'   \item execution_time: Timestamp when function was called
#'   \item detection_method: Method used to detect the script
#'   \item r_version: R version string
#'   \item platform: Platform information
#' }
#'
#' @details
#' Detection methods tried in order:
#' \enumerate{
#'   \item RStudio API: \code{rstudioapi::getActiveDocumentContext()$path}
#'   \item Command line: Parse \code{commandArgs()} for \code{--file=} argument
#'   \item Call stack analysis: Search sys.frames() for source references
#'   \item Environment variables: Check for sourcing info
#' }
#'
#' @examples
#' \dontrun{
#' # Get current script information
#' script_info <- get_script_info()
#' print(script_info$script_path)
#' print(script_info$detection_method)
#' }
#'
#' @export
get_script_info <- function() {
  script_path <- NULL
  detection_method <- "unknown"

  # Method 1: RStudio API (most reliable when in RStudio)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    tryCatch({
      doc_context <- rstudioapi::getActiveDocumentContext()
      doc_path <- doc_context$path
      if (!is.null(doc_path) && doc_path != "" && doc_path != "Untitled") {
        script_path <- doc_path
        detection_method <- "rstudio_api"
      }
    }, error = function(e) {
      # RStudio API failed, continue to next method
      NULL
    })
  }

  # Method 2: Command line arguments (R CMD BATCH, Rscript)
  if (is.null(script_path)) {
    tryCatch({
      args <- commandArgs(trailingOnly = FALSE)

      # Look for --file= argument
      file_arg <- grep("--file=", args, value = TRUE)
      if (length(file_arg) > 0) {
        script_path <- sub("--file=", "", file_arg[1])
        detection_method <- "command_line"
      } else {
        # Alternative: look for script name in args
        # This catches cases like: Rscript myscript.R
        script_args <- grep("\\.R$|\\.r$", args, value = TRUE, ignore.case = TRUE)
        if (length(script_args) > 0) {
          script_path <- script_args[1]
          detection_method <- "command_line_script"
        }
      }
    }, error = function(e) {
      # Command line detection failed
      NULL
    })
  }

  # Method 3: Call stack analysis (works in some sourced scenarios)
  if (is.null(script_path)) {
    tryCatch({
      # Check sys.frames for source references
      for (i in 1:sys.nframe()) {
        frame_info <- sys.frame(i)

        # Check for RStudio-specific environment variables
        if (exists(".rs.getSourceFile", envir = frame_info)) {
          source_file <- get(".rs.getSourceFile", envir = frame_info)
          if (!is.null(source_file) && source_file != "") {
            script_path <- source_file
            detection_method <- "rstudio_frame"
            break
          }
        }

        # Check for source references in call stack
        src_ref <- attr(sys.call(i), "srcref")
        if (!is.null(src_ref)) {
          src_file <- attr(src_ref, "srcfile")
          if (!is.null(src_file) && !is.null(src_file$filename)) {
            script_path <- src_file$filename
            detection_method <- "source_reference"
            break
          }
        }
      }
    }, error = function(e) {
      # Call stack analysis failed
      NULL
    })
  }

  # Method 4: Check for sourced files in call stack
  if (is.null(script_path)) {
    tryCatch({
      calls <- sys.calls()
      for (i in seq_along(calls)) {
        call <- calls[[i]]
        if (length(call) > 1 && as.character(call[[1]]) %in% c("source", "sys.source")) {
          if (length(call) > 1) {
            # Extract file argument from source() call
            file_arg <- call[[2]]
            if (is.character(file_arg)) {
              script_path <- file_arg
              detection_method <- "source_call"
              break
            } else if (is.call(file_arg) || is.name(file_arg)) {
              # Try to evaluate the argument
              tryCatch({
                script_path <- as.character(file_arg)
                detection_method <- "source_call_eval"
                break
              }, error = function(e) NULL)
            }
          }
        }
      }
    }, error = function(e) {
      # Source call detection failed
      NULL
    })
  }

  # Method 5: Environment variables (last resort)
  if (is.null(script_path)) {
    tryCatch({
      # Check for common environment variables that might contain script info
      env_vars <- c("R_PROFILE_USER", "RSTUDIO_USER_IDENTITY")
      for (var in env_vars) {
        env_val <- Sys.getenv(var)
        if (env_val != "" && grepl("\\.[Rr]$", env_val)) {
          script_path <- env_val
          detection_method <- "environment_variable"
          break
        }
      }
    }, error = function(e) {
      # Environment variable detection failed
      NULL
    })
  }

  # Convert to absolute path if found and file exists
  if (!is.null(script_path) && script_path != "") {
    # Clean up the path
    script_path <- gsub("^\"|\"$", "", script_path)  # Remove quotes
    script_path <- gsub("^'|'$", "", script_path)    # Remove single quotes

    # Try to normalize path
    tryCatch({
      if (file.exists(script_path)) {
        script_path <- normalizePath(script_path, winslash = "/")
      } else {
        # Path might be relative, try from working directory
        full_path <- file.path(getwd(), script_path)
        if (file.exists(full_path)) {
          script_path <- normalizePath(full_path, winslash = "/")
        }
        # If still doesn't exist, keep original path but mark as unverified
      }
    }, error = function(e) {
      # Path normalization failed, keep original
      NULL
    })
  }

  # Return comprehensive script information
  return(list(
    script_name = if (!is.null(script_path)) basename(script_path) else NULL,
    script_path = script_path,
    working_directory = getwd(),
    user = Sys.info()[["user"]],
    execution_time = Sys.time(),
    detection_method = detection_method,
    r_version = R.version.string,
    platform = R.version$platform,
    packages_loaded = .packages(),
    file_exists = if (!is.null(script_path)) file.exists(script_path) else FALSE
  ))
}

#' Helper function to validate script detection
#'
#' @return TRUE if script detection is working, FALSE otherwise
#' @export
test_script_detection <- function() {
  script_info <- get_script_info()

  cat("=== Script Detection Test ===\n")
  cat("Detection method:", script_info$detection_method, "\n")
  cat("Script path:", script_info$script_path %||% "Not detected", "\n")
  cat("Script name:", script_info$script_name %||% "Not detected", "\n")
  cat("File exists:", script_info$file_exists, "\n")
  cat("Working directory:", script_info$working_directory, "\n")
  cat("User:", script_info$user, "\n")
  cat("R version:", script_info$r_version, "\n")

  # Return TRUE if we successfully detected a script path
  return(!is.null(script_info$script_path) && script_info$script_path != "")
}
