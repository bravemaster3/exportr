% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saveRDS_with_metadata.R
\name{saveRDS_with_metadata}
\alias{saveRDS_with_metadata}
\title{Enhanced saveRDS with automatic metadata tracking}
\usage{
saveRDS_with_metadata(
  object,
  file,
  ...,
  metadata_mode = "attribute",
  create_log = FALSE
)
}
\arguments{
\item{object}{R object to serialize}

\item{file}{Output file path}

\item{...}{Additional arguments passed to \code{base::saveRDS()}}

\item{metadata_mode}{Character. Method for storing metadata: "attribute" (default),
"sidecar", "log", or "none"}

\item{create_log}{Logical. Whether to create a log file (default FALSE)}
}
\value{
Invisibly returns NULL (same as \code{base::saveRDS()})
}
\description{
This function works exactly like \code{base::saveRDS()} but automatically
adds metadata about the source script to the exported file. The metadata is
stored in file system properties, extended attributes, or sidecar files
depending on the platform.
}
\details{
The function first saves the file using \code{base::saveRDS()}, then
attempts to add metadata including the full path to the source R script
as the primary piece of information.

Note: "comment" mode is not available for RDS files as they are binary.
}
\examples{
\dontrun{
# Basic usage - writes script path to file properties
model <- lm(mpg ~ wt + hp, data = mtcars)
saveRDS_with_metadata(model, "car_model.rds")

# Save other R objects
my_list <- list(data = iris, model = lm(Sepal.Length ~ Petal.Length, data = iris))
saveRDS_with_metadata(my_list, "analysis_results.rds")

# Alternative metadata modes
saveRDS_with_metadata(mtcars, "cars_data.rds", metadata_mode = "sidecar")
saveRDS_with_metadata(fitted_model, "model.rds", metadata_mode = "log")
}

}
\seealso{
\code{\link{fwrite_with_metadata}}, \code{\link{write_csv_with_metadata}}
}
