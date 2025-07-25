# exportr
Enhanced data export functions with automatic provenance tracking.

## Installation

```r
# Install from GitHub
devtools::install_github("bravemaster3/exportr")

# Quick start
library(exportr)

# Instead of data.table::fwrite()
fwrite_with_metadata(my_data, "output.csv")

# Instead of write.csv()
write_csv_with_metadata(my_data, "output.csv")

# Instead of saveRDS()
saveRDS_with_metadata(my_object, "output.rds")
