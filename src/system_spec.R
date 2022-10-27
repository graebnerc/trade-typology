# All packages used in at least one R file:
library(devtools)
library(knitr)
library(countrycode)
library(data.table)
library(tidyverse)
library(factoextra)
library(cluster)
library(MacroDataR)
library(icaeDesign)
library(tidyverse)
library(data.table)
library(icaeDesign)
library(countrycode)
library(data.table)
library(tidyverse)
library(WDI)


my_session_info <- devtools::session_info()
package_frame <- data.frame(
  package = my_session_info$packages$package,
  loaded_version = my_session_info$packages$loadedversion,
  attached = my_session_info$packages$date,
  source = my_session_info$packages$source
)
writeLines(text = {
  paste(sep = "\n", collapse = "",
        paste0(
          rep("-", 40), collapse = ""),
        paste(paste0(rep("-", 11), collapse = ""),
              "R environment",
              paste0(
                rep("-", 14), collapse = "")
              ),
        paste0(
          rep("-", 40), collapse = ""),
        paste(
          knitr::kable(
            data.frame(setting = as.character(my_session_info$platform)
                       )
            ), collapse = "\n"),
        paste0(rep("-", 40), collapse = ""),
        paste(
          paste0(rep("-", 15), collapse = ""),
              "packages",
              paste0(rep("-", 15), collapse = "")),
        paste0(rep("-", 58), collapse = ""),
        paste(knitr::kable(
          package_frame
          ), collapse = "\n")
  )
}, con = "session_info.txt")
