## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(magrittr)
library(RPackageCoursera)
filename <- make_filename(2013)
filename

## ------------------------------------------------------------------------
fars_data <- fars_read(filename)
head(fars_data)

## ------------------------------------------------------------------------
fars_summarize_years(2013:2015)

## ------------------------------------------------------------------------
fars_map_state(45, 2015)

