### ======================================================
### 00_Main.R
### Environment and path initialisation
### ======================================================

## CRAN packages: conditional installation then loading
install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) {
    install.packages(missing, dependencies = TRUE)
  }
  invisible(lapply(pkgs, require, character.only = TRUE))
}

cran_pkgs <- c(
  "tidyverse", "readxl", "writexl", "broom", "fixest",
  "zoo", "lmtest", "sandwich", "scales", "data.table",
  "modelsummary", "tinytable", "remotes", "gridExtra",
  "here", "sf", "stringi", "stringr","patchwork", "janitor", "glue", "pheatmap", "terra", "ncdf4", "raster", "sp", "parallel", "arrow"
)

install_if_missing(cran_pkgs)

## Define project root
wd <- here::here()

## Correction if here() starts inside /Code
if (basename(wd) == "Code") {
  wd <- dirname(wd)
}


## Main directories
wd_code   <- file.path(wd, "Code")
wd_sortie <- file.path(wd, "Sortie")

## Output sub-directories — one folder per script
## 01_Prob_ff
wd_sortie01 <- file.path(wd_sortie, "01_prob_ff")
wd_sortie03 <- file.path(wd_sortie, "01_prob_ff", "probabilites")
wd_sortie04 <- file.path(wd_sortie, "01_prob_ff", "redistribution")

## 02_Calibrated_costs
wd_sortie05 <- file.path(wd_sortie, "02_calibrated_costs", "couts")
wd_sortie08 <- file.path(wd_sortie, "02_calibrated_costs", "historique")
wd_sortie09 <- file.path(wd_sortie, "02_calibrated_costs", "calibration")

## Input data directories
wd_data <- file.path(wd, "Data")

## Directory for projected inventories (parquet BAU/EE) — stored locally outside OneDrive
## Update this path if the files are stored elsewhere (external drive, desktop, etc.)
wd_inventaires <- "INSERT ASSET DATA HERE"  # path to directory containing projected inventory parquet files

## Create directories if absent
dir.create(wd_sortie01, recursive = TRUE, showWarnings = FALSE)
dir.create(wd_sortie03, recursive = TRUE, showWarnings = FALSE)
dir.create(wd_sortie04, recursive = TRUE, showWarnings = FALSE)
dir.create(wd_sortie05, recursive = TRUE, showWarnings = FALSE)
dir.create(wd_sortie08, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(wd_sortie09, "graphiques"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(wd_sortie09, "tableaux"),   recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(wd_sortie, "03_stats"),     recursive = TRUE, showWarnings = FALSE)


## Minimal display for verification
message("Project root: ", wd)
message("Input data directory: ", wd_data)
message("Output directory: ", wd_sortie)


## Run pipeline
source(file.path(wd_code, "01_Prob_ff.R"))
source(file.path(wd_code, "02_Calibrated_costs.R"))
source(file.path(wd_code, "03_Stats.R"))

