if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, data.table,
  readxl, readr, ggcorrplot, cowplot,
  RColorBrewer, scales, nortest, xlsx,
  skimr,xtable
  )

windowsFonts(Arial=windowsFont("sans"))

options(scipen=999)