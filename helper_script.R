library(dplyr)
library(tidyverse)
library(tidyr)
library(tidytext)
library(ggplot2)
library(readr)

#loading the data
df_d <- read_csv("csv_pus/psam_pusd.csv", col_names = TRUE, progress = show_progress(), skip_empty_rows = TRUE)
df_d_mod <- df_d %>%
  select(REGION, ST, AGEP, COW, JWMNP, JWRIP, JWTR, MAR, SCH, SCHG, SCHL, SEX, WKHP, WKL, WKW, FOD1P, FOD2P, INDP, JWAP, JWDP, POVPIP)

#save the trimmed dataframe to work on
save(df_d_mod, file = "df_d.RData")

