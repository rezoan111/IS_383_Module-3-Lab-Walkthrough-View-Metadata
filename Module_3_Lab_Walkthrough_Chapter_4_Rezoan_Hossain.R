# Chapter 4 - Module 3 Lab Walkthrough
# Problem 2 (Combine Monthly Production Reports)
# Problem 4 (Unstack Marketing Research Data)
#
# Name: Rezoan Ishteaque Hossain

# install only once (if needed)
# install.packages(c("tidyverse", "writexl"))

library(tidyverse)
library(writexl)

# -----------------------------
# Problem 2
# -----------------------------

# check working directory + files
getwd()
list.files()

# load the 3 monthly production files
det <- readr::read_csv("detprod.csv")
alb <- readr::read_csv("albprod.csv")
pro <- readr::read_csv("proprod.csv")

# add facility column
det <- det %>% dplyr::mutate(Facility = "Detroit")
alb <- alb %>% dplyr::mutate(Facility = "Albany")
pro <- pro %>% dplyr::mutate(Facility = "Providence")

# combine all rows
janprod <- dplyr::bind_rows(det, alb, pro)

# quick checks (for evidence)
dplyr::glimpse(janprod)
head(janprod)

# save as Excel
writexl::write_xlsx(janprod, "janprod.xlsx")


# -----------------------------
# Problem 4
# -----------------------------

# load stacked data
br <- readr::read_csv("brughastacked_r.csv")

# clean Sales (removes $ and makes it numeric)
br <- br %>% dplyr::mutate(Sales = readr::parse_number(Sales))

# unstack (wide format)
brougha_unstacked <- br %>%
  tidyr::pivot_wider(
    names_from = Period,
    values_from = Sales
  )

# quick checks (for evidence)
dplyr::glimpse(brougha_unstacked)
head(brougha_unstacked)

# save as Excel
writexl::write_xlsx(brougha_unstacked, "broughaunstacked.xlsx")
