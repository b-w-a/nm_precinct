# new mexico data analysis 
# Bryan Wilcox-Archuleta
# Jan. 5, 2016

# header -----
# this script makes the plots and EI analysis for NM. 
# questions contact bwa@ucla.edu

# libraries -----
library(tidyverse)
library(ei)
library(eiCompare)

# 2016 data -----

data <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2016_precinct_with_pct_latino.csv")


# EI using EI compare 

df <- data %>% dplyr::select(pct_trump, pct_clinton, total_votes, pct_latino) %>% na.omit()


nm_2016 <- df %>%  mutate(pct_other = 1 - (pct_trump + pct_clinton),pct_nonlatino = 1-pct_latino)

head(nm_2016)

cands <- c("pct_clinton", "pct_trump", "pct_other")
groups <- c("~ pct_latino", "~ pct_nonlatino") 
table_names <- c("EI: Pct Latino", "EI: Pct Non Latino")

results_nm_2016 <- ei_est_gen(cands, groups,
                              "total", data = nm_2016, 
                              table_names = table_names)

