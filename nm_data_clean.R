# new mexico 

library(tidyverse)
library(xlsx)
# read in precinct level returns

bin <- vector("list", length = 33)

for(i in 1:33){
  bin[[i]] <- read.xlsx("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_results_precinct.xlsx",sheetIndex = i)
}

df <- do.call(rbind, bin)

head(df)

colnames(df) <- tolower(colnames(df))
names <- c('county', 'precinct', 'johnson', 'castle', 'trump', 'clinton', 'stein', 'riva', 'mcmullin', 'rocky')
colnames(df) <- names

df$county <- sprintf("%03d",df$county) 

# now for the precinct
df$precinct <- as.character(df$precinct)

df$precinct <- gsub("PCT |PRECINCT |Precinct |PREC ","",df$precinct)
df$precinct <- gsub(" - Lordsburg/West| - Lordsburge/Central| - Lordsburg/East| - Virden| - Rodeo| - Animas","",df$precinct)
df$precinct <- sprintf("%03s",df$precinct) 

df <- df %>% mutate(county_prec = paste(county, precinct, sep = "_"))

# total votes and drop candidates
df <- df %>% mutate(total_votes = johnson + castle + trump + clinton + stein + riva + mcmullin + rocky, 
                    pct_trump = trump / total_votes, 
                    pct_clinton = clinton / total_votes) %>% 
  dplyr::select(county, precinct, county_prec, total_votes, pct_trump, pct_clinton, clinton, trump)


