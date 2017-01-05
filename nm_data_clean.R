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

write_csv(df, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2016_vote_returns.csv")

setwd("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/nm")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read_csv)

dem <- do.call(rbind, myfiles)

dem$county <- sprintf("%03s",dem$county) 
dem$precinct <- sprintf("%03s",dem$precinct) 
dem <- dem %>% mutate(county_prec = paste(county, precinct, sep = "_"))
head(dem)

# NAs to zero 
dem[is.na(dem)] <- 0

# percent latino 
dem <- dem %>% mutate(total_reg = as.numeric(asian) + black + white + latino + as.numeric(native) + other, 
                      pct_latino = latino / total_reg) %>% dplyr::select(county_prec, pct_latino)

# merge the data ----

data <- inner_join(df, dem, by = "county_prec")

# write the csv 

write_csv(data,"/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2016_precinct_with_pct_latino.csv")

# now the demographcis
# 1	Bernalillo County
# 3	Catron County
# 5	Chaves County
# 6	Cibola County
# 7	Colfax County
# 9	Curry County
# 11	De Baca County
# 13	Dona Ana County
# 15	Eddy County
# 17	Grant County
# 19	Guadalupe County
# 21	Harding County
# 23	Hidalgo County
# 25	Lea County
# 27	Lincoln County
# 28	Los Alamos County
# 29	Luna County
# 31	McKinley County
# 33	Mora County
# 35	Otero County
# 37	Quay County
# 39	Rio Arriba County
# 41	Roosevelt County
# 43	Sandoval County
# 45	San Juan County
# 47	San Miguel County
# 49	Santa Fe County
# 51	Sierra County
# 53	Socorro County
# 55	Taos County
# 57	Torrance County
# 59	Union County
# 61	Valencia County



