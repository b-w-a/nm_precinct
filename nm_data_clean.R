# new mexico data clean 
# Bryan Wilcox-Archuleta
# Jan. 5, 2016

# header -----
# this script cleans and organizes the data for nm. It produces two data files. 2016 returns and 2012 with 2016 returns. 
# Both files contain the percent latino which is the percent of latino registered votes in the precinct. 

library(tidyverse)
library(xlsx)

# functions 

dataCleanR <- function(data, county_code){
  colnames(data) <- c('county', 'precinct', 'office', 'candidate', 'total_votes')
  
  data <- data %>% dplyr::select(precinct, candidate, total_votes)
  drop <- which(is.na(data$total_votes))
  
  data <- data[-drop,]
  data$row <- 1:nrow(data)
  
  data$candidate <- as.factor(data$candidate)
  
  long <- spread(data, candidate, total_votes) %>% arrange(row)
  colnames(long) <- c('precinct', 'row', 'obama', 'johnson', 'stein', 'romney', 'rocky', 'goode')
  
  bin <- vector('list', length = length(unique(long$precinct)))
  
  precinct <- unique(long$precinct) %>% na.omit()
  
  for(i in 1:length(precinct)){
    bin[[i]] <- rep(unique(precinct)[i], each = 6)
  }
  
  res <- do.call(c, bin)
  
  final <- data.frame(prec = res, long)
  final[is.na(final)] <- 0
  
  foo <- final %>% group_by(prec) %>% summarise(
    obama = sum(obama), 
    romney = sum(romney), 
    total_votes = sum(obama, romney, stein, johnson, goode, rocky)) %>% dplyr::select(prec, obama, romney, total_votes)
  
  foo$county <- as.character(county_code)
  return(foo)
}

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

foo <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2016_precinct_with_pct_latino.csv")


foo %>% filter(county == "003")
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

# 2012 data -----
# 1	Bernalillo County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/bern.csv")
county <- dataCleanR(county, '001')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_bern_final.csv")

# 3	Catron County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_catron_raw.csv")
county <- dataCleanR(county, '003')
county$prec <- gsub("A|B|C","",county$prec)
county$prec <- sprintf("%03s",county$prec)

county <- county %>% group_by(prec) %>% summarise(
  obama_votes = sum(obama), 
  romney_votes = sum(romney), 
  total = sum(total_votes)) %>% dplyr::select(prec, obama = obama_votes, romney = romney_votes, total_votes = total) %>% 
  mutate(county = "003")

head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_catron_final.csv")

# 5	Chaves County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_chaves_raw.csv")
county <- dataCleanR(county, '005')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_chaves_final.csv")

# 6	Cibola County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_cibola_raw.csv")
county <- dataCleanR(county, '006')
county$prec <- gsub("A|B|C","",county$prec)
county$prec <- sprintf("%03s",county$prec)

county <- county %>% group_by(prec) %>% summarise(
  obama_votes = sum(obama), 
  romney_votes = sum(romney), 
  total = sum(total_votes)) %>% dplyr::select(prec, obama = obama_votes, romney = romney_votes, total_votes = total) %>% 
  mutate(county = "006")

head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_cibola_final.csv")

# 7	Colfax County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_colfax_raw.csv")
county <- dataCleanR(county, '007')
county$prec <- gsub("A|B|C","",county$prec)
county$prec <- sprintf("%03s",county$prec)

county <- county %>% group_by(prec) %>% summarise(
  obama_votes = sum(obama), 
  romney_votes = sum(romney), 
  total = sum(total_votes)) %>% dplyr::select(prec, obama = obama_votes, romney = romney_votes, total_votes = total) %>% 
  mutate(county = "007")

head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_colfax_final.csv")

# 9	Curry County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_curry_raw.csv")
county <- dataCleanR(county, '009')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_curry_final.csv")

# 11	De Baca County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_de_baca_raw.csv")
county <- dataCleanR(county, '011')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_de_baca_final.csv")

# 13	Dona Ana County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_dona_ana_raw.csv")
county <- dataCleanR(county, '013')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_dona_ana_final.csv")

# 15	Eddy County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_eddy_raw.csv")
county <- dataCleanR(county, '015')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_eddy_final.csv")

# 17	Grant County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_grant_raw.csv")
county <- dataCleanR(county, '017')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_grant_final.csv")

# 19	Guadalupe County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_guadalupe_raw.csv")
county <- dataCleanR(county, '019')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_guadalupe_final.csv")

# 21	Harding County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_harding_raw.csv")
county <- dataCleanR(county, '021')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_harding_final.csv")

# 23	Hidalgo County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_hidalgo_raw.csv")
county <- dataCleanR(county, '023')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_hidalgo_final.csv")

# 25	Lea County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_lea_raw.csv")
county <- dataCleanR(county, '025')
county$prec <- gsub("A|B|C","",county$prec)
county$prec <- sprintf("%03s",county$prec)

county <- county %>% group_by(prec) %>% summarise(
  obama_votes = sum(obama), 
  romney_votes = sum(romney), 
  total = sum(total_votes)) %>% dplyr::select(prec, obama = obama_votes, romney = romney_votes, total_votes = total) %>% 
  mutate(county = "025")

head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_lea_final.csv")

# 27	Lincoln County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_lincoln_raw.csv")
county <- dataCleanR(county, '027')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_lincoln_final.csv")

# 28	Los Alamos County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_los_alamos_raw.csv")
county <- dataCleanR(county, '028')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_los_alamos_final.csv")

# 29	Luna County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_luna_raw.csv")
county <- dataCleanR(county, '029')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_luna_final.csv")

# 31	McKinley County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_mckinley_raw.csv")
county <- dataCleanR(county, '031')
county$prec <- gsub("A|B|C","",county$prec)
county$prec <- sprintf("%03s",county$prec)

county <- county %>% group_by(prec) %>% summarise(
  obama_votes = sum(obama), 
  romney_votes = sum(romney), 
  total = sum(total_votes)) %>% dplyr::select(prec, obama = obama_votes, romney = romney_votes, total_votes = total) %>% 
  mutate(county = "031")

head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_mckinley_final.csv")

# 33	Mora County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_mora_raw.csv")
county <- dataCleanR(county, '033')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_mora_final.csv")

# 35	Otero County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_otero_raw.csv")
county <- dataCleanR(county, '035')
county$prec <- gsub("A|B|C","",county$prec)
county$prec <- sprintf("%03s",county$prec)

county <- county %>% group_by(prec) %>% summarise(
  obama_votes = sum(obama), 
  romney_votes = sum(romney), 
  total = sum(total_votes)) %>% dplyr::select(prec, obama = obama_votes, romney = romney_votes, total_votes = total) %>% 
  mutate(county = "035")
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_otero_final.csv")

# 37	Quay County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_quay_raw.csv")
county <- dataCleanR(county, '037')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_quay_final.csv")

# 39	Rio Arriba County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_rio_arriba_raw.csv")
county <- dataCleanR(county, '039')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_rio_arriba_final.csv")

# 41	Roosevelt County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_roosevelt_raw.csv")
county <- dataCleanR(county, '041')
county$prec <- gsub("A|B|C","",county$prec)
county$prec <- sprintf("%03s",county$prec)

county <- county %>% group_by(prec) %>% summarise(
  obama_votes = sum(obama), 
  romney_votes = sum(romney), 
  total = sum(total_votes)) %>% dplyr::select(prec, obama = obama_votes, romney = romney_votes, total_votes = total) %>% 
  mutate(county = "041")
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_roosevelt_final.csv")

# 43	Sandoval County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_sandoval_raw.csv")
county <- dataCleanR(county, '043')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_sandoval_final.csv")

# 45	San Juan County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_san_juan_raw.csv")
county <- dataCleanR(county, '045')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_san_juan_final.csv")

# 47	San Miguel County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_san_miguel_raw.csv")
county <- dataCleanR(county, '047')
county$prec <- gsub("A|B|C","",county$prec)
county$prec <- sprintf("%03s",county$prec)

county <- county %>% group_by(prec) %>% summarise(
  obama_votes = sum(obama), 
  romney_votes = sum(romney), 
  total = sum(total_votes)) %>% dplyr::select(prec, obama = obama_votes, romney = romney_votes, total_votes = total) %>% 
  mutate(county = "047")
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_san_miguel_final.csv")

# 49	Santa Fe County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_santa_fe_raw.csv")
county <- dataCleanR(county, '049')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_santa_fe_final.csv")

# 51	Sierra County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_sierra_raw.csv")
county <- dataCleanR(county, '051')
county$prec <- gsub("A|B|C","",county$prec)
county$prec <- sprintf("%03s",county$prec)

county <- county %>% group_by(prec) %>% summarise(
  obama_votes = sum(obama), 
  romney_votes = sum(romney), 
  total = sum(total_votes)) %>% dplyr::select(prec, obama = obama_votes, romney = romney_votes, total_votes = total) %>% 
  mutate(county = "051")
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_sierra_final.csv")

# 53	Socorro County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_socorro_raw.csv")
county <- dataCleanR(county, '053')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_socorro_final.csv")

# 55	Taos County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_taos_raw.csv")
county <- dataCleanR(county, '055')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_taos_final.csv")

# 57	Torrance County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_torrance_raw.csv")
county <- dataCleanR(county, '057')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_torrance_final.csv")

# 59	Union County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_union_raw.csv")
county <- dataCleanR(county, '059')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_union_final.csv")

# 61	Valencia County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_valencia_raw.csv")
county <- dataCleanR(county, '061')
head(county)
write_csv(county, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_valencia_final.csv")


# read and combine all 

bin <- vector("list", length = 33)
setwd("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012")
temp = list.files(pattern="*final.csv")
myfiles = lapply(temp, read_csv)

res_2012 <- do.call(rbind, myfiles)
head(res_2012)

res_2012$prec <- sprintf("%03s",res_2012$prec) 



res_2012 <- res_2012 %>% mutate(county_prec = paste(county, prec, sep = "_"),
                                pct_obama = obama / total_votes, 
                                pct_romney = romney / total_votes) %>% 
  dplyr::select(county_prec, pct_obama, pct_romney, obama, romney)

head(res_2012)

write_csv(res_2012, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012_vote_returns_final.csv")








bern <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/bern.csv")

head(bern)
colnames(bern) <- c('county', 'precinct', 'office', 'candidate', 'total_votes')

bern <- bern[,c('precinct','candidate', 'total_votes')]
drop <- which(is.na(bern$total_votes))

bern <- bern[-drop,]
bern$row <- 1:nrow(bern)
str(bern)

length(unique(bern$candidate))

bern$candidate <- as.factor(bern$candidate)

test <- spread(bern, candidate, total_votes) %>% arrange(row)
colnames(test) <- c('precinct', 'row', 'obama', 'johnson', 'stein', 'romney', 'rocky', 'goode')
head(test)

unique(test$precinct)

precinct <- rep(1:603, each = 6)

test$precinct
i <- 48

bin <- vector('list', length = length(unique(test$precinct)))

precinct <- unique(test$precinct) %>% na.omit()

for(i in 1:length(precinct)){
  bin[[i]] <- rep(unique(precinct)[i], each = 6)
}

res <- do.call(c, bin)

final <- data.frame(prec = res, test)
head(final)
final[is.na(final)] <- 0

foo <- final %>% group_by(prec) %>% summarise(
  obama = sum(obama), 
  romney = sum(romney), 
  total_votes = sum(obama, romney, stein, johnson, goode, rocky)) %>% dplyr::select(prec, obama, romney, total_votes)

head(foo)

write_csv(foo, "/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_bern_final.csv")

# 3	Catron County
county <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2012/2012_catron_raw.csv")
head(data)


colnames(county) <- c('county', 'precinct', 'office', 'candidate', 'total_votes')

county <- county[,c('precinct','candidate', 'total_votes')]
drop <- which(is.na(county$total_votes))

county <- county[-drop,]
county$row <- 1:nrow(county)
head(county)

length(unique(county$candidate))

county$candidate <- as.factor(county$candidate)

county_long <- spread(county, candidate, total_votes) %>% arrange(row)
colnames(county_long) <- c('precinct', 'row', 'obama', 'johnson', 'stein', 'romney', 'rocky', 'goode')
head(county_long)

bin <- vector('list', length = length(unique(county_long$precinct)))

precinct <- unique(county_long$precinct) %>% na.omit()

for(i in 1:length(precinct)){
  bin[[i]] <- rep(unique(precinct)[i], each = 6)
}

res <- do.call(c, bin)

final <- data.frame(prec = res, county_long)
head(final)
final[is.na(final)] <- 0

foo <- final %>% group_by(prec) %>% summarise(
  obama = sum(obama), 
  romney = sum(romney), 
  total_votes = sum(obama, romney, stein, johnson, goode, rocky)) %>% dplyr::select(prec, obama, romney, total_votes)

head(foo)


test <- dataCleanR(county)

head(test)
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

