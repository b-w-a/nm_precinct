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
                              "total_votes", data = nm_2016, 
                              table_names = table_names)

write_csv(results_nm_2016,"/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2016_ei_estimates.csv")

# plots -----

# weighted vote share 
df <- gather(data,candidate, pct_vote ,c(pct_clinton, pct_trump))

drop <- which(df$pct_vote==0 | df$pct_vote == 1)
df <- df[-drop,]
drop2 <- which(df$pct_latino > 1.0)
df <- df[-drop2,]



weighted <- ggplot(df, aes(x=pct_latino, y = pct_vote, color = candidate, weight = total_votes, size = total_votes)) + geom_point(alpha = .10) + 
  scale_color_manual(values = c('blue', 'red'), 
                     breaks = c('pct_clinton', 'pct_trump'),
                     labels = c('Clinton', 'Trump'), 
                     name = "Candidate") + 
  stat_smooth(se = F) + 
  theme_bw() + 
  scale_y_continuous(limits=c(0,1), breaks = c(seq(0,1,.1))) + 
  labs(title = "New Mexico Presidential Vote: Official Precinct-Level Election Returns", 
       x = "Percent Latino Registered Voter in Precinct", 
       y = "2016 Presidental Vote Share") + 
  guides(size=F)

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2016_vote_share.png", weighted, height = 8, width = 8)


# density plot ----
# ei through ei just for clinton
model_clinton <- pct_clinton ~ pct_latino

ei_clinton <- ei(model_clinton, total="total_votes", erho=.5, data=nm_2016)
beta_clinton <- eiread(ei_clinton, "betab")
df_beta <- data.frame(beta = beta_clinton)

ei_est <- eiread(ei_clinton, "maggs")[1]


plot <- ggplot(df_beta, aes(x=beta)) + geom_density() +
  geom_vline(xintercept = .54, col = "red", lty = 2) + theme_bw() +
  geom_vline(xintercept = ei_est, lty = 2) + 
  annotate("text", x = .4, y = 7.5, label = "Exit Poll \n Estimate = .54", size = 3) +
  annotate("text", x = .8, y = 8.5, label = "EI Estimate\n = .69", size = 3) + 
  labs(x = "Estimated Latino Vote for Clinton", y = "Density", title = "New Mexico Presidential Latino Vote") 

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/new_mexico/nm_precinct/2016_density.png", plot, height = 8, width = 8)

pvalue_exit <- mean(df_beta$beta < .54, na.rm = T)  

mean(df_beta$beta < ei_est, na.rm = T)

