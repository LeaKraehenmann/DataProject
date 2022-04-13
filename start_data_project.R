library(tidyverse)
setwd("C:/Users/sveng/OneDrive/Dokumente/My Documents/Schule/Studium/Taiwan/Data Analysis/Project")
names_path <- list.files("names")

names_df <- data.frame()

for (i in names_path){

  temp <- read_delim(paste0("names/", i), col_names = F)
  names_df <- bind_rows(names_df, temp)
  
}

colnames(names_df) <- c("name", "gender", "count")

names_df <- names_df |> group_by(name, gender) |> summarize(count = sum(count))
names_df <- names_df |> pivot_wider(names_from = gender, values_from = count)

names_df[is.na(names_df)] <- 0
names_df$prob_M <- names_df$M / (names_df$M + names_df$F)

names_df$gender <- NA
names_df$gender[names_df$prob_M > 0.8] <- "M"
names_df$gender[names_df$prob_M < 0.2] <- "F"

names_df[names_df$name == "Kim",]

names_df[names_df$prob_M > 0.3 & names_df$prob_M < 0.4,]


#we could also weight the donations and count by probability (ie if somebody who's name is male with prob. 0.7 donated 
#100 then 70 are allocated to male, 30 are allocated to female ? )

#mention limitations in paper that we only alanyzed donations to "biden for president" and that the sample of those
#oepole who voted for this may not be representative!!!

#data source: https://www.ssa.gov/oact/babynames/limits.html

#bidenvictory fund is connected to national committee where donors can give and contribute much much more money than to the 
#regulated biden for presidency campaign!


