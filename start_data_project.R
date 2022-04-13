####################################################
#Preparation
####################################################

library(usmap)
library(tidyverse)
library(data.table)
library(rvest)

####################################################
#Name Data
####################################################

#Files downloaded from https://www.ssa.gov/oact/babynames/limits.html
#Files are stored in folder names in working directory

#read all file names with loop, merge and store as names_df
names_path <- list.files("names")
names_df <- data.frame()

for (i in names_path){

  temp <- read_delim(paste0("names/", i), col_names = F)
  names_df <- bind_rows(names_df, temp)
  
}

#change column names
colnames(names_df) <- c("name", "gender", "count")

#summarize data frame by counting number of times with which the name occurs and the associated gender
#pivot to wider and create M/F as individual columns 
names_df <- names_df |> group_by(name, gender) |> summarize(count = sum(count))
names_df <- names_df |> pivot_wider(names_from = gender, values_from = count)
names_df <- names_df |> ungroup()

#set NA's to 0 and calculate percentage of each Name which was given to Male individuals
names_df[is.na(names_df)] <- 0
names_df$prob_M <- names_df$M / (names_df$M + names_df$F)

#determine a name to be Male if it is given to Male in +80% of the cases, as female if given to Female in +80% of cases 
#and leave undetermined if neither applies.
names_df$gender <- NA
names_df$gender[names_df$prob_M > 0.8] <- "M"
names_df$gender[names_df$prob_M < 0.2] <- "F"

#convert names to Upper for further analysis
names_df$name <- toupper(names_df$name)

####################################################
#Donation Data !check for duplicates in the data!
####################################################

#download donation bulk data from https://www.fec.gov/data/browse-data/?tab=bulk-data
#-> Contributions by Individuals -> 2019 - 2020

#as the file is very large (> 18 GB, we split it into several smaller lines with 1Mio lines each)
#use git bash command: 
#split FILENAME.txt -l 1000000

#files are stored in folder in working directory called donations

donations <- data.frame()
donation_files <- list.files("donations")

#read all file names with loop, merge and store as donations
for (i in donation_files[1]){
  
  temp <- fread(paste0("donations/", i), sep = "|", header = F, skip = 1)
  
  #filter out only donations which were made to Biden and Trump Fundraising Committee
  biden_for_president <- grepl("BIDEN FOR PRESIDENT", temp$V20, ignore.case = T)
  trump_for_president <- grepl("TRUMP FOR PRESIDENT", temp$V20, ignore.case = T)
  donations <- bind_rows(donations, temp[biden_for_president | trump_for_president,])
  
}

#import file provided by the FEC which contains the column names (were not provided in the donation files)
col_names_donations <- read_csv("header_file.csv", col_names = F)

#assign column names to donation table
colnames(donations) <- col_names_donations[1,]
donations <- as.data.frame(donations)

#Extract first and last names from NAME column 
donations$last_name <- str_extract_all(donations$NAME, pattern = "^[A-Za-z]+", simplify = T)
first_name <- str_split(donations$NAME, pattern = ",", simplify = T)[,2]

#split by space and select longer name 
first_name <- sapply(str_split(first_name, " "), function(x) x[which.max(nchar(x))])
#split by . and select longer name 
first_name <- sapply(str_split(first_name, "\\."), function(x) x[which.max(nchar(x))])
donations$first_name <- first_name

#select only columns that are needed for further analysis & do some cleaning
donations <- donations |> select(STATE, ZIP_CODE, EMPLOYER, OCCUPATION, TRANSACTION_AMT, first_name, last_name, MEMO_TEXT)
donations$MEMO_TEXT <- ifelse(grepl("BIDEN", donations$MEMO_TEXT), "BIDEN", "TRUMP")
donations <- donations |> rename("Candidate" = "MEMO_TEXT")
####################################################
#Analyze Donation Data
####################################################

merged <- merge(donations, names_df[c("name","gender")], by.x = "first_name", by.y = "name", all.x = T)

#remove NA's --> show impact of drop in table as output?
merged <-drop_na(merged)
#gender ratio per state from merged data_frame 
merged_summarized <- merged |> group_by(STATE, gender, Candidate) |> summarize(VALUE = sum(TRANSACTION_AMT))
merged_summarized <- pivot_wider(merged_summarized, names_from = gender, values_from = VALUE)

merged_summarized[is.na(merged_summarized)] <- 0
merged_summarized$pct_men <- merged_summarized$M / (merged_summarized$M + merged_summarized$F)


####################################################
#Plot Donation Data
####################################################

#clean frame to use for the US state plot function
plot_state <- merged_summarized  |> select(STATE, pct_men, Candidate)
colnames(plot_state) <- c("state", "values", "Candidate")

#get vector of proper US State Abbreviations
#store all data tables from Wikipedia website
wiki_tables <- read_html("https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States") |>
            html_nodes(css = "table") |> 
            html_table(fill = T)

table <- wiki_tables[[2]]
states <- table[2]
states <- states$`Flag, name andpostal abbreviation[13]`
states <- states[-1]

#filter out non-existent states
plot_state <- plot_state |> filter(state %in% states)

#plot result for republicans
data_republicans <- plot_state |> filter(Candidate == "TRUMP")

plot_usmap(regions = "states", data = data_republicans)+
  scale_fill_continuous(low = "white", high = "blue", name = "% donations by men")+
  labs(title = "TITLE", subtitle = "SUBTITLE")+
  theme(legend.position = "right")

#plot result for democrats 
data_democrats <- plot_state |> filter(Candidate == "BIDEN")

plot_democrats <- plot_usmap(regions = "states", data = data_democrats)+
                    scale_fill_continuous(low = "white", high = "blue", name = "% donations by men")+
                    labs(title = "TITLE", subtitle = "SUBTITLE")+
                    theme(legend.position = "right")

ggsave(plot_democrats, device = "png", filename = "sample")
