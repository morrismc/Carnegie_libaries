# this was written by Matthew morriss starting on April 24, 2019
# to compile the list from wikipedia of all carnegie libraries

#################################### SECTION TITLE ####################################
setwd('/Users/matthew/Documents/GitHub/Carnegie_Libraries')
library(rvest)
library(RCurl)
library(XML)
library(magrittr)
library(tidyverse)
library(lubridate)
library(ggridges)
library(ExPanDaR)
rm(list = ls())
#################################### Load data ####################################
url <- 'https://en.wikipedia.org/wiki/List_of_Carnegie_libraries_in_the_United_States'
url2<-read_html(url)
parsed<-htmlParse(url2)

links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")

df <- as.data.frame(matrix(unlist(links), byrow = TRUE))
df1 <- as.data.frame(df[grep('/wiki/List',df$V1),])
df1 <- distinct(df1)
names(df1) <- c('V1')

df1 <- as.data.frame(df1[!grepl('Africa|Europe|Philadelphia|Oceania|Caribbean|City|D.C.|District|Puerto|Canada|United' ,df1$V1),])
names(df1) <- c('V1')
string <- as.character(df1$V1)
df3 <- as.data.frame(paste0("https://en.wikipedia.org", string))
colnames(df3) <- "list"
df3 <- as.data.frame(droplevels(df3$list))
colnames(df3) <- "list"
write.csv(df3,file = 'Carnegie_Library_links.csv')

rm(list = ls())
#################################### Go get all data ####################################
# April 25, 2019, still need to add the label of which state the libraries
# are in somehow... 
rm(list = ls())
df <- read.csv('Carnegie_Library_links.csv')
libData <- list() 

for(i in 1:nrow(df)){
  Sys.sleep(0.1)
  print(i)
  url <- levels(df$list)[i]
  website <- read_html(url)
  
  tbls_ls <- website %>%
    html_nodes("table") %>%
    .[1:2] %>%
    html_table(fill = TRUE)
  
  t = gsub(pattern = '_',replacement = ' ', x = url)
  
  # if(any(grepl(t,"Philadelphia"))){
  #   state <- 'Pennsylvania'
  # }
  # else{
  state <- str_extract(t, paste(state.name, collapse='|'))
  # }
  #need to add an if statment if there's another table extracted first
  # to only get the state tabel
  if(any(grepl('Map',tbls_ls[[1]][1,]))){
    tempDf <- as.data.frame(tbls_ls[[2]]) 
    tempDf <- add_column(tempDf,state)
    names(tempDf) = gsub(pattern = "\\[[^]]*]", replacement = "", x = names(tempDf))
    names(tempDf) = gsub(pattern = "Remarks", replacement = "Notes", x = names(tempDf))
    names(tempDf) = gsub(pattern = "Locality", replacement = "City ortown", x = names(tempDf))
    libData[[i]] <- tempDf
  }
  else{
    tempDf <- as.data.frame(tbls_ls[[1]]) 
    tempDf <- add_column(tempDf,state)
    names(tempDf) = gsub(pattern = "\\[[^]]*]", replacement = "", x = names(tempDf))
    names(tempDf) = gsub(pattern = "Remarks", replacement = "Notes", x = names(tempDf))
    names(tempDf) = gsub(pattern = "Locality", replacement = "City ortown", x = names(tempDf))
    libData[[i]] <- tempDf
  }
}
  

libraries <- plyr::ldply(libData, data.frame)
write.csv(libraries,file = 'Carnegie_library_Data.csv')


#################################### format ####################################
rm(list = ls())
df <- read.csv('Carnegie_library_Data.csv')

# df$Dategranted <- toString(df$Dategranted)
df$Dategranted <- mdy(df$Dategranted)

df <- df %>%
  mutate(yearD = decimal_date(Dategranted)) %>%
  mutate(year = year(Dategranted))

names(df) <- c('N', 'N in state','Name','City','image','Date Granted','Grant Amount',
               'location','notes','State','City2','Status','Town','Year GrantedD','Year Granted')

keeps <- c('Name','City','Date Granted','Grant Amount','State', 'Year GrantedD','Year Granted')
df <-  df[,keeps,drop = FALSE]
write.csv(df,'Carnegie_library_data2.csv')
#################################### Plot! Line plot ####################################
rm(list = ls())
df <- read.csv('Carnegie_library_data2.csv')
df %>%
  arrange((`Year.GrantedD`)) %>%
  ggplot(aes(x = `Year.GrantedD`,
             y = 1:nrow(df))) +
  geom_line()+
  xlim(1890, 1920)+
  theme_light()+
  labs(x = 'Year Granted', y = 'Number', title = 'Number of Carnegie Libraries Granted')

#################################### Histogram of Libraries ####################################

df %>%
  ggplot(aes(x = `Year GrantedD`))+
  geom_histogram(binwidth = 1, color = 'black',fill = 'blue') +
  xlim(1890, 1920)+
  theme_light()+
  labs(x = 'Year Granted', y = 'Number', title = 'Number of Carnegie Libraries Granted')

#################################### Bubble plot by state ####################################

df %>%
  group_by(State, `Year GrantedD`) %>%
  summarize(n = n()) %>%
  filter(.,n >= 3) %>%
  ggplot(aes(x = `Year GrantedD`, y = reorder(State,desc(State)), color = n, 
             size = n))+
  geom_point()+
  xlim(1899, 1920)+
  theme_light()+
  labs(x = 'Year Granted', y = 'State', title = 'Number of Carnegie Libraries Granted',
       size = 'Number',color = 'Number')+
theme(axis.text.y = element_text(angle = 45, hjust = 1))

#################################### Ridges plot ####################################

df %>%
  group_by(State, `Year.GrantedD`) %>%
  summarize(n = n()) %>%
  
