### Part 0: Pre-process the data like in Exercise 3
  
install.packages("arrow") # Installing all the necessary packages
library(arrow)
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("stringr")
library(stringr)
install.packages("skimr")
library(skimr)
install.packages("gender")
library(gender)
install.packages("wru")
library(wru)

data_path = "C:\\Users\\dobri\\OneDrive\\Desktop\\McGill Courses\\ORGB 672\\672_project_data\\" # Setting up the data path
applications = read_parquet(paste0(data_path,"app_data_sample.parquet"))
edges = read_csv(paste0(data_path,"edges_sample.csv"))

applications # Print the application table and the edge table
edges

install_genderdata_package() # Do this the first time you run the package

examiner_names = applications %>% 
  distinct(examiner_name_first)

examiner_names # Print all unique first names

examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

examiner_names_gender # Estimate the gender based on the gender name

# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()

examiner_surnames <- applications %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_surnames # Get all unique surnames

examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()

examiner_race # Estimate the race based on the last name
write.csv(examiner_race, "C:\\Users\\dobri\\OneDrive\\Desktop\\McGill Courses\\ORGB 672\\672_project_data\\LNAMES.csv", row.names=FALSE) # Generate a CSV file

examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  )) # As a most likely race column

examiner_race # Re-print the table

# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)

applications <- applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

rm(examiner_race)
rm(examiner_surnames)
gc()

examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date) 

examiner_dates # Print the dates

examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) # Format all dates in the YYYY-MM-DD format

examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
  ) %>% 
  filter(year(latest_date)<2018) # Calculate how long an employee has been in the USPTO

examiner_dates # PRINT!!!!!!!!
examiner_dates[order(examiner_dates$tenure_days, decreasing = TRUE, na.last = TRUE),] # Check to see who has been there the longest

applications <- applications %>% 
  left_join(examiner_dates, by = "examiner_id") # Clean the data one more time

rm(examiner_dates)
gc()

applications

### Part 1: Calculate Processing Time

applications = applications %>% mutate(decision_date = pmax(patent_issue_date, abandon_date, na.rm = TRUE))
applications$decision_date

applications$app_proc_time = as.numeric(as.Date(as.character(applications$decision_date)) - as.Date(as.character(applications$filing_date)))
applications$app_proc_time
summary(applications$app_proc_time)

applications = applications %>% drop_na(app_proc_time)

applications$examiner_PT = with(applications, ave(app_proc_time, examiner_id, FUN=mean))
applications$examiner_PT
applications
colnames(applications)

write.csv(applications, "C:\\Users\\dobri\\OneDrive\\Desktop\\McGill Courses\\ORGB 672\\672_project_data\\APP2.csv", row.names=FALSE) # Generate a CSV file

### Part 2: Run a Regression

## Sub-Part 0: Pre-process the data like in exercise 3

APP161 = applications[applications$examiner_art_unit %in% (1610:1619), ]  
APP161
APP242 = applications[applications$examiner_art_unit %in% (2420:2429), ]  
APP242
APP179 = applications[applications$examiner_art_unit %in% (1790:1799), ]  
APP179

# Add an art unit identifier

APP161$AU = 161
APP242$AU = 242
APP179$AU = 179

# Then merge the three datasets

APPSMALL = APP161 %>% full_join(APP242)
APPBIG = APPSMALL %>% full_join(APP179)
APPBIG

summary(APP161$tenure_days)
summary(APP179$app_proc_time)
print(applications$app_proc_time)

## Sub-part 1: Re-create the network

# Edges

edges_ALL = edges %>%
  filter(ego_examiner_id %in% applications$examiner_id & alter_examiner_id %in% applications$examiner_id) %>%
  drop_na() %>%
  select (to = ego_examiner_id, from = alter_examiner_id)

# Nodes

nodes_ALL = edges_ALL %>%
  pivot_longer(cols=c("from", "to")) %>%
  distinct(examiner_id = value) %>%
  left_join(applications, by = "examiner_id", multiple = "all") %>%
  distinct(examiner_id, gender, race, tenure_days, examiner_PT) %>% rename(name = examiner_id) %>% mutate(name = as.character(name))

library(igraph)
library(tidygraph)
library(ggraph)
library(gridExtra)

# Create Network Element

network2 = graph_from_data_frame(edges_ALL, directed = TRUE) %>% as_tbl_graph() %>% left_join(nodes_ALL, by="name")

# Calculate the Three Centrality Values

BC2 = betweenness(network2)
DC2 = degree(network2)
CC2 = closeness(network2)

# Add the Values to Each Node

nodes_ALL$BC = BC2
nodes_ALL$DC = DC2
nodes_ALL$CC = CC2

## Sub-part 2: Check if all the values are good

nodes_ALL

## Sub-part 3: Plot the Actual Regression

lm11 = lm(nodes_ALL$examiner_PT ~ nodes_ALL$DC + nodes_ALL$CC + nodes_ALL$BC)
summary(lm11)

lm22 = lm(nodes_ALL$examiner_PT ~ nodes_ALL$DC)
summary(lm22)
plot(nodes_ALL$DC, nodes_ALL$examiner_PT)
b0=coef(lm22)[1]
b1=coef(lm22)[2]
abline(b0,b1)

lm33 = lm(nodes_ALL$examiner_PT ~ nodes_ALL$tenure_days)
summary(lm33)
plot(nodes_ALL$tenure_days, nodes_ALL$examiner_PT)
b0=coef(lm33)[1]
b1=coef(lm33)[2]
abline(b0,b1)

### Part 3: Add an examiner gender variable

lm55 = lm(nodes_ALL$examiner_PT ~ nodes_ALL$DC + nodes_ALL$gender)
summary(lm55)
plot(nodes_ALL$DC, nodes_ALL$examiner_PT, col=ifelse(nodes_ALL$gender=="female", "red", "blue"))
b0=coef(lm55)[1]
b1=coef(lm55)[2]
b2=coef(lm55)[3]
abline(b0+b2,b1, col="blue")
abline(b0,b1, col="red")

lm66 = lm(nodes_ALL$examiner_PT ~ nodes_ALL$DC + nodes_ALL$gender + nodes_ALL$gender*nodes_ALL$DC)
summary(lm66)
plot(nodes_ALL$DC, nodes_ALL$examiner_PT, col=ifelse(nodes_ALL$gender=="female", "red", "blue"))
b0=coef(lm66)[1]
b1=coef(lm66)[2]
b2=coef(lm66)[3]
b3=coef(lm66)[4]
abline(b0+b2,b1+b3, col="blue")
abline(b0,b1, col="red")

# It crosses!!! 

'''
(b0+b2) + X*(b1 + b3) = (b0) + X*(b1)
b2 + b3*X = 0
X = b2/(-b3) = 42.36193
Y = b0 + X*b1 = 1281.566
'''

b2/(-b3)
b0 + (b2/(-b3))*b1

POINT = c(42.36193, 1281.566)

POINT

### Part 4: Do the results differ if we take the whole dataset? 

# New Edges

edges_APP = edges %>%
  filter(ego_examiner_id %in% APPBIG$examiner_id & alter_examiner_id %in% APPBIG$examiner_id) %>%
  drop_na() %>%
  select (to = ego_examiner_id, from = alter_examiner_id)

# New Nodes

nodes_APP = edges_APP %>%
  pivot_longer(cols=c("from", "to")) %>%
  distinct(examiner_id = value) %>%
  left_join(APPBIG, by = "examiner_id", multiple = "all") %>%
  distinct(examiner_id, gender, race, tenure_days, examiner_PT) %>% rename(name = examiner_id) %>% mutate(name = as.character(name))

# Create New Network Element

network = graph_from_data_frame(edges_APP, directed = TRUE) %>% as_tbl_graph() %>% left_join(nodes_APP, by="name")

# Calculate the Three New Centrality Values

BC = betweenness(network)
DC = degree(network)
CC = closeness(network)

# Add the New Values to Each Node

nodes_APP$BC = BC
nodes_APP$DC = DC
nodes_APP$CC = CC

nodes_APP

lm1 = lm(nodes_APP$examiner_PT ~ nodes_APP$DC + nodes_APP$CC + nodes_APP$BC)
summary(lm1)

lm2 = lm(nodes_APP$examiner_PT ~ nodes_APP$CC)
summary(lm2)
plot(nodes_APP$CC, nodes_APP$examiner_PT)
b0=coef(lm2)[1]
b1=coef(lm2)[2]
abline(b0,b1)

lm3 = lm(nodes_APP$examiner_PT ~ nodes_APP$tenure_days)
summary(lm3)
plot(nodes_APP$tenure_days, nodes_APP$examiner_PT)
b0=coef(lm3)[1]
b1=coef(lm3)[2]
abline(b0,b1)


lm5 = lm(nodes_APP$examiner_PT ~ nodes_APP$CC + nodes_APP$gender)
summary(lm5)

plot(nodes_APP$CC, nodes_APP$examiner_PT, col=ifelse(nodes_APP$gender=="female", "red", "blue"))

b0=coef(lm5)[1]
b1=coef(lm5)[2]
b2=coef(lm5)[3]
abline(b0+b2,b1, col="blue")
abline(b0,b1, col="red")

lm6 = lm(nodes_APP$examiner_PT ~ nodes_APP$CC + nodes_APP$gender + nodes_APP$gender*nodes_APP$CC)
summary(lm6)

plot(nodes_APP$CC, nodes_APP$examiner_PT, col=ifelse(nodes_APP$gender=="female", "red", "blue"))

b0=coef(lm6)[1]
b1=coef(lm6)[2]
b2=coef(lm6)[3]
b3=coef(lm6)[4]
abline(b0+b2,b1+b3, col="blue")
abline(b0,b1, col="red")

