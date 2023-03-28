### Part 1: Load and prepare all files

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
  )) # Ass a most likely race column

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
write.csv(applications, "C:\\Users\\dobri\\OneDrive\\Desktop\\McGill Courses\\ORGB 672\\672_project_data\\APP.csv", row.names=FALSE) # Generate a CSV file

### Part 2: Split and Analyze three subsets

# Get three subsets based on the art unit

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

# Create a box plot based on unit identifier to compare tenure

par(mfrow=c(1,1)) # Reset the size

boxplot(tenure_days~AU,
        data=APPBIG,
        main="Different boxplots for each art unit",
        xlab="Art Unit Number",
        ylab="Tenure Days",
        col="blue",
        border="black"
)

# With the values of the boxplot below

summary(APP161$tenure_days)
summary(APP179$tenure_days)
summary(APP242$tenure_days)

# Create a pie charts for gender

par(mfrow=c(1,3)) # Put all three of them on one page

pie(table(APP161$gender), main="161 Gender Breakdown (majority female)", col=c("pink","light blue"))

table(APP161$gender)

pie(table(APP179$gender), main="179 Gender Breakdown (one third female)", col=c("pink","light blue"))

table(APP179$gender)

pie(table(APP242$gender), main="242 Gender Breakdown (one fifth female)", col=c("pink","light blue"))

table(APP242$gender)

par(mfrow=c(1,4)) # Reset the size

# Idem for race

pie(table(APP161$race), main = "161 Race Breakdown")
table(APP161$race)

pie(table(APP242$race), main = "242 Race Breakdown")
table(APP242$race)

pie(table(APP179$race), main = "179 Race Breakdown")
table(APP179$race)

pie(table(APPBIG$race), main = "Total Race Breakdown") # Include a pie chart for all subsets together
table(APPBIG$race)

### Part 3: Network Plotting

## Network Creation

# Edges

edges_APP = edges %>%
  filter(ego_examiner_id %in% APPBIG$examiner_id & alter_examiner_id %in% APPBIG$examiner_id) %>%
  drop_na() %>%
  select (to = ego_examiner_id, from = alter_examiner_id)

# Nodes

nodes_APP = edges_APP %>%
  pivot_longer(cols=c("from", "to")) %>%
  distinct(examiner_id = value) %>%
  left_join(APPBIG, by = "examiner_id", multiple = "all") %>%
  distinct(examiner_id, gender, race, tenure_days) %>% rename(name = examiner_id) %>% mutate(name = as.character(name))

library(igraph)
library(tidygraph)
library(ggraph)
library(gridExtra)

# Create Network Element

network = graph_from_data_frame(edges_APP, directed = TRUE) %>% as_tbl_graph() %>% left_join(nodes_APP, by="name")

# Calculate the Three Centrality Values

BC = betweenness(network)
DC = degree(network)
CC = closeness(network)

# Add the Values to Each Node

nodes_APP$BC = BC
nodes_APP$DC = DC
nodes_APP$CC = CC

# Plot by Gender

plotG = ggraph(network, layout = "fr") + 
  geom_edge_link(edge_colour = "black", alpha = 0.05) + 
  geom_node_point(aes(color = gender, size = tenure_days)) + theme_void()
plotG

# Plot by Race

plotR = ggraph(network, layout = "fr") + 
  geom_edge_link(edge_colour = "black", alpha = 0.05) + 
  geom_node_point(aes(color = race, size = tenure_days)) + theme_void()
plotR
