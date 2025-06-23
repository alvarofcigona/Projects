library(readxl)
library(dplyr)
library(hms)
library(ggplot2)
library(forcats)
library(knitr)

library(png)
library(grid)

data <- read_excel("/Users/alvarofernandezdelacigonabarreiro/Desktop/Research Analyst - results.xlsx")

##Data Exploration (Understanding data)
head(data)
tail(data)
str(data)
summary(data)

different_sites <- unique(data$`Site Name`)
print(different_sites)
length(different_sites)
#####Important to metion only one NA, has to be looked at####
###165 different sites

different_Link_Account<- unique(data$`Link Account`)
print(different_Link_Account)
length(different_Link_Account)
###145 different link accounts
#####Important to metion only one NA, has to be looked at####

different_event_date<- unique(data$`Event Date`)
print(different_event_date)
length(different_event_date)
#####Important to metion only one NA, has to be looked at####
###character dates has to be converted to date objects and then ordered###
###38 days recorded of incidents

different_event_time<- unique(data$`Event Time`)
print(different_event_time)
###Check NAs here important, and order them###
###no need to use ength here, they are all times, irrelevant for study##

different_event_detail<- unique(data$`Event Detail`)
print(different_event_detail)
length(different_event_detail)
###Event time and the times behind the events are almost the same, many details merged into a variable, Danegrous (24495 different details)?####

different_resolutions<- unique(data$Resolution)
print(different_resolutions)
length(different_resolutions)
###19 different resolutions

different_handling_times <- unique(data$`Handling Time`)
print(different_handling_times)
###order this ones to see distribution###

different_installer <- unique(data$Installer)
print(different_installer)
###Only two installers. How many incidents does each one have?###

different_operator <- unique(data$Operator)
print(different_operator)
###One NA, 13 operators only counting NA####

###Super important to start grouping them together to see relations, many of them have only one NA, investigate






##Making Data Understandable
#Rearrangind data for better understanding
data_ordered <- data %>%
  mutate(`Event Date` = as.Date(`Event Date`, format = "%d/%m/%Y"), `Event Time` = as_hms(`Event Time`)) %>% 
  arrange(`Site Name`, `Event Date`, `Event Time`)

print(data_ordered)


#Missing Values
sapply(data, function(x) sum(is.na(x)))
###Site name, event date, event time, event detail same number of NAs: 43417
###Handling time and Operator same number of NAs: 128555
###Link account "alone", 53724 NAs####
###Creating a table to show it:
na_summary <- data.frame(
  `Variable Group` = c("Site Name, Event Date, Event Time, Event Detail",
                       "Handling Time and Operator",
                       "Link Account"),
  `NA Count` = c(43417, 128555, 53724)
)
kable(na_summary, caption = "Summary of Missing Values by Variable Group")


###Handle NAs after visualisations???###




#Stablishing relations between data(patterns) giving visualisations
##Operators
data <- read_excel("/Users/alvarofernandezdelacigonabarreiro/Desktop/Research Analyst - results.xlsx")

##Data Exploration (Understanding data)
head(data)
tail(data)
str(data)
summary(data)

different_sites <- unique(data$`Site Name`)
print(different_sites)
length(different_sites)
#####Important to metion only one NA, has to be looked at####
###165 different sites

different_Link_Account<- unique(data$`Link Account`)
print(different_Link_Account)
length(different_Link_Account)
###145 different link accounts
#####Important to metion only one NA, has to be looked at####

different_event_date<- unique(data$`Event Date`)
print(different_event_date)
length(different_event_date)
#####Important to metion only one NA, has to be looked at####
###character dates has to be converted to date objects and then ordered###
###38 days recorded of incidents

different_event_time<- unique(data$`Event Time`)
print(different_event_time)
###Check NAs here important, and order them###
###no need to use ength here, they are all times, irrelevant for study##

different_event_detail<- unique(data$`Event Detail`)
print(different_event_detail)
length(different_event_detail)
###Event time and the times behind the events are almost the same, many details merged into a variable, Danegrous (24495 different details)?####

different_resolutions<- unique(data$Resolution)
print(different_resolutions)
length(different_resolutions)
###19 different resolutions

different_handling_times <- unique(data$`Handling Time`)
print(different_handling_times)
###order this ones to see distribution###

different_installer <- unique(data$Installer)
print(different_installer)
###Only two installers. How many incidents does each one have?###

different_operator <- unique(data$Operator)
print(different_operator)
###One NA, 13 operators only counting NA####

###Super important to start grouping them together to see relations, many of them have only one NA, investigate






##Making Data Understandable
#Rearrangind data for better understanding
data_ordered <- data %>%
  mutate(`Event Date` = as.Date(`Event Date`, format = "%d/%m/%Y"), `Event Time` = as_hms(`Event Time`)) %>% 
  arrange(`Site Name`, `Event Date`, `Event Time`)

print(data_ordered)


#Missing Values
sapply(data, function(x) sum(is.na(x)))
###Site name, event date, event time, event detail same number of NAs: 43417
###Handling time and Operator same number of NAs: 128555
###Link account "alone", 53724 NAs####

###Handle NAs after visualisations???###








#Understanding the variables and it's relationships (patterns) through the presented data
##Operators
#Starting with the relationship between with the resolutions

operator_resolution_counts_filtered <- operator_resolution_counts %>%
  filter(!is.na(Operator) & !is.na(Resolution))

ggplot(operator_resolution_counts_filtered, aes(x = Operator, y = count, fill = Resolution)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_log10() +
  labs(title = "Number of Resolutions Given by Each Operator (NA Excluded)",
       x = "Operator",
       y = "Count of Resolutions (log scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
####This grouped bar plot shows the count of different resolutions given by each operator (with NA values excluded). The log scale on the y-axis compresses large counts and expands small ones, making it easier to compare operators even when counts differ by several orders of magnitude.


###Counts of resolutions per operator
operator_counts <- data %>%
  filter(!is.na(Operator)) %>%  # Exclude missing operators
  group_by(Operator) %>%
  summarise(incidents = n()) %>%
  arrange(desc(incidents))

ggplot(operator_counts, aes(x = reorder(Operator, incidents), y = incidents)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_log10() +
  coord_flip() +
  labs(title = "Number of Incidents Reported per Operator (Log Scale)",
       x = "Operator",
       y = "Incident Count (log scale)") +
  theme_minimal()

###Number of incidents per site (look into the different resolutions to find an interesting relationship)
# Count the number of incidents for each site (excluding missing values)
site_counts <- data %>%
  filter(!is.na(`Site Name`)) %>%
  group_by(`Site Name`) %>%
  summarise(incidents = n()) %>%
  arrange(desc(incidents)) %>%
  # Optional: convert Site Name into a factor with levels in order of descending incidents
  mutate(`Site Name` = factor(`Site Name`, levels = unique(`Site Name`)))
print(site_counts)

###Sites to look at
important_sites <- data.frame(
  Site = c("Ki 6PA", "RirmFK", "Bl2)KY", "Ma2SEH", "Wa10EH", "TAetG3", "TAesPH", "Bl3SPH", "An85DE", "Th08NE"),
  Count = c(22402, 17053, 11974, 7137, 3981, 2690, 2379, 2198, 2079, 2057)
)
# Print the table
print(important_sites)
count_incidents_crucial_sites <- sum(important_sites$Count)
total_incidents <- nrow(data)
print(total_incidents)
count_incidents_crucial_sites/total_incidents
###The first 10 most crucial sites by number of interest are a 49% of the whole data set, very very intresting, further exploring(could be compatred with the resolutions of those sites, by getting those 10 sites as a date frame and analysing it, could be really representative.


kable(important_sites, caption = "Important Sites and Their Incident Counts")




###Resolutions

# 1. Calculate resolution frequencies and order them
resolution_counts <- data %>%
  filter(!is.na(Resolution)) %>%
  count(Resolution) %>%
  arrange(desc(n)) %>%
  # Set factor levels so that the most frequent resolution appears first
  mutate(Resolution = factor(Resolution, levels = Resolution))

# Print the resolution counts
print(resolution_counts)

# 2. Create a pie chart of the resolution frequencies
pie_chart_frequencies <- ggplot(resolution_counts, aes(x = "", y = n, fill = Resolution)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y", start = 0) +
  labs(title = "Distribution of Resolutions", fill = "Resolution") +
  theme_void()
print(pie_chart_frequencies)
###None assigned highest one but we will have to avpoid for the moment, No visible cause second highest(error, human error?), activation outisde protected are(error again?), and then environmental conditions(!!!Interestingthis one!!!)






