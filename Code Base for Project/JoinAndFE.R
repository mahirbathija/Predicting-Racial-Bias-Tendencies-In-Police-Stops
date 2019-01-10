library(dplyr)
library(tidyr)
library(stringr)

stop <- read.csv('StopDataCleaned.csv', stringsAsFactors = F)
election.2008 <- read.csv('2008ElectionResults_cleanData.csv', stringsAsFactors = F)
election.2012 <- read.csv('2012ElectionResults_cleanData.csv', stringsAsFactors = F)
election.2016 <- read.csv('2016ElectionResults_cleanData.csv', stringsAsFactors = F)
demo <- read.csv('demographics_cleanData.csv', stringsAsFactors = F)

election.2008 <- election.2008[, -3]
election.2008 <- spread(election.2008, Party, PercentageOfTotalVotes)
election.2008$County <- paste(election.2008$County, "County")
election.2012 <- election.2012[, -3]
election.2012 <- spread(election.2012, Party, PercentageOfTotalVotes)
election.2012$County <- paste(election.2012$County, "County")
election.2016 <- election.2016[, -3]
election.2016 <- spread(election.2016, Party, PercentageOfTotalVotes)
election.2016$County <- paste(election.2016$County, "County")

demo <- demo[, -2]
demo <- spread(demo, race, percentage)
demo$`White alone` <- demo$`White alone` - demo$`Hispanic or Latino`
demo <- demo[, -9]

stop.demo <- left_join(stop, demo, by = c("county_name" = "county"))

stop.demo$year <- substring(stop.demo$id, 4, 7)
stops1 <- stop.demo %>%
  filter(year == '2009' | year == '2010')
stops2 <- stop.demo %>%
  filter(year == '2011' | year == '2012' | year == '2013' | year == '2014')
stops3 <- stop.demo %>%
  filter(year == '2015' | year == '2016')

final1 <- inner_join(stops1, election.2008, by = c('county_name' = 'County'))
final2 <- inner_join(stops2, election.2012, by = c('county_name' = 'County'))
final3 <- inner_join(stops3, election.2016, by = c('county_name' = 'County'))

data <- rbind(final1, final2, final3)

data$DUIviolation <- ifelse(grepl('dui', tolower(data$violation)), 1, 0)
data$SpeedingViolation <- ifelse(grepl('speeding', tolower(data$violation)), 1, 0)
data$EquipmentViolation <- ifelse(grepl('equipment', tolower(data$violation)), 1, 0)
data$LicenseViolation <- ifelse(grepl('license', tolower(data$violation)), 1, 0)
data$LightsViolation <- ifelse(grepl('lights', tolower(data$violation)), 1, 0)
data$PaperworkViolation <- ifelse(grepl('paperwork', tolower(data$violation)), 1, 0)
data$SafeMovementViolation <- ifelse(grepl('safe movement', tolower(data$violation)), 1, 0)
data$StoppingViolation <- ifelse(grepl('stop sign', tolower(data$violation)), 1, 0)
data$RegistrationViolation <- ifelse(grepl('registration', tolower(data$violation)), 1, 0)
data$SeatBeltViolation <- ifelse(grepl('seat belt', tolower(data$violation)), 1, 0)
data$MovingViolation <- ifelse(grepl('moving', tolower(data$violation)), 1, 0)
data$CellPhoneViolation <- ifelse(grepl('cell phone', tolower(data$violation)), 1, 0)
data$TruckViolation <- ifelse(grepl('truck', tolower(data$violation)), 1, 0)
data$OtherViolation <- ifelse(grepl('other', tolower(data$violation)), 1, 0)

data$TotalViolations <- str_count(data$violation, pattern = ',') + 1

data$MinorityDriver <- ifelse(data$driver_race != 'White', 1, 0)
data$MinorityOfficer <- ifelse(data$officer_race != 'White', 1, 0)

data$DriverOfficerSameRace <- ifelse(data$officer_race == data$driver_race, 1, 0)

data$ArrestOrCitation <- ifelse(data$stop_outcome == "Arrest or Citation", 1, 0)

write.csv(data, file = 'complete_clean_data.csv', row.names = F)
