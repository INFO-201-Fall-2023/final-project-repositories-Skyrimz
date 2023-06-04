
library(dplyr)


air_quality <- read.csv("Air_Quality.csv")
chronic_disease <- read.csv("Chronic_Disease.csv")
census <- read.csv("census.csv")

#reduce size

#2010

air_quality_2010 <- subset(air_quality, ReportYear == 2010)

chronic_disease_2010 <- subset(chronic_disease, YearStart == 2010)

r1_chronic_disease_2010 <- subset(chronic_disease_2010, Question == "Cancer of the lung and bronchus, mortality")

r2_chronic_disease_2010 <- subset(r1_chronic_disease_2010, StratificationCategory1 == "Gender")

r3_chronic_disease_2010 <- subset(r2_chronic_disease_2010, DataValueType == "Average Annual Number")

r1_air_quality_2010 <- subset(air_quality_2010, MeasureName == "Number of days with maximum 8-hour average ozone concentration over the National Ambient Air Quality Standard")

merged_data_2010 <- merge(x = r1_air_quality_2010, y = r3_chronic_disease_2010, by.x = "StateName", by.y = "LocationDesc", all.x = TRUE)

reduced_merged_data_2010 <- select(merged_data_2010, StateName, MeasureId, MeasureName, CountyName, ReportYear, Value, UnitName, YearStart, YearEnd, DataValue, DataSource, Topic, Question, StratificationCategory1, Stratification1)

reduced_merged_data_2010 <- reduced_merged_data_2010 %>%
  group_by(StateName) %>%
  mutate(`average Measured Air conditional Value in State` = mean(Value, na.rm = TRUE))

reduced_merged_data_2010$DataValue <- as.numeric(reduced_merged_data_2010$DataValue)

reduced_merged_data_2010 <- reduced_merged_data_2010 %>%
  group_by(StateName, CountyName) %>%
  mutate(`Total chronic disease value` = sum(DataValue, na.rm = TRUE))

census$Geographic.Area <- sub("^\\.", "", census$Geographic.Area)

census2010 <- census %>%
  select(Geographic.Area, X2010)

merged_census_data_2010 <- merge(reduced_merged_data_2010, census2010, by.x = "StateName", by.y = "Geographic.Area")

colnames(merged_census_data_2010)[colnames(merged_census_data_2010) == "X2010"] <- "state_census"

merged_census_data_2010$state_census <- gsub("[^0-9.]", "", merged_census_data_2010$state_census)

merged_census_data_2010$state_census <- as.numeric(merged_census_data_2010$state_census)

merged_census_data_2010 <- merged_census_data_2010 %>%
  mutate(Ratio.Total.chronic.disease.value.per.census = `Total chronic disease value` / state_census)

Final_DF_2010 <- merged_census_data_2010 %>%
  mutate(Index.Total.chronic.disease.value.per.census.per.average.Measured.Air.conditional.Value.in.State = (Ratio.Total.chronic.disease.value.per.census * 100) / `average Measured Air conditional Value in State`)

Final_DF_2010 <- Final_DF_2010 %>%
  mutate(Index.type = "percentage")

#2011

air_quality_2011 <- subset(air_quality, ReportYear == 2011)

chronic_disease_2011 <- subset(chronic_disease, YearStart == 2011)

r1_chronic_disease_2011 <- subset(chronic_disease_2011, Question == "Cancer of the lung and bronchus, mortality")

r2_chronic_disease_2011 <- subset(r1_chronic_disease_2011, StratificationCategory1 == "Gender")

r3_chronic_disease_2011 <- subset(r2_chronic_disease_2011, DataValueType == "Average Annual Number")

r1_air_quality_2011 <- subset(air_quality_2011, MeasureName == "Number of days with maximum 8-hour average ozone concentration over the National Ambient Air Quality Standard")

merged_data_2011 <- merge(x = r1_air_quality_2011, y = r3_chronic_disease_2011, by.x = "StateName", by.y = "LocationDesc", all.x = TRUE)

reduced_merged_data_2011 <- select(merged_data_2011, StateName, MeasureId, MeasureName, CountyName, ReportYear, Value, UnitName, YearStart, YearEnd, DataValue, DataSource, Topic, Question, StratificationCategory1, Stratification1)

reduced_merged_data_2011 <- reduced_merged_data_2011 %>%
  group_by(StateName) %>%
  mutate(`average Measured Air conditional Value in State` = mean(Value, na.rm = TRUE))

reduced_merged_data_2011$DataValue <- as.numeric(reduced_merged_data_2011$DataValue)

reduced_merged_data_2011 <- reduced_merged_data_2011 %>%
  group_by(StateName, CountyName) %>%
  mutate(`Total chronic disease value` = sum(DataValue, na.rm = TRUE))

census$Geographic.Area <- sub("^\\.", "", census$Geographic.Area)

census2011 <- census %>%
  select(Geographic.Area, X2011)

merged_census_data_2011 <- merge(reduced_merged_data_2011, census2011, by.x = "StateName", by.y = "Geographic.Area")

colnames(merged_census_data_2011)[colnames(merged_census_data_2011) == "X2011"] <- "state_census"

merged_census_data_2011$state_census <- gsub("[^0-9.]", "", merged_census_data_2011$state_census)

merged_census_data_2011$state_census <- as.numeric(merged_census_data_2011$state_census)

merged_census_data_2011 <- merged_census_data_2011 %>%
  mutate(Ratio.Total.chronic.disease.value.per.census = `Total chronic disease value` / state_census)

Final_DF_2011 <- merged_census_data_2011 %>%
  mutate(Index.Total.chronic.disease.value.per.census.per.average.Measured.Air.conditional.Value.in.State = (Ratio.Total.chronic.disease.value.per.census * 100) / `average Measured Air conditional Value in State`)

Final_DF_2011 <- Final_DF_2011 %>%
  mutate(Index.type = "percentage")

write.csv(Final_DF_2010, "df_2010.csv")
write.csv(Final_DF_2011, "df_2011.csv")
