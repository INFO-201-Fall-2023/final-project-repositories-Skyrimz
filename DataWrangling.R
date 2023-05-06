
library(dplyr)


air_quality <- read.csv("Air_Quality.csv")
chronic_disease <- read.csv("Chronic_Disease.csv")
census <- read.csv("census.csv")

air_quality_2010 <- subset(air_quality, ReportYear == 2010)
chronic_disease_2010 <- subset(chronic_disease, YearStart == 2010)

merged_data <- merge(x = air_quality_2010, y = chronic_disease_2010, by.x = "StateName", by.y = "LocationDesc", all.x = TRUE)


reduced_merged_data <- subset(merged_data, Question == "Cancer of the lung and bronchus, mortality")
df1 <- subset(reduced_merged_data, MeasureName == "Number of days with maximum 8-hour average ozone concentration over the National Ambient Air Quality Standard (monitor and modeled data)")
df2 <- subset(df1, DataValueType == "Average Annual Number")
df3 <- subset(df2, StratificationCategory1 == "Gender")
str(df3)
reduced_df3 <- select(df3, StateName, MeasureId, MeasureName, CountyName, ReportYear, Value, UnitName, YearStart, YearEnd, DataValue, DataSource, Topic, Question, StratificationCategory1, Stratification1)
reduced_df3 <- reduced_df3 %>%
  group_by(StateName) %>%
  mutate(`average Measured Air conditional Value in State` = mean(Value, na.rm = TRUE))

reduced_df3$DataValue <- as.numeric(reduced_df3$DataValue)

reduced_df3 <- reduced_df3 %>%
  group_by(StateName, CountyName) %>%
  mutate(`Total chronic disease value` = sum(DataValue, na.rm = TRUE))

census$Geographic.Area <- sub("^\\.", "", census$Geographic.Area)

census2010 <- census %>%
  select(Geographic.Area, X2010)

merged_census_data <- merge(reduced_df3, census2010, by.x = "StateName", by.y = "Geographic.Area")
colnames(merged_census_data)[colnames(merged_census_data) == "X2010"] <- "state_census"
str(merged_census_data)
merged_census_data$state_census <- gsub("[^0-9.]", "", merged_census_data$state_census)
merged_census_data$state_census <- as.numeric(merged_census_data$state_census)
merged_census_data <- merged_census_data %>%
  mutate(Ratio.Total.chronic.disease.value.per.census = `Total chronic disease value` / state_census)

Final_DF <- merged_census_data %>%
  mutate(Index.Total.chronic.disease.value.per.census.per.average.Measured.Air.conditional.Value.in.State = (Ratio.Total.chronic.disease.value.per.census * 100) / `average Measured Air conditional Value in State`)
Final_DF <- Final_DF %>%
  mutate(Index.type = "percentage")
