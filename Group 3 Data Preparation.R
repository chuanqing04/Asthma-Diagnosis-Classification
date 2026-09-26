install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("plotly")
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

sample_data = read.csv("asthma_dataset.csv", header = TRUE)


asthma_data <- data.frame(lapply(asthma_data, function(x) {
  if (is.character(x)) {
    # Remove special characters and the string "unknown"
    x <- gsub("[?_,-]", "", x)       # Remove ?, - characters
    x <- gsub("unknown", "", x)     # Remove the string "unknown"
    x <- trimws(x)                  # Trim any leading or trailing whitespace
  }
  x
}))

# Set columns with numeric attributes to numeric datatype
asthma_data$BMI <- as.numeric(asthma_data$BMI)
asthma_data$PhysicalActivity <- as.numeric(asthma_data$PhysicalActivity)
asthma_data$DietQuality <- as.numeric(asthma_data$DietQuality)
asthma_data$SleepQuality <- as.numeric(asthma_data$SleepQuality)
asthma_data$PollutionExposure <- as.numeric(asthma_data$PollutionExposure)
asthma_data$PollenExposure <- as.numeric(asthma_data$PollenExposure)
asthma_data$DustExposure <- as.numeric(asthma_data$DustExposure)
asthma_data$LungFunctionFEV1 <- as.numeric(asthma_data$LungFunctionFEV1)
asthma_data$LungFunctionFVC <- as.numeric(asthma_data$LungFunctionFVC)


# Define a function to replace NA and out-of-range value to the column's mean and quartiles
replace_value_numeric <- function(data, columnName, min_range, max_range){
  # Extract data of the column into a variable
  columnData <- data[[columnName]]
  # Calculate quartiles and mean of the column
  quartiles <- quantile(columnData, probs = c(0.25, 0.75), na.rm = TRUE)
  mean_value <- mean(columnData, na.rm = TRUE)
  # Define 1st Quartile (Q1) and 3rd Quartile (Q3) for replacing out-of-range values
  Q1 <- quartiles[1]
  Q3 <- quartiles[2]
  # Replace out-of-range values with Q1 or Q3
  columnData <- ifelse(columnData < min_range & !is.na(columnData), Q1, columnData)
  columnData <- ifelse(columnData > max_range & !is.na(columnData), Q3, columnData)
  # Replace NA values with the mean of the column
  columnData[is.na(columnData)] <- mean_value
  # Update the column in the original data frame
  data[[columnName]] <- columnData
  return(data)
}

# Apply the function to each numerical column
asthma_data <- replace_value_numeric(asthma_data, "Age", min_range = 5, max_range = 80)
asthma_data <- replace_value_numeric(asthma_data, "BMI", min_range = 15, max_range = 40)
asthma_data <- replace_value_numeric(asthma_data, "PhysicalActivity", min_range = 0, max_range = 10)
asthma_data <- replace_value_numeric(asthma_data, "DietQuality", min_range = 0, max_range = 10)
asthma_data <- replace_value_numeric(asthma_data, "SleepQuality", min_range = 4, max_range = 10)
asthma_data <- replace_value_numeric(asthma_data, "PollutionExposure", min_range = 0, max_range = 10)
asthma_data <- replace_value_numeric(asthma_data, "PollenExposure", min_range = 0, max_range = 10)
asthma_data <- replace_value_numeric(asthma_data, "DustExposure", min_range = 0, max_range = 10)
asthma_data <- replace_value_numeric(asthma_data, "LungFunctionFEV1", min_range = 1.0, max_range = 4.0)
asthma_data <- replace_value_numeric(asthma_data, "LungFunctionFVC", min_range = 1.5, max_range = 6.0)

#Rounding the data
asthma_data$Gender = round(as.numeric(asthma_data$Gender))
asthma_data$Ethnicity = round(as.numeric(asthma_data$Ethnicity))
asthma_data$EducationLevel = round(as.numeric(asthma_data$EducationLevel))
asthma_data$Smoking = round(as.numeric(asthma_data$Smoking))
asthma_data$PetAllergy = round(as.numeric(asthma_data$PetAllergy))
asthma_data$FamilyHistoryAsthma = round(as.numeric(asthma_data$FamilyHistoryAsthma))
asthma_data$HistoryOfAllergies = round(as.numeric(asthma_data$HistoryOfAllergies))
asthma_data$Eczema = round(as.numeric(asthma_data$Eczema))
asthma_data$HayFever = round(as.numeric(asthma_data$HayFever))
asthma_data$GastroesophagealReflux = round(as.numeric(asthma_data$GastroesophagealReflux))
asthma_data$Wheezing = round(as.numeric(asthma_data$Wheezing))
asthma_data$ShortnessOfBreath = round(as.numeric(asthma_data$ShortnessOfBreath))
asthma_data$ChestTightness = round(as.numeric(asthma_data$ChestTightness))
asthma_data$Coughing = round(as.numeric(asthma_data$Coughing))
asthma_data$NighttimeSymptoms = round(as.numeric(asthma_data$NighttimeSymptoms))
asthma_data$ExerciseInduced = round(as.numeric(asthma_data$ExerciseInduced))
asthma_data$Diagnosis = round(as.numeric(asthma_data$Diagnosis))

#Converting data into Integer
asthma_data$Gender <- as.integer(asthma_data$Gender)
asthma_data$Ethnicity <- as.integer(asthma_data$Ethnicity)
asthma_data$EducationLevel <- as.integer(asthma_data$EducationLevel)
asthma_data$Smoking <- as.integer(asthma_data$Smoking)
asthma_data$PetAllergy <- as.integer(asthma_data$PetAllergy)
asthma_data$FamilyHistoryAsthma <- as.integer(asthma_data$FamilyHistoryAsthma)
asthma_data$HistoryOfAllergies <- as.integer(asthma_data$HistoryOfAllergies)
asthma_data$Eczema <- as.integer(asthma_data$Eczema)
asthma_data$HayFever <- as.integer(asthma_data$HayFever)
asthma_data$GastroesophagealReflux <- as.integer(asthma_data$GastroesophagealReflux)
asthma_data$Wheezing <- as.integer(asthma_data$Wheezing)
asthma_data$ShortnessOfBreath <- as.integer(asthma_data$ShortnessOfBreath)
asthma_data$ChestTightness <- as.integer(asthma_data$ChestTightness)
asthma_data$Coughing <- as.integer(asthma_data$Coughing)
asthma_data$NighttimeSymptoms <- as.integer(asthma_data$NighttimeSymptoms)
asthma_data$ExerciseInduced <- as.integer(asthma_data$ExerciseInduced)
asthma_data$Diagnosis <- as.integer(asthma_data$Diagnosis)


#Replace NA values to the column's mode
calculate_mode <- function(x) {
  x <- na.omit(x)
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}
asthma_data$Gender[is.na(asthma_data$Gender)] <- calculate_mode(asthma_data$Gender)
asthma_data$Ethnicity[is.na(asthma_data$Ethnicity)] <- calculate_mode(asthma_data$Ethnicity)
asthma_data$EducationLevel[is.na(asthma_data$EducationLevel)] <- calculate_mode(asthma_data$EducationLevel)
asthma_data$Smoking[is.na(asthma_data$Smoking)] <- calculate_mode(asthma_data$Smoking)
asthma_data$PetAllergy[is.na(asthma_data$PetAllergy)] <- calculate_mode(asthma_data$PetAllergy)
asthma_data$FamilyHistoryAsthma[is.na(asthma_data$FamilyHistoryAsthma)] <- calculate_mode(asthma_data$FamilyHistoryAsthma)
asthma_data$HistoryOfAllergies[is.na(asthma_data$HistoryOfAllergies)] <- calculate_mode(asthma_data$HistoryOfAllergies)
asthma_data$Eczema[is.na(asthma_data$Eczema)] <- calculate_mode(asthma_data$Eczema)
asthma_data$HayFever[is.na(asthma_data$HayFever)] <- calculate_mode(asthma_data$HayFever)
asthma_data$GastroesophagealReflux[is.na(asthma_data$GastroesophagealReflux)] <- calculate_mode(asthma_data$GastroesophagealReflux)
asthma_data$Wheezing[is.na(asthma_data$Wheezing)] <- calculate_mode(asthma_data$Wheezing)
asthma_data$ShortnessOfBreath[is.na(asthma_data$ShortnessOfBreath)] <- calculate_mode(asthma_data$ShortnessOfBreath)
asthma_data$ChestTightness[is.na(asthma_data$ChestTightness)] <- calculate_mode(asthma_data$ChestTightness)
asthma_data$Coughing[is.na(asthma_data$Coughing)] <- calculate_mode(asthma_data$Coughing)
asthma_data$NighttimeSymptoms[is.na(asthma_data$NighttimeSymptoms)] <- calculate_mode(asthma_data$NighttimeSymptoms)
asthma_data$ExerciseInduced[is.na(asthma_data$ExerciseInduced)] <- calculate_mode(asthma_data$ExerciseInduced)
asthma_data$Diagnosis[is.na(asthma_data$Diagnosis)] <- calculate_mode(asthma_data$Diagnosis)

# Convert specific columns to factors and transform into Categorical Values with labels given
asthma_data$Gender <- factor(asthma_data$Gender, levels = c(0, 1), labels = c("Male", "Female"))
asthma_data$Ethnicity <- factor(asthma_data$Ethnicity, levels = c(0, 1, 2, 3), labels = 
                                  c("Caucasian", "African American", "Asian", "Other"))
asthma_data$EducationLevel <- factor(asthma_data$EducationLevel, levels = c(0, 1, 2, 3), labels = 
                                       c("None", "High School", "Bachelor's", "Higher"))
asthma_data$Smoking <- factor(asthma_data$Smoking, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$PetAllergy <- factor(asthma_data$PetAllergy, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$FamilyHistoryAsthma <- factor(asthma_data$FamilyHistoryAsthma, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$HistoryOfAllergies <- factor(asthma_data$HistoryOfAllergies, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$Eczema <- factor(asthma_data$Eczema, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$HayFever <- factor(asthma_data$HayFever, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$GastroesophagealReflux <- factor(asthma_data$GastroesophagealReflux, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$Wheezing <- factor(asthma_data$Wheezing, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$ShortnessOfBreath <- factor(asthma_data$ShortnessOfBreath, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$ChestTightness <- factor(asthma_data$ChestTightness, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$Coughing <- factor(asthma_data$Coughing, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$NighttimeSymptoms <- factor(asthma_data$NighttimeSymptoms, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$ExerciseInduced <- factor(asthma_data$ExerciseInduced, levels = c(0, 1), labels = c("No", "Yes"))
asthma_data$Diagnosis <- factor(asthma_data$Diagnosis, levels = c(0, 1), labels = c("No", "Yes"))

summary(asthma_data)
