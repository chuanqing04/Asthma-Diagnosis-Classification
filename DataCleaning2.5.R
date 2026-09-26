sample_data = read.csv("asthma_dataset.csv", header = TRUE)

#create subset
sub_data = subset(sample_data)

#rename
names(sub_data) = c("Age","Sex","Ethnicity","EducationLevel","BMI","Smoking","PhysicalActivity","DQ","SQ","ExposedtoPollution","ExposedtoPollen","ExposedtoDust","PetAllergy","AsthmaHereditary","AsthmaHistory","Eczema","HayFever","GastroephagealReflux","FEV","FVC","Wheezing","SOB","CT","Coughing","NighttimeSyndrome","ExerciseInduced","Infection")

#remove special characters
sub_data$Sex = gsub("[?_]","",sub_data$Sex)
sub_data$FVC = gsub("[?_]","",sub_data$FVC)
sub_data$ExposedtoPollen = gsub("[?_]","",sub_data$ExposedtoPollen)
sub_data$FEV = gsub("[?_]","",sub_data$FEV)

#change datatype to round up
sub_data$Sex = round(as.numeric(sub_data$Sex))
sub_data$Infection = round(as.numeric(sub_data$Infection))

#change datatype
sub_data$Sex = as.integer(sub_data$Sex)
sub_data$Infection = as.integer(sub_data$Infection)
sub_data$FVC = as.numeric(sub_data$FVC)
sub_data$ExposedtoPollen = as.numeric(sub_data$ExposedtoPollen)
sub_data$FEV = as.numeric(sub_data$FEV)

#mode imputation
get_mode <- function(v) 
{  
  uniqv <- unique(v)  
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
sub_data$Sex[is.na(sub_data$Sex)] = get_mode(sub_data$Sex)
sub_data$Infection[is.na(sub_data$Infection)] = get_mode(sub_data$Infection)

#mean
sub_data$ExposedtoPollen[is.na(sub_data$ExposedtoPollen)] = mean(sub_data$ExposedtoPollen, na.rm = TRUE)
sub_data$FVC[is.na(sub_data$FVC)] = mean(sub_data$FVC, na.rm = TRUE)
sub_data$FEV[is.na(sub_data$FEV)] = mean(sub_data$FEV, na.rm = TRUE)

summary(sub_data)
str(sub_data)
View(sub_data)