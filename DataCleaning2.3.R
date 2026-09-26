sample_data = read.csv("asthma_dataset.csv", header = TRUE)

#create subset
sub_data4 = subset(sample_data,select=c("Gender","Smoking","PollenExposure","FamilyHistoryAsthma","Diagnosis"))

#rename
names(sub_data4) = c("Sex","Smking","ExposedtoPollen","AsthmaHereditary","Infection")

#remove special characters
sub_data4$Sex = gsub("[?_]","",sub_data4$Sex)
sub_data4$Smking = gsub("[?_]","",sub_data4$Smking)
sub_data4$ExposedtoPollen = gsub("[?_]","",sub_data4$ExposedtoPollen)
sub_data4$AsthmaHereditary = gsub("[?_]","",sub_data4$AsthmaHere)

#change datatype to round up
sub_data4$Sex = round(as.numeric(sub_data4$Sex))
sub_data4$Smking = round(as.numeric(sub_data4$Smking))
sub_data4$ExposedtoPollen = round(as.numeric(sub_data4$ExposedtoPollen))
sub_data4$AsthmaHereditary = round(as.numeric(sub_data4$AsthmaHereditary))
sub_data4$Infection = round(as.numeric(sub_data4$Infection))

#change datatype
sub_data4$Sex = as.integer(sub_data4$Sex)
sub_data4$Smking = as.integer(sub_data4$Smking)
sub_data4$AsthmaHereditary = as.integer(sub_data4$AsthmaHereditary)
sub_data4$Infection = as.integer(sub_data4$Infection)

#mode imputation
get_mode <- function(v) 
{  
  uniqv <- unique(v)  
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
sub_data4$Sex[is.na(sub_data4$Sex)] = get_mode(sub_data4$Sex)
sub_data4$Smking[is.na(sub_data4$Smking)] = get_mode(sub_data4$Smking)
sub_data4$AsthmaHereditary[is.na(sub_data4$AsthmaHereditary)] = get_mode(sub_data4$AsthmaHereditary)
sub_data4$Infection[is.na(sub_data4$Infection)] = get_mode(sub_data4$Infection)

#mean
sub_data4$ExposedtoPollen[is.na(sub_data4$ExposedtoPollen)] = mean(sub_data4$ExposedtoPollen, na.rm = TRUE)

summary(sub_data4)
str(sub_data4)
View(sub_data4)