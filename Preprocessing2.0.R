names(asthma_data)[2] = "Sex"
names(asthma_data)[6] = "Smking"
names(asthma_data)[14] = "AsthmaHereditary" 
names(asthma_data)[11] = "ExposedtoPollen"
names(asthma_data)[27] = "Infection"

#2.Create subset
cleaned_data = asthma_data[, c("Sex", "Smking", "AsthmaHereditary", "ExposedtoPollen","Infection")]
cleaned_data

cleaned_data = asthma_data

#different viewing samples
cleaned_data
View(cleaned_data)

#head
head(cleaned_data, 20)

#tails
tail(cleaned_data, 20)

#column names
names(cleaned_data)

#data structures
str(cleaned_data)

#no: of rows and columns
dim(cleaned_data)

#summary
summary(cleaned_data)


