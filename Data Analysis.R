# Objective 3 : To find out if having family history of asthma will increase the likelihood of being diagnosed with asthma - Teo Chuan Qing TP070483

library(dplyr)
library(ggplot2)

# Analysis 3-1
# list out all the details of patients who have asthma
PInfection=asthma_data[asthma_data$Diagnosis == "Yes", ] 
nrow(PInfection)

ggplot(PInfection, aes(x = Diagnosis)) +
  geom_bar(width = 0.5, fill = "lightblue") +
  labs(title = "Number of Patients with Asthma Diagnosis",
       x = "Asthma Diagnosis",
       y = "Count") +
  theme_minimal()


# 8968

# Analysis 3-2
# all patients who have family history of asthma and asthma
FH = asthma_data[(asthma_data$FamilyHistoryAsthma == "Yes")&(asthma_data$Diagnosis == "Yes"),]
nrow(FH) #2440
# less than half

ggplot(FH, aes(x = FamilyHistoryAsthma)) +
  geom_bar(width = 0.5, fill = "steelblue") +
  labs(title = "Number of Patients with Family History and Asthma Diagnosis",
       x = "Family History of Asthma",
       y = "Count") +
  theme_minimal()

# 6528

# Analysis 3-3
# all patients who are male, don't have family history of asthma and asthma
MnFH = asthma_data[(asthma_data$Gender == "Male")&(asthma_data$FamilyHistoryAsthma == "No")&
                     (asthma_data$Diagnosis == "Yes"),]
nrow(MnFH) #3155
# less than half

ggplot(asthma_data, aes(x = "", fill = Gender)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("lightblue", "pink")) +  
  geom_text(aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1)),
            stat = "count", position = position_stack(vjust = 0.5)) + 
  labs(fill = "Gender") +
  ggtitle("Patients without Family History of Asthma") +
  theme_void()

# 3373

# Analysis 3-4
# all patients who are female, smokes, don't have family history of asthma and asthma
MnSnFH = filter(asthma_data, Gender == "Female" & Smoking == "Yes" & FamilyHistoryAsthma == "No" & Diagnosis == "Yes")
nrow(MnSnFH) #262
# less than half

Ysmoke = nrow(filter(asthma_data, Gender == "Female" & Smoking == "Yes" & FamilyHistoryAsthma == "No" & Diagnosis == "Yes"))
Nsmoke = nrow(filter(asthma_data, Gender == "Female" & Smoking == "No" & FamilyHistoryAsthma == "No" & Diagnosis == "Yes"))
a=c(Ysmoke, Nsmoke)
b=c("Smokes","Does not smokes")
percentages <- round(a / sum(a) * 100, 1)
labels <- paste(b, "\n", a, "\n(", percentages, "%)", sep = "")
pie(a, labels = labels, radius = 1, main = "Smoking habits", 
    col = c("red", "lightgreen"), clockwise = TRUE)


# 3111

# Analysis 3-5
# all patients who are female, don't smokes, don't have family history of asthma and asthma, Pollen Exposure of >=5
all = filter(asthma_data, Gender == "Female" & Smoking == "No" &
               FamilyHistoryAsthma == "No" & PollenExposure >= 5 & Diagnosis == "Yes")
nrow(all) #1675
# more than half

filtered_data <- filter(asthma_data, 
                        Gender == "Female" & 
                          Smoking == "No" & 
                          FamilyHistoryAsthma == "No" & 
                          Diagnosis == "Yes")
ggplot(filtered_data,aes(x=PollenExposure)) + 
  geom_histogram(colour = "orange",aes(fill = ..count..)) +
  scale_fill_gradient("Number of patients",low="green",high="red") +
  ggtitle("Pollen Exposure Histogram") +
  labs(x = "Pollen Exposure", y = "Number of Patients")

