library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

# Additional Analysis

# Create subset
sub_data = subset(asthma_data,select=c("Gender","PollenExposure","LungFunctionFEV1","LungFunctionFVC","Diagnosis"))

# Rename
names(sub_data) = c("Sex","ExposedtoPollen","FEV","FVC","Infection")

# Female patients with asthma
Female = sub_data[(sub_data$Sex == "Female")&(sub_data$Infection == "Yes"),]
nrow(Female) #4937

# 4937
#Female patients, FVC<=5.1, have asthma
FnFVC = sub_data[(sub_data$Sex == "Female")&(sub_data$FVC <= 5.1)&(sub_data$Infection == "Yes"),]
nrow(FnFVC) #4317
# more than half

# 4317
#Female patients, FVC<=5.1, FEV<=3.5, have asthma
FnFEVnFVC = filter(sub_data, Sex=="Female" & FEV<=3.5 & FVC <= 5.1 & Infection=="Yes")
nrow(FnFEVnFVC) #3978
#more than half

# 3978
#Female patients, FVC<=5.1, FEV<=3.5, Pollen Exposure>=2.5, have asthma
All = filter(sub_data, Sex=="Female" & FEV<=3.5 & FVC<=5.1 & ExposedtoPollen>=2.5 & Infection=="Yes")
nrow(All) #3466

(3466/4937)*100
# more than half



# VISUALIZATION

# PIE CHART

FEV = filter(sub_data, Sex=="Female" & FEV<=3.5 & Infection=="Yes")
FVC = filter(sub_data, Sex=="Female" & FVC<=5.1 & Infection=="Yes")
Pollen = filter(sub_data, Sex=="Female" & ExposedtoPollen>=2.5 & Infection=="Yes")
a = c(nrow(FEV),nrow(FVC),nrow(Pollen))
b = c("FEV","FVC","Pollen Exposure")
pie(a,b,radius=1,main="Female patients",col=c("pink","blue","orange"),clockwise = TRUE)

Male = filter(sub_data, Sex=="Male" & Infection=="Yes")
Female = filter(sub_data, Sex=="Female" & Infection=="Yes")
c = c(nrow(Male),nrow(Female))
d = c("Male Patients","Female Patients")
pie(c,d,radius=1,main="Asthma patients",col=c("lightblue","pink"),clockwise = TRUE)


# HISTOGRAM

sub_data$Sex = as.factor(sub_data$Sex)
sub_data = sub_data[sub_data$Infection == "Yes",]
sub_data = sub_data[sub_data$Sex == "Female",]

ggplot(sub_data,aes(x=FEV)) + 
  geom_histogram(colour = "orange",aes(fill = ..count..)) +
  scale_fill_gradient("count",low="green",high="red") +
  ggtitle("FEV Histogram")


ggplot(sub_data,aes(x=FVC)) + 
  geom_histogram(colour = "orange",aes(fill = ..count..)) +
  scale_fill_gradient("count",low="green",high="red") +
  ggtitle("FVC Histogram")


ggplot(sub_data,aes(x=ExposedtoPollen)) + 
  geom_histogram(colour = "orange",aes(fill = ..count..)) +
  scale_fill_gradient("count",low="green",high="red") +
  ggtitle("Pollen Exposure Histogram")


# BOXPLOT

sub_data$Sex = as.factor(sub_data$Sex)
sub_data = sub_data[sub_data$Infection == "Yes",]

ggplot(sub_data, aes(x = Sex, y = FEV)) +
  geom_boxplot(aes(fill = Sex), color = "black") +
  scale_fill_manual(values = c("blue", "pink")) + 
  labs(title = "FEV Boxplot", x = "Sex", y = "FEV")

ggplot(sub_data, aes(x = Sex, y = FVC)) +
  geom_boxplot(aes(fill = Sex), color = "black") +
  scale_fill_manual(values = c("blue", "pink")) + 
  labs(title = "FVC Boxplot", x = "Sex", y = "FVC")

ggplot(sub_data, aes(x = Sex, y = ExposedtoPollen)) +
  geom_boxplot(aes(fill = Sex), color = "black") +
  scale_fill_manual(values = c("blue", "pink")) + 
  labs(title = "Pollen Exposure Boxplot", x = "Sex", y = "Pollen Exposure")


# CLUSTERED BAR GRAPH

sub_data$FEV_category <- cut(sub_data$FEV, breaks = 3, labels = c("Low", "Medium", "High"))
ggplot(sub_data, aes(x = factor(Sex), fill = FEV_category)) + 
  geom_bar() +
  labs(x = "Sex", fill = "FEV categories") +
  ggtitle("FEV Clustered Bar Graph")

sub_data$FVC_category <- cut(sub_data$FVC, breaks = 3, labels = c("Low", "Medium", "High"))
ggplot(sub_data, aes(x = factor(Sex), fill = FVC_category)) + 
  geom_bar() +
  labs(x = "Sex", fill = "FVC categories") +
  ggtitle("FVC Clustered Bar Graph")

sub_data$Pollen_category <- cut(sub_data$ExposedtoPollen, breaks = 3, labels = c("Low", "Medium", "High"))
ggplot(sub_data, aes(x = factor(Sex), fill = Pollen_category)) + 
  geom_bar() +
  labs(x = "Sex", fill = "Pollen Exposure Levels") +
  ggtitle("Pollen Exposure Clustered Bar Graph")


# DENSITY GRAPH

ggplot(sub_data, aes(x = FEV, fill = factor(Sex))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("lightgreen", "violet")) +
  labs(x = "FEV", fill = "Sex") +
  ggtitle("FEV Density Graph")

ggplot(sub_data, aes(x = FVC, fill = factor(Sex))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("lightgreen", "violet")) +
  labs(x = "FVC", fill = "Sex") +
  ggtitle("FVC Density Graph")

ggplot(sub_data, aes(x = ExposedtoPollen, fill = factor(Sex))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("lightgreen", "violet")) +
  labs(x = "Pollen Exposure", fill = "Sex") +
  ggtitle("Pollen Exposure Density Graph")


# VIOLIN GRAPH

ggplot(sub_data, aes(x = Sex, y = FEV, fill = Sex)) + 
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1) +
  ggtitle("FEV Violin Graph")

ggplot(sub_data, aes(x = Sex, y = FVC, fill = Sex)) + 
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1) +
  ggtitle("FVC Violin Graph")

ggplot(sub_data, aes(x = Sex, y = ExposedtoPollen, fill = Sex)) + 
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1) +
  ggtitle("Pollen Exposure Violin Graph")

summary(asthma_data)


# LOLLIPOP GRAPH

sub_data <- sub_data %>%
  filter(Sex == "Female") %>%
  mutate(FEV_category = cut(FEV, 
                            breaks = 3, 
                            labels = c("Low", "Medium", "High")),
         FVC_category = cut(FVC, 
                            breaks = 3, 
                            labels = c("Low", "Medium", "High")),
         PollenExposure_category = cut(ExposedtoPollen, 
                                       breaks = 3, 
                                       labels = c("Low", "Medium", "High")))

category_data <- sub_data %>%
  group_by(Infection) %>%
  summarise(FEV_Low = sum(FEV_category == "Low"),
            FEV_Medium = sum(FEV_category == "Medium"),
            FEV_High = sum(FEV_category == "High"),
            FVC_Low = sum(FVC_category == "Low"),
            FVC_Medium = sum(FVC_category == "Medium"),
            FVC_High = sum(FVC_category == "High"),
            Pollen_Low = sum(PollenExposure_category == "Low"),
            Pollen_Medium = sum(PollenExposure_category == "Medium"),
            Pollen_High = sum(PollenExposure_category == "High"))

category_data_long <- category_data %>%
  pivot_longer(cols = -Infection, names_to = c("Variable", "Category"), names_sep = "_", values_to = "Count")

ggplot(category_data_long, aes(x = Category, y = Count, color = Infection, group = Infection)) +
  geom_segment(aes(x = Category, xend = Category, y = 0, yend = Count), size = 1) +
  geom_point(size = 5) +
  facet_wrap(~ Variable) +
  theme_minimal() +
  labs(title = "Lollipop Graph for FEV, FVC, and Pollen Exposure Categories",
       x = "Category", y = "Number of female patients") +
  theme(legend.position = "bottom")


# POLYNOMIAL

# Fit a quadratic polynomial regression model (degree 2)
poly_model <- lm(FEV ~ poly(FVC, 2) + Infection, data = sub_data)

# Summarize the model
summary(poly_model)

# Create the visualization
ggplot(sub_data, aes(x = FVC, y = FEV, color = as.factor(Infection))) +
  geom_point(alpha = 0.4, position = position_jitter(width = 0.1)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +  # Polynomial line
  labs(title = "Polynomial Regression: FEV vs FVC, Infection",
       x = "FVC",
       y = "FEV") +
  facet_wrap(~ as.factor(Infection), ncol = 2) +
  theme_minimal()


# SCATTERPLOT

# Categorize FVC into three levels
sub_data$FVC_category <- cut(sub_data$FVC, breaks = 3, labels = c("Low", "Medium", "High"))

# Filter for female patients with infection == "Yes"
female_data <- sub_data %>%
  filter(Sex == "Female", Infection == "Yes")

# Calculate the count of female patients in each pollen exposure category
count_data <- female_data %>%
  group_by(FVC_category) %>%
  summarise(Count = n(), .groups = 'drop')

# Create the scatter plot with Pollen_category on the x-axis and FVC_category on the y-axis for female patients with infection == "Yes"
ggplot(female_data, aes(x = ExposedtoPollen, y = FVC_category)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.2, height = 0.2), color = "blue") +
  geom_text(data = count_data, aes(x = max(female_data$FVC), y = FVC_category, label = Count), 
            vjust = -2, hjust = 5, color = "black", size = 4) +
  
  labs(
    title = "Scatter Plot of FVC Category by Pollen Exposure for Female Asthmatic Patient",
    x = "Pollen Exposure",
    y = "FVC Category"
  ) +
  theme_minimal()


# Categorize FEV into three levels
sub_data$FEV_category <- cut(sub_data$FEV, breaks = 3, labels = c("Low", "Medium", "High"))

# Filter for female patients with infection == "Yes"
female_data <- sub_data %>%
  filter(Sex == "Female", Infection == "Yes")

# Calculate the count of female patients in each FEV category
count_data <- female_data %>%
  group_by(FEV_category) %>%
  summarise(Count = n(), .groups = 'drop')

# Create the scatter plot with ExposedtoPollen on the x-axis and FEV_category on the y-axis for female patients with infection == "Yes"
ggplot(female_data, aes(x = ExposedtoPollen, y = FEV_category)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.2, height = 0.2), color = "blue") +
  geom_text(data = count_data, aes(x = max(female_data$FEV), y = FEV_category, label = Count), 
            vjust = -2, hjust = 5, color = "black", size = 4) +
  labs(
    title = "Scatter Plot of FEV Category by Pollen Exposure for Female Asthmatic Patients",
    x = "Pollen Exposure",
    y = "FEV Category"
  ) +
  theme_minimal()


# HEATMAP

# Categorize pollen exposure levels
sub_data$Pollen_category <- cut(sub_data$ExposedtoPollen, breaks = 3, 
                                labels = c("Low", "Medium", "High"))
# Categorize FEV into three levels
sub_data$FEV_category <- cut(sub_data$FEV, breaks = 3, labels = c("Low", "Medium", "High"))

# Summarize the count of Pollen Category across FEV categories
heatmap_data <- female_data %>%
  group_by(FEV_category, Pollen_category) %>%
  summarise(Count = n(), .groups = 'drop')

# Create heatmap
ggplot(heatmap_data, aes(x = Pollen_category, y = FEV_category, fill = Count)) +
  geom_tile() +  # Creates the heatmap
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale for the count
  labs(
    title = "Heatmap of Pollen Category and FEV Categories for Female Asthmatic Patients",
    x = "Pollen Category",
    y = "FEV Category",
    fill = "Count"
  ) +
  theme_minimal()



# Categorize pollen exposure levels
sub_data$Pollen_category <- cut(sub_data$ExposedtoPollen, breaks = 3, 
                                labels = c("Low", "Medium", "High"))
# Categorize FVC into three levels
sub_data$FVC_category <- cut(sub_data$FVC, breaks = 3, labels = c("Low", "Medium", "High"))

# Filter for female patients with infection == "Yes"
female_data <- sub_data %>%
  filter(Sex == "Female", Infection == "Yes")

# Summarize the count of ExposedtoPollen across FVC categories
heatmap_data <- female_data %>%
  group_by(FVC_category, Pollen_category) %>%
  summarise(Count = n(), .groups = 'drop')

# Create heatmap
ggplot(heatmap_data, aes(x = Pollen_category, y = FVC_category, fill = Count)) +
  geom_tile() +  # Creates the heatmap
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale for the count
  labs(
    title = "Heatmap of Pollen Category and FVC Categories for Female Asthmatic Patients",
    x = "Pollen Category",
    y = "FVC Category",
    fill = "Count"
  ) + theme_minimal()


# Categorize FEV into three levels
sub_data$FEV_category <- cut(sub_data$FEV, breaks = 3, labels = c("Low", "Medium", "High"))

# Categorize FVC into three levels
sub_data$FVC_category <- cut(sub_data$FVC, breaks = 3, labels = c("Low", "Medium", "High"))

# Filter for female patients with infection == "Yes"
female_data <- sub_data %>%
  filter(Sex == "Female", Infection == "Yes")

# Summarize the count of patients across FEV and FVC categories
heatmap_data <- female_data %>%
  group_by(FEV_category, FVC_category) %>%
  summarise(Count = n(), .groups = 'drop')

# Create heatmap
ggplot(heatmap_data, aes(x = FEV_category, y = FVC_category, fill = Count)) +
  geom_tile() +  # Creates the heatmap
  scale_fill_gradient(low = "white", high = "blue") +  # Color scale for the count
  labs(
    title = "Heatmap of FEV and FVC Categories for Female Asthmatic Patients",
    x = "FEV Category",
    y = "FVC Category",
    fill = "Count"
  ) + -theme_minimal()
