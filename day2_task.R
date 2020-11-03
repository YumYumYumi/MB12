Eprod <- read.csv("data/day2_data_energy_prod_EU_2020-08-03_2020-08-09.csv")
head(Eprod)
DE_TenneT_GER <- Eprod[Eprod$MapCode == "DE_TenneT_GER",] #6400
DE_Amprion <- Eprod[Eprod$MapCode == "DE_Amprion" ,] #11400
DE_50HzT <- Eprod[Eprod$MapCode == "DE_50HzT" ,] #6000
DE_TransnetBW <- Eprod[Eprod$MapCode == "DE_TransnetBW" ,] # 3000
unique(Eprod$MapCode)
unique(Eprod$ProductionTypeName)

#1 
DE_Amp_ProductionType<- as.data.frame(table(DE_Amprion$ProductionTypeName))
DE_Amp_ProductionType$Type <- DE_Amp_ProductionType$Var1
library(ggpubr)
ggpie(
  DE_Amp_ProductionType, x = "Freq", label = "Freq",
  lab.pos = "in", lab.font = list(color = "white"), 
  fill = "Type", color = "white",
  palette = "rainbow"
)

#2
DE_TenneT_ProductionType<- as.data.frame(table(DE_TenneT_GER$ProductionTypeName))
DE_TenneT_ProductionType$Type <- DE_TenneT_ProductionType$Var1
ggpie(
  DE_TenneT_ProductionType, x = "Freq", label = "Freq",
  lab.pos = "in", lab.font = list(color = "white"), 
  fill = "Type", color = "white",
  palette = "rainbow"
)


#3
DE_50HzT <- Eprod[Eprod$MapCode == "DE_50HzT" ,] #6000
DE_50HzT_ProductionType<- as.data.frame(table(DE_50HzT$ProductionTypeName))
DE_50HzT_ProductionType$Type <- DE_50HzT_ProductionType$Var1
ggpie(
  DE_50HzT_ProductionType, x = "Freq", label = "Freq",
  lab.pos = "in", lab.font = list(color = "white"), 
  fill = "Type", color = "white",
  palette = "rainbow"
)

#4
DE_TransnetBW_ProductionType<- as.data.frame(table(DE_TransnetBW$ProductionTypeName))
DE_TransnetBW_ProductionType$Type <- DE_TransnetBW_ProductionType$Var1
ggpie(
  DE_TransnetBW_ProductionType, x = "Freq", label = "Freq",
  lab.pos = "in", lab.font = list(color = "white"), 
  fill = "Type", color = "white",
  palette = "rainbow"
)


############################## from the course 
#load
df <- read.csv("data/day2_data_energy_prod_EU_2020-08-03_2020-08-09.csv")

#summary
summary(df)
dim(df)
colnames(df)

#categorical variables 
unique(df$MapCode)
unique(df$GenerationUnitEIC)
length(unique(df$GenerationUnitEIC))

#actual vs. installed capacity
plot(x = df$InstalledGenCapacity, y = df$ActualGenerationOutput)

max(df$ActualGenerationOutput, na.rm = T)
df[which.max(df$ActualGenerationOutput), ]
df <- df[-which.max(df$ActualGenerationOutput), ]

plot(x = df$InstalledGenCapacity, y = df$ActualGenerationOutput)
df <- df[-which.max(df$ActualGenerationOutput), ]
plot(x = df$InstalledGenCapacity, y = df$ActualGenerationOutput)
abline(coef = c(0,1), col="red")

#create counts of Production Type 
counts <- table(df$ProductionTypeName)

#aggregate Generation by day
prod_by_day <- aggregate(df$ActualGenerationOutput, 
                         by = list(Day = df$Day), 
                         FUN = sum, 
                         na.rm = T)

prod_by_day$x <- prod_by_day$x * 0.001 # giga watt 
plot(x = prod_by_day$Day, y = prod_by_day$x)


cap_by_day <- aggregate(df$InstalledGenCapacity, 
                         by = list(Day = df$Day), 
                         FUN = sum, 
                         na.rm = T)

cap_by_day$x <- cap_by_day$x * 0.001 # giga watt 
plot(x = cap_by_day$Day, y = cap_by_day$x)


########################################################################

DE_Amprion <- Eprod[Eprod$MapCode == "DE_Amprion" ,] #11400 
apply(DE_Amprion$ProductionTypeName, 2, function(c)sum(c!=0))
plot()
plot(Eprod1$ActualConsumption, Eprod1$ActualGenerationOutput)

Eprod1 <- na.omit(Eprod)
library(dplyr)
Eprod1 <- filter(Eprod1, ActualGenerationOutput> 0, Eprod1$ActualConsumption > 0)
plot(Eprod1$ActualGenerationOutput, Eprod1$ActualConsumption)