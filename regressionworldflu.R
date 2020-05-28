setwd("~/R Projects/Maps")
source("preprocessworldflu.R")
set.seed(123)

library(dplyr)
library(caTools)
library(ggplot2)
library(gridExtra)

set.season <- function(data){
  data <- as.character(data)
  
  if (is.na(data)){
    data <- NA
  } else if(data == "March" | data == "April" | data == "May"){
    data <- "Spring"
  } else if (data == "June" | data == "July" | data == "August"){
    data <- "Summer"
  } else if (data == "September" | data == "October" | data == "November"){
    data <- "Autumn"
  } else if (data == "December" | data == "January" | data == "February"){
    data <- "Winter"
  } 
  
  return(data)
}
regressorpercountry <- function(df, country){
  df <- df %>% filter(Country == country) %>% select(-c(Country, WHOREGION, FLUREGION, Date, INF_A))
  df <- df %>% group_by(Year, ILI, Season) %>% summarise(Incidence = mean(Incidence), GDP = mean(GDP))
  df <- as.data.frame(df)
  
  df <- df %>% mutate(Year = as.factor(Year), 
                      Year = factor(Year, levels = levels(Year), labels = seq(1, length(levels(Year)))),
                      Season = as.factor(Season), 
                      Season = factor(Season, levels = levels(Season), labels = seq(1, length(levels(Season)))),
                      ILI = factor(ILI, levels = levels(ILI), labels = seq(1, length(levels(ILI)))))
  
  return(df)
}
regressorperwhoregion <- function(df, whoregion){
  df <- df %>% filter(WHOREGION == whoregion) %>% select(-c(WHOREGION, Date, INF_A))
  df <- df %>% group_by(Country, FLUREGION, Year, ILI, Season) %>% summarise(Incidence = mean(Incidence, na.rm = TRUE), Population = mean(Population, na.rm = TRUE), GDP = mean(GDP, na.rm = TRUE))
  df <- as.data.frame(df)
  
  df <- df %>% mutate(Year = as.factor(Year), 
                      Year = factor(Year, levels = levels(Year), labels = seq(1, length(levels(Year)))),
                      Country = as.factor(Country), 
                      Country = factor(Country, levels = levels(Country), labels = seq(1, length(levels(Country)))),
                      FLUREGION = as.factor(FLUREGION), 
                      FLUREGION = factor(FLUREGION, levels = levels(FLUREGION), labels = seq(1, length(levels(FLUREGION)))),
                      Season = as.factor(Season), 
                      Season = factor(Season, levels = levels(Season), labels = seq(1, length(levels(Season)))),
                      ILI = factor(ILI, levels = levels(ILI), labels = seq(1, length(levels(ILI)))))
  
  return(df)
}
sv_img <- function(datatoimage, filename, ...){
  library(png)
  library(svglite)
  library(rsvg)
  svglite("image.svg", ...)
  datatoimage
  dev.off()
  
  bitmap <- rsvg("image.svg", width = 3840)
  png::writePNG(bitmap, target = paste(filename, ".png", sep=""), dpi = 144)
}


flu <- flu %>% filter(!is.na(Incidence))
flu <- flu %>% mutate(Month = as.Date(Date), Month = months(Month), Season = sapply(Month, set.season))

### Primer regressor linea usando los datos correspondientes a Mexico
mexico <- regressorpercountry(flu, country = "Mexico")

split <- sample.split(mexico$Incidence, SplitRatio = 0.8)

training_set <- subset(mexico %>% filter(!is.na(GDP)), split == TRUE)
test_set <- subset(mexico %>% filter(!is.na(GDP)), split == FALSE)

# Primera iteracion del regressor de Mexico
regressor = lm(formula = Incidence ~ .,
               data = training_set)
summary(regressor)

# Segunda iteracion del regressor de Mexico
regressor = lm(formula = Incidence ~ Year + ILI + Season,
               data = training_set)
summary(regressor)

# Tercera iteracion del regressor de Mexico
regressor = lm(formula = Incidence ~ ILI + Season,
               data = training_set)
summary(regressor)

# Cuarta iteracion del regressor de Mexico
regressor = lm(formula = Incidence ~ Season,
               data = training_set)
summary(regressor)

# Prediccion del regressor de Mexico  
y_pred = predict(regressor, newdata = test_set)
y_pred

ggplot(cbind(test_set, y_pred)) + aes(x = as.factor(Season)) + 
  geom_point(aes(y = Incidence, size = as.numeric(ILI), color = as.factor(Season))) +
  geom_line(aes(y = y_pred), colour = "red", size = 2) +
  labs(title = "Region of Mexico of WHO",
       caption = "Source: FluNet/WorldBank") + 
  scale_color_discrete(name = "Season", breaks = c("1", "2", "3", "4"), labels=c("Winter", "Spring", "Summer", "Autumn")) + 
  scale_size_continuous(name = "ILI", breaks = c(1:6), labels = c("Local Outbreak", "No Activity", "No Report", "Regional Outbreak", "Sporadic", "Widespread Outbreak")) + 
  theme(plot.title = element_text(colour = "slategray"),
        plot.caption = element_text(colour = "slategray")) + 
  annotate("rect", xmin=35000, xmax=45000, ymin=3.8, ymax=4.2, fill="white", colour="red") +
  annotate("text", x=40000, y=4.0, label = paste("R^2 == ", format(summary(regressor)$adj.r.squared, digits = 4)), parse=T) 

### Segundo regressor lineal usando los datos correspondientes de America 
america <- regressorperwhoregion(flu, whoregion = "Region of the Americas of WHO")

split <- sample.split(america$Incidence, SplitRatio = 0.8)

training_set <- subset(america %>% filter(!is.na(GDP)), split == TRUE)
test_set <- subset(america %>% filter(!is.na(GDP)), split == FALSE)

# Primera iteracion del regressor de America 
regressor = lm(formula = Incidence ~ .,
               data = training_set)
summary(regressor)

# Segunda iteracion del regressor de America 
regressor = lm(formula = Incidence ~ Country + Year + ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Tercera iteracion del regressor de America 
regressor = lm(formula = Incidence ~ Country + ILI + Season  + GDP,
               data = training_set)
summary(regressor)

# Cuarta iteracion del regressor de America 
regressor = lm(formula = Incidence ~ ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Quinta iteracion del regressor de America 
regressor = lm(formula = Incidence ~ Season + GDP,
               data = training_set)
summary(regressor)

# Sexta iteracion del regressor de America 
regressor = lm(formula = Incidence ~ GDP,
               data = training_set)
summary(regressor)

format(summary(regressor)$adj.r.squared, digits = 4)

# Prediccion del regressor de America  
y_pred = predict(regressor, newdata = test_set)
y_pred

sv_img(grid.arrange(ggplot(cbind(test_set, y_pred)) + aes(x = GDP) + 
  geom_point(aes(y = Incidence, size = as.numeric(ILI), color = as.factor(Season))) +
  geom_line(aes(y = y_pred), colour = "red", size = 2) +
  labs(title = "Region of the Americas of WHO",
       caption = "Source: FluNet/WorldBank") + 
  scale_color_discrete(name = "Season", breaks = c("1", "2", "3", "4"), labels=c("Winter", "Spring", "Summer", "Autumn")) + 
  scale_size_continuous(name = "ILI", breaks = c(1:6), labels = c("Local Outbreak", "No Activity", "No Report", "Regional Outbreak", "Sporadic", "Widespread Outbreak")) + 
  theme(plot.title = element_text(colour = "slategray"),
        plot.caption = element_text(colour = "slategray")) + 
  annotate("rect", xmin=32000, xmax=48000, ymin=1.9, ymax=2.1, fill="white", colour="red") +
  annotate("text", x=40000, y=2.0, label = paste("R^2 == ", format(summary(regressor)$adj.r.squared, digits = 4)), parse=T),
  layout_matrix = rbind(c(1,1))), filename = "flu-regressor-americas", width = 18, height = 10)
  
### Tercer regressor usando los datos correspondientes a Europa
europe <- regressorperwhoregion(flu, whoregion = "European Region of WHO")

split <- sample.split(europe$Incidence, SplitRatio = 0.8)

training_set <- subset(europe %>% filter(!is.na(GDP)), split == TRUE)
test_set <- subset(europe %>% filter(!is.na(GDP)), split == FALSE)

# Primera iteracion del regressor de Europa 
regressor = lm(formula = Incidence ~ .,
               data = training_set)
summary(regressor)

# Segunda iteracion del regressor de Europa
regressor = lm(formula = Incidence ~ Country + Year + ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Tercera iteracion del regressor de Europa 
regressor = lm(formula = Incidence ~ Year + ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Cuarta iteracion del regressor de Europa 
regressor = lm(formula = Incidence ~ ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Cuarta iteracion del regressor de Europa 
regressor = lm(formula = Incidence ~ ILI + GDP,
               data = training_set)
summary(regressor)

# Quinta iteracion del regressor de Europa 
regressor = lm(formula = Incidence ~ GDP,
               data = training_set)
summary(regressor)

# Prediccion del regressor de Europa 
y_pred = predict(regressor, newdata = test_set)
y_pred

euro_plot <- ggplot(cbind(test_set, y_pred)) + aes(x = GDP) + 
  geom_point(aes(y = Incidence, size = as.numeric(ILI), color = as.factor(Season))) +
  geom_line(aes(y = y_pred), colour = "red", size = 2) +
  labs(title = "European Region of WHO",
       caption = "Source: FluNet/WorldBank") + 
  scale_color_discrete(name = "Season", breaks = c("1", "2", "3", "4"), labels=c("Winter", "Spring", "Summer", "Autumn")) + 
  scale_size_continuous(name = "ILI", breaks = c(1:6), labels = c("Local Outbreak", "No Activity", "No Report", "Regional Outbreak", "Sporadic", "Widespread Outbreak")) + 
  theme(plot.title = element_text(colour = "slategray"),
        plot.caption = element_text(colour = "slategray")) + 
  annotate("rect", xmin=68000, xmax=91000, ymin=3.6, ymax=4.3, fill="white", colour="red") +
  annotate("text", x=80000, y=4.0, label = paste("R^2 == ", format(summary(regressor)$adj.r.squared, digits = 4)), parse=T)

### Cuarto regressor usando los datos correspondientes al Mediterraneo 
mediterrean <- regressorperwhoregion(flu, whoregion = "Eastern Mediterranean Region of WHO")

split <- sample.split(mediterrean$Incidence, SplitRatio = 0.8)

training_set <- subset(mediterrean %>% filter(!is.na(GDP)), split == TRUE)
test_set <- subset(mediterrean %>% filter(!is.na(GDP)), split == FALSE)

# Primera iteracion del regressor del Mediterraneo 
regressor = lm(formula = Incidence ~ .,
               data = training_set)
summary(regressor)

# Segunda iteracion del regressor del Mediterraneo
regressor = lm(formula = Incidence ~ Country + Year + ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Tercera iteracion del regressor del Mediterraneo
regressor = lm(formula = Incidence ~ Country + ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Cuarta iteracion del regressor del Mediterraneo
regressor = lm(formula = Incidence ~ ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Cuarta iteracion del regressor del Mediterraneo 
regressor = lm(formula = Incidence ~ ILI + GDP,
               data = training_set)
summary(regressor)

# Quinta iteracion del regressor del Mediterraneo
regressor = lm(formula = Incidence ~ GDP,
               data = training_set)
summary(regressor)

# Prediccion del regressor del Mediterraneo 
y_pred = predict(regressor, newdata = test_set)
y_pred

medi_plot <- ggplot(cbind(test_set, y_pred)) + aes(x = GDP) + 
  geom_point(aes(y = Incidence, size = as.numeric(ILI), color = as.factor(Season))) +
  geom_line(aes(y = y_pred), colour = "red", size = 2) +
  labs(title = "Eastern Mediterranean Region of WHO",
       caption = "Source: FluNet/WorldBank") + 
  scale_color_discrete(name = "Season", breaks = c("1", "2", "3", "4"), labels=c("Winter", "Spring", "Summer", "Autumn")) + 
  scale_size_continuous(name = "ILI", breaks = c(1:6), labels = c("Local Outbreak", "No Activity", "No Report", "Regional Outbreak", "Sporadic", "Widespread Outbreak")) + 
  theme(plot.title = element_text(colour = "slategray"),
        plot.caption = element_text(colour = "slategray")) + 
  annotate("rect", xmin=68000, xmax=91000, ymin=3.6, ymax=4.3, fill="white", colour="red") +
  annotate("text", x=80000, y=4.0, label = paste("R^2 == ", format(summary(regressor)$adj.r.squared, digits = 4)), parse=T)

### Quinto regressor usando los datos correspondientes a Asia
asia <- regressorperwhoregion(flu, whoregion = "South-East Asia Region of WHO")

split <- sample.split(asia$Incidence, SplitRatio = 0.8)

training_set <- subset(asia %>% filter(!is.na(GDP)), split == TRUE)
test_set <- subset(asia %>% filter(!is.na(GDP)), split == FALSE)

# Primera iteracion del regressor de Asia
regressor = lm(formula = Incidence ~ .,
               data = training_set)
summary(regressor)

# Segunda iteracion del regressor de Asia
regressor = lm(formula = Incidence ~ Country + Year + ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Tercera iteracion del regressor de Asia
regressor = lm(formula = Incidence ~ Country + Year + Season + GDP,
               data = training_set)
summary(regressor)

# Cuarta iteracion del regressor de Asia
regressor = lm(formula = Incidence ~ Year + Season + GDP,
               data = training_set)
summary(regressor)

# Quinta iteracion del regressor de Asia
regressor = lm(formula = Incidence ~ Season + GDP,
               data = training_set)
summary(regressor)

# Sexta iteracion del regressor de Asia 
regressor = lm(formula = Incidence ~ GDP,
               data = training_set)
summary(regressor)

# Prediccion del regressor de Asia 
y_pred = predict(regressor, newdata = test_set)
y_pred

asia_plot <- ggplot(cbind(test_set, y_pred)) + aes(x = GDP) + 
  geom_point(aes(y = Incidence, size = as.numeric(ILI), color = as.factor(Season))) +
  geom_line(aes(y = y_pred), colour = "red", size = 2) +
  labs(title = "South-East Asia Region of WHO",
       caption = "Source: FluNet/WorldBank") + 
  scale_color_discrete(name = "Season", breaks = c("1", "2", "3", "4"), labels=c("Winter", "Spring", "Summer", "Autumn")) + 
  scale_size_continuous(name = "ILI", breaks = c(1:6), labels = c("Local Outbreak", "No Activity", "No Report", "Regional Outbreak", "Sporadic", "Widespread Outbreak")) + 
  theme(plot.title = element_text(colour = "slategray"),
        plot.caption = element_text(colour = "slategray")) + 
  annotate("rect", xmin=6800, xmax=9100, ymin=3.8, ymax=4.2, fill="white", colour="red") +
  annotate("text", x=8000, y=4.0, label = paste("R^2 == ", format(summary(regressor)$adj.r.squared, digits = 4)), parse=T)

### Sexto regressor usando los datos correspondientes a Africa
africa <- regressorperwhoregion(flu, whoregion = "African Region of WHO")

split <- sample.split(africa$Incidence, SplitRatio = 0.8)

training_set <- subset(africa %>% filter(!is.na(GDP)), split == TRUE)
test_set <- subset(africa %>% filter(!is.na(GDP)), split == FALSE)

# Primera iteracion del regressor de Africa
regressor = lm(formula = Incidence ~ .,
               data = training_set)
summary(regressor)

# Segunda iteracion del regressor de Africa
regressor = lm(formula = Incidence ~ Country + Year + ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Tercera iteracion del regressor de Africa
regressor = lm(formula = Incidence ~ Country + ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Cuarta iteracion del regressor de Africa
regressor = lm(formula = Incidence ~ ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Quinta iteracion del regressor de Africa
regressor = lm(formula = Incidence ~ Season + GDP,
               data = training_set)
summary(regressor)

# Sexta iteracion del regressor de Africa
regressor = lm(formula = Incidence ~ GDP,
               data = training_set)
summary(regressor)

# Prediccion del regressor de Africa 
y_pred = predict(regressor, newdata = test_set)
y_pred

africa_plot <- ggplot(cbind(test_set, y_pred)) + aes(x = GDP) + 
  geom_point(aes(y = Incidence, size = as.numeric(ILI), color = as.factor(Season))) +
  geom_line(aes(y = y_pred), colour = "red", size = 2) +
  labs(title = "African Region of WHO",
       caption = "Source: FluNet/WorldBank") + 
  scale_color_discrete(name = "Season", breaks = c("1", "2", "3", "4"), labels=c("Winter", "Spring", "Summer", "Autumn")) + 
  scale_size_continuous(name = "ILI", breaks = c(1:6), labels = c("Local Outbreak", "No Activity", "No Report", "Regional Outbreak", "Sporadic", "Widespread Outbreak")) + 
  theme(plot.title = element_text(colour = "slategray"),
        plot.caption = element_text(colour = "slategray")) + 
  annotate("rect", xmin=7500, xmax=10500, ymin=3.8, ymax=4.2, fill="white", colour="red") +
  annotate("text", x=9000, y=4.0, label = paste("R^2 == ", format(summary(regressor)$adj.r.squared, digits = 4)), parse=T)

### Septimo regressor usando los datos correspondientes a Pacific
pacific <- regressorperwhoregion(flu, whoregion = "Western Pacific Region of WHO")

split <- sample.split(africa$Incidence, SplitRatio = 0.8)

training_set <- subset(pacific %>% filter(!is.na(GDP)), split == TRUE)
test_set <- subset(pacific %>% filter(!is.na(GDP)), split == FALSE)

# Primera iteracion del regressor de Pacific
regressor = lm(formula = Incidence ~ .,
               data = training_set)
summary(regressor)

# Segunda iteracion del regressor de Pacific
regressor = lm(formula = Incidence ~ Country + Year + ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Tercera iteracion del regressor de Pacific
regressor = lm(formula = Incidence ~ Country + ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Cuarta iteracion del regressor de Pacific
regressor = lm(formula = Incidence ~ ILI + Season + GDP,
               data = training_set)
summary(regressor)

# Quinta iteracion del regressor de Pacific
regressor = lm(formula = Incidence ~ ILI + GDP,
               data = training_set)
summary(regressor)

# Sexta iteracion del regressor de Pacific
regressor = lm(formula = Incidence ~ GDP,
               data = training_set)
summary(regressor)

# Prediccion del regressor de Pacific
y_pred = predict(regressor, newdata = test_set)
y_pred

pacific_plot <- ggplot(cbind(test_set, y_pred)) + aes(x = GDP) + 
  geom_point(aes(y = Incidence, size = as.numeric(ILI), color = as.factor(Season))) +
  geom_line(aes(y = y_pred), colour = "red", size = 2) +
  labs(title = "Western Pacific Region of WHO",
       caption = "Source: FluNet/WorldBank") + 
  scale_color_discrete(name = "Season", breaks = c("1", "2", "3", "4"), labels=c("Winter", "Spring", "Summer", "Autumn")) + 
  scale_size_continuous(name = "ILI", breaks = c(1:6), labels = c("Local Outbreak", "No Activity", "No Report", "Regional Outbreak", "Sporadic", "Widespread Outbreak")) + 
  theme(plot.title = element_text(colour = "slategray"),
        plot.caption = element_text(colour = "slategray")) + 
  annotate("rect", xmin=50000, xmax=70000, ymin=3.8, ymax=4.15, fill="white", colour="red") +
  annotate("text", x=60000, y=4.0, label = paste("R^2 == ", format(summary(regressor)$adj.r.squared, digits = 4)), parse=T)


sv_img(grid.arrange(euro_plot, pacific_plot, africa_plot, asia_plot, medi_plot, 
             layout_matrix = rbind(c(1,1,1,2,2,2), c(3,3,4,4,5,5))), filename = "flu-regressor-whoregion",
       width=18, height=10)



