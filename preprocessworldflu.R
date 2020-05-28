setwd("~/R Projects/Maps")
library(dplyr)
library(tidyr)

selectdata <- function(df){
  if(length(df) > 22){
    df <- df[2:length(df)]
  }
  
  colnames(df) <- c("Country", colnames(df)[2:length(colnames(df))])
  df <- df %>% rename(Date = SDATE, ILI = TITLE) %>% 
    select(Country, WHOREGION, FLUREGION, Year, Week, Date, INF_A, ILI)
  return(df)
}
recode.countries <- function(df){
  df$Country <- recode(df$Country, 
                        "Antigua and Barbuda" = "Barbuda",
                        "Bahamas, The" = "Bahamas",
                        "Bolivia (Plurinational State of)" = "Bolivia",
                        "Bonaire, Saint Eustatius and Saba" = "Saint Eustatius",
                        "British Virgin Islands" = "Virgin Islands",
                        "Brunei Darussalam" = "Brunei",
                        "CÃ´te d'Ivoire" = "Ivory Coast",
                        "Cote d'Ivoire" = "Ivory Coast",
                        "Cabo Verde" = "Cape Verde",
                        "China, Hong Kong SAR" = "Hong Kong",
                        "China, Macao SAR" = "Macao",
                        "Congo" = "Democratic Republic of the Congo",
                        "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                        "CuraÃ§ao" = "Curacao",
                        "Czechia" = "Czech Republic",
                        "Democratic People's Republic of Korea" = "North Korea",
                        "Iran (Islamic Republic of)" = "Iran",
                        "Lao People's Democratic Republic" = "Laos",
                        "Micronesia (Federated States of)" = "Micronesia",
                        "North Macedonia" = "Macedonia",
                        "Korea, North" = "North Korea",
                        "Republic of Korea" = "South Korea",
                        "Korea, South" = "South Korea",
                        "Republic of Moldova" = "Moldova",
                        "Russian Federation" = "Russia",
                        "Saint Kitts and Nevis" = "Nevis",
                        "Saint Vincent and the Grenadines" = "Grenadines",
                        "St. Vincent and the Grenadines" = "Grenadines",
                        "Sint Maarten (Dutch part)" = "Saint Martin",
                        "Syrian Arab Republic" = "Syria",
                        "Trinidad and Tobago" = "Trinidad",
                        "United Kingdom of Great Britain and Northern Ireland" = "UK",
                        "United Kingdom" = "UK",
                        "United Republic of Tanzania" = "Tanzania",
                        "United States of America" = "USA",
                        "United States" = "USA",
                        "Venezuela (Bolivarian Republic of)" = "Venezuela",
                        "Viet Nam" = "Vietnam",
                        "Wallis and Futuna Islands" = "Wallis and Futuna",
                        "West Bank and Gaza Strip" = "Gaza Strip",
                        "Yemen, Rep." = "Yemen")
  df <- arrange(df, Country)
  return(df)
}
preprocessflunet <- function(){
  africa <- read.csv("African Region of WHO - 20102019.csv")
  america <- read.csv("Region of the Americas of WHO - 20102019.csv")
  asia <- read.csv("South-East Asia Region of WHO - 20102019.csv")
  europe <- read.csv("European Region of the WHO - 20102019.csv")
  mediterrean <- read.csv("Eastern Mediterranean Region of WHO - 20102019.csv")
  pacific <- read.csv("Western Pacific Region of WHO - 20102019.csv")
  
  africa <- selectdata(africa)
  america <- selectdata(america)
  asia <- selectdata(asia)
  europe <- selectdata(europe)
  mediterrean <- selectdata(mediterrean)
  pacific <- selectdata(pacific)
  
  flu <- rbind(africa, america, asia, europe, mediterrean, pacific)
  flu$Country <- as.character(flu$Country)
  flu <- flu %>% arrange(Country, Year)
  
  flu <- recode.countries(flu)
  
  return(flu)
}
preprocesspopulation <- function(){
  population <- read.csv("Population Reference Bureau.csv", stringsAsFactors = FALSE)
  population <- population %>% filter(Type == "Country") %>% rename(Country = Name, Population = Data) %>% 
    select(Country, Population)
  
  population <- recode.countries(population)
  
  return(population)
}
preprocessgdp <- function(){
  gdp <- read.csv("GDP WorldBank.csv", stringsAsFactors = FALSE)
  colnames(gdp) <- c("Country", "Country.Code", "Indicator", "Indicator.Code", 1960:2018)
  
  gdp <- gdp %>% select(-c(Country.Code, Indicator, Indicator.Code))
  gdp <- gdp %>% gather(key=Year, value = GDP, -c(Country)) %>% arrange(Country, Year)
  
  gdp$Year <- as.integer(gdp$Year)
  
  gdp <- recode.countries(gdp)
  
  return(gdp)
}
preprocessworld <- function(df, ...){
  world_map <- map_data("world")
  
  world <- world_map %>% left_join(df %>% group_by(Country, WHOREGION, Year) %>% summarize(...), 
                                   by=c("region" = "Country")) %>% select(-c(order, subregion))
  
  world$Factor[which(is.nan(world$Factor) == TRUE)] <- NA
  
  countries <- world %>% filter(is.na(Year)) 
  countries <- levels(as.factor(countries$region))
  countries <- as.data.frame(rep(countries, length(2010:2019))) 
  colnames(countries) <- c("Country")
  countries <- countries %>% arrange(Country)
  countries <- cbind(countries, rep(2010:2019, length(countries)))
  colnames(countries) <- c("Country", "Year")
  
  world <- world %>% left_join(countries, by=c("region" = "Country"))
  
  world$Year.x[which(is.na(world$Year.x) == TRUE)] <- world$Year.y[which(is.na(world$Year.x) == TRUE)]
  world <- world %>% rename(Year = Year.x) %>% select(-Year.y)
  
  return(world)  
}
preprocesspaho <- function(df){
  paho <- read.csv("PAHOVaccines.csv", stringsAsFactors = TRUE)
  paho <- paho %>% rename(Country = Country.PaÃ.s, Coverage = Coverage.Cobertura, Year = Year.AÃ.o) %>% select(Country, Year, Coverage)
  paho <- recode.countries(paho)
  
  paho <- left_join(df, paho, by=c("Country", "Year")) %>% filter(WHOREGION == "Region of the Americas of WHO")
  
  return(paho)
}
runflu <- function(){
  flu <- preprocessflunet()
  population <- preprocesspopulation()
  gdp <- preprocessgdp()
  
  flu <- flu %>% left_join(population, by="Country") %>% 
    mutate(Population = Population * 1E6, Incidence = (INF_A*1E5)/Population) %>% 
    left_join(gdp, by=c("Country", "Year"))
  
  return(flu)
}

flu <- runflu()
world <- preprocessworld(df = flu, Factor = mean(GDP, na.rm = TRUE))
paho <- preprocesspaho(df = flu)

