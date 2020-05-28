#install.packages("sp")
#install.packages("broom")
#install.packages("tibble")
#install.packages("maptools")
#install.packages("gpclib")
#install.packages("rgeos")

setwd("~/R Projects/Maps")
source("preprocessflu.R")

library(sp)
library(dplyr)
library(ggplot2)
library(gganimate)
library(maptools)
library(gpclib)
library(rgeos)

gpclibPermit()

#mexico <- readRDS("gadm36_MEX_1_sp.rds")

getshapemex <- function(file, region){
  df <- readRDS(file)
  #df@data
  
  df.points <- fortify(df, region = region)
  df.points <- df.points[,c("long", "lat", "id", "group")]
  df.points$id <- as.factor(df.points$id)
  
  df.points$id <- recode(df.points$id,
                         "Distrito Federal" = "Ciudad de Mexico")
  df.points$id <- gsub(" ", "", df.points$id)
  df.points$id <- estados.transform(df.points$id)
  colnames(df.points) <- c("long", "lat", "id", "group")
  df.points$id <- sapply(df.points$id, as.factor)
  
  return(df.points)
}

mexico <- getshapemex(file = "gadm36_MEX_1_sp.rds", region = "NAME_1")
mexico <- left_join(mexico,
                    flu, c("id" = "Estados"))

makestaticyear <- function(df, year, ...){
  df <- df %>% filter(Años == year, Sexo == "H") 
  p <- ggplot(df) + aes(long, lat, group=group) + 
      geom_polygon(aes(...)) + 
      geom_path(color="white") + 
      labs(
         subtitle = paste("Año", as.character(year), sep = " "),
         caption = "Fuente: SINAVE/DGE") +
      scale_fill_gradient(low = "#5efad3", high = "#2F5B64", na.value = "darkgray") + 
      theme(
        plot.background = element_rect(fill = "lightgray"),
        panel.background = element_rect(fill = "lightgray"),
        axis.text = element_blank(),
        axis.title = element_blank()
    )
  p
}

makestaticyear(mexico, year = 2011, fill = Incidencia)
sv_img(grid.arrange(makestaticyear(mexico, year = 2011, fill = Incidencia),
                    layout_matriz=rbind(c(1,1))), "mexico2011", width=10, height=7)

