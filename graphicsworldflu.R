setwd("~/R Projects/Maps")
source("preprocessworldflu.R")

library(dplyr)
library(ggplot2)
library(gganimate)
library(gridExtra)

normalize <- function(x) {
  x <- as.numeric(x)
  return ((x - min(x)) / (max(x) - min(x)))
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
sv_anim <- function(data, name){
  final_animation <- animate(data, 200, fps = 120, duration = 10, width = 2000,
                             height = 1000, renderer = gifski_renderer())
  assign("final_animation", final_animation, envir = globalenv())
  filename <- getwd()
  filename <- paste(filename, "/", name, ".gif", sep = "")
  anim_save(filename, animation=final_animation)
}

world <- preprocessworld(flu, Factor = mean(Incidence, na.rm = TRUE))

## Comparación de gráficas de acuerdo con la cantidad de casos e incidencia reportados en las diversas regiones de la OMS 
sv_img(grid.arrange(
  ggplot(flu %>% group_by(WHOREGION, Year) %>% summarise(INF_A = mean(INF_A, na.rm=TRUE))) + 
    aes(x = as.factor(Year), INF_A) + 
    geom_boxplot(aes(group = as.factor(Year))) + 
    geom_point(aes(color = as.factor(WHOREGION))) +
    labs(caption = "Fuente: FluNet",
         x = "Años",
         y = "Casos de Influenza A") + 
    theme(plot.caption = element_text(colour="gray")) +
    scale_colour_discrete(name= "WHO REGION"),
  
  ggplot(flu %>% group_by(WHOREGION, Year) %>% summarise(Incidence = mean(Incidence, na.rm=TRUE))) + 
    aes(x = as.factor(Year), Incidence) + 
    geom_boxplot(aes(group = as.factor(Year))) + 
    geom_point(aes(color = as.factor(WHOREGION))) +
    labs(caption = "Fuente: FluNet/PRB",
         x = "Años",
         y = "Incidencia de Influenza A") + 
    theme(plot.caption = element_text(colour="gray")) +
    scale_colour_discrete(name= "WHO REGION"), layout_matrix = rbind(c(1,1), c(2,2))),
  filename = "flu-incidencia-casos-whoregion", width=18, height=10)


## Gráfica correspondiente a la incidencia de influenza de acuerdo a la región de America Central 
sv_img(grid.arrange(ggplot(flu %>% filter(FLUREGION == "Central America and Caribbean", !is.na(Incidence)) %>% 
                             group_by(Country, Year) %>% summarize(Incidence = mean(Incidence, na.rm = TRUE))) + 
                      aes(x = as.factor(Year), y = Incidence) + 
                      geom_point(aes(color = as.factor(Country), size = Incidence)) + 
                      geom_line(aes(color = as.factor(Country), group=as.factor(Country))) + 
                      labs(caption = "Fuente: FluNet/PRB",
                           x = "Años",
                           y = "Incidencia de Influenza A") + 
                      theme(legend.position = "none",
                            plot.caption = element_text(colour="gray")) + 
                      scale_colour_discrete(name="Paises de Centro America") +
                      scale_size_continuous(name="Incidencia") + 
                      facet_wrap(~ Country, scales = "fixed"), 
                    layout_matrix = rbind(c(1,1))), filename = "flu-incidencia-centroamerica",
       width=18, height=11)


## Gráfica correspondiente con la correlacion entre PIB e Incidencia de influenza en la región de Centro America
sv_img(grid.arrange(
  ggplot(flu %>% filter(FLUREGION == "Central America and Caribbean", !is.na(Incidence), !is.na(GDP)) %>% 
           mutate(Incidence = normalize(Incidence), GDP = normalize(GDP)) %>% 
           group_by(Country, Year) %>% summarize(Incidence = mean(Incidence, na.rm = TRUE),
                                                 GDP = mean(GDP, na.rm = TRUE))) + 
    aes(x = as.factor(Year)) + 
    geom_point(aes(y = Incidence, color = as.factor(Country), size = Incidence)) + 
    geom_line(aes(y = Incidence, color = as.factor(Country), group=as.factor(Country))) + 
    geom_point(aes(y = GDP), color = "darkslategray") + 
    geom_line(aes(y = GDP, group = as.factor(Country)), color = "darkslategray") + 
    labs(caption = "Fuente: FluNet/PRB/BancoMundial",
         x = "Años",
         y = "Normalizacion Incidencia/PIB") + 
    theme(legend.position = "none",
          plot.caption = element_text(colour="gray")) + 
    scale_colour_discrete(name="Paises de Centro America") +
    scale_size_continuous(name="Incidencia") + 
    facet_wrap(~ Country, scales = "fixed"), 
  layout_matrix = rbind(c(1,1))), filename = "flu-pib-centroamerica",
  width=18, height=11)


## Representación geográfica de la incidencia de influenza durante 2019
sv_img(
  grid.arrange(
    ggplot(world%>%filter(Year==2015 | is.na(Year))) + aes(long, lat, group=group, fill=Factor) + 
      geom_polygon()+
      geom_path(colour = "white") + 
      labs(title = "Incidencia Mundial de Influenza",
           subtitle = "OMS: 2019",
           caption  = "Fuente: FluNet/PRB") + 
      theme(plot.title=element_text(size=20, hjust=0.5, face="bold", colour="grey", vjust=-1)
            ,plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey")
            ,plot.caption = element_text(size=12, face = "bold", color="lightgray")
            ,panel.background = element_rect(fill = "white")
            ,plot.background = element_rect(fill = "white")
            ,panel.grid = element_blank()
            ,axis.text = element_blank()
            ,axis.title = element_blank()
            ,axis.ticks = element_blank()) +
      scale_fill_gradient(name="GDP" ,low = "#2AD5CE", high = "#37743B", na.value = "darkgray"),
    layout_matrix=rbind(c(1,1))), filename = "flu-world-incidence-2019",
  width = 18, height = 11)


## Representación dinámica geográfica de la incidencia de influenza en el mundo 
qlt <- ggplot(world) + aes(long, lat, group=group, fill=Incidence) + 
  geom_polygon()+
  geom_path(colour = "white") + 
  labs(title = "Incidencia Mundial de Influenza",
       subtitle = "OMS: 2010 - 2019",
       caption  = "Fuente: FluNet/PRB") + 
  theme(plot.title=element_text(size=20, hjust=0.5, face="bold", colour="grey", vjust=-1)
        ,plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey")
        ,plot.caption = element_text(size=12, face = "bold", color="lightgray")
        ,panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "white")
        ,panel.grid = element_blank()
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()) +
  scale_fill_gradient(low = "lightsteelblue", high = "navy", na.value = "darkgray") + 
  transition_manual(as.factor(Year))

sv_anim(qlt, "flu-world-incidence-2010-2019")


## Manera de adicionar los datos al df world para poder hacer el mapa geográfico 
paho %>% group_by(Country, WHOREGION, Year) %>% summarise(Coverage = mean(Coverage))

world <- world %>% left_join(paho %>% group_by(Country, WHOREGION, Year) %>% summarise(Coverage = mean(Coverage)),
                             by = c("region" = "Country", "WHOREGION", "Year"))

world %>% filter(region == "Mexico")

## Representación geográfica de la cobertura de inmunización de influenza en la OPS durante 2012
sv_img(grid.arrange(
  ggplot(world %>% filter(WHOREGION == "Region of the Americas of WHO", Year == 2012)) + aes(long, lat, group=group, fill=Coverage) + 
    geom_polygon() + 
    geom_path(colour = "white") + 
    labs(title = "Cobertura de Inmunizacion",
         subtitle = "OPS: 2012",
         caption  = "Fuente: OPS") + 
    theme(plot.title=element_text(size=20, hjust = 0.5, face="bold", colour="grey", vjust=-1)
          ,plot.subtitle=element_text(size=18, hjust = 0.5, face="italic", color="grey")
          ,plot.caption = element_text(size=12, face = "bold", color="lightgray")
          ,panel.background = element_rect(fill = "white")
          ,plot.background = element_rect(fill = "white")
          ,panel.grid = element_blank()
          ,axis.text = element_blank()
          ,axis.title = element_blank()
          ,axis.ticks = element_blank()) +
    scale_fill_gradient(name="Cobertura", low = "#D2D538", high = "#C9363B", na.value = "darkgray"),
  layout_matrix = rbind(c(1,1))), filename = "flu-immunization-2012", width=18, height=10)


## Representación geográfica dinámica de la cobertura de inmunización de influenza en la OPS durante 2010 - 2019
plt <- ggplot(world %>% filter(WHOREGION == "Region of the Americas of WHO")) + aes(long, lat, group=group, fill=Coverage) + 
  geom_polygon() + 
  geom_path(colour = "white") + 
  labs(title = "Cobertura de Inmunizacion",
       subtitle = "OPS: 2010 - 2019",
       caption  = "Fuente: OPS") + 
  theme(plot.title=element_text(size=20, hjust = 0.5, face="bold", colour="grey", vjust=-1)
        ,plot.subtitle=element_text(size=18, hjust = 0.5, face="italic", color="grey")
        ,plot.caption = element_text(size=12, face = "bold", color="lightgray")
        ,panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "white")
        ,panel.grid = element_blank()
        ,axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()) +
  scale_fill_gradient(name="Cobertura", low = "#D2D538", high = "#C9363B", na.value = "darkgray") + 
  transition_manual(Year)
plt

sv_anim(plt, "flu-america-coverage-2010-2019")
