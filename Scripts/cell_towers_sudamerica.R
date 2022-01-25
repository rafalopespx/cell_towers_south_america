library(dplyr)
library(readr)
library(ggplot2)
library(ggtext)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geobr)
library(stringr)
library(vroom)

source("Functions/functions.R")

names_countries<-c('Argentina','Bolivia', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Paraguay', 
         'Peru', 'Suriname', 'Venezuela', 'Guyana', 'French Guiana', 'Uruguay')

names_countries<-sort(names_countries)

celltowers_list<-list.files("Data/", pattern = "_cell_towers.csv.gz", full.names = T)

celltowers_list<-lapply(celltowers_list, function(x){
  x<-vroom(x)|>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
})
names(celltowers_list)<-names_countries
n_countries<-length(names_countries)

countries<-vector("list", n_countries)
plots<-vector("list", n_countries)
countries<-countries |>
  setNames(names_countries)
plots<-plots |>
  setNames(names_countries)

for (i in 1:n_countries) {
  name_country<-names(countries[i])
  countries[[i]]<-ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')|>
    filter(name == name_country) 
  
  plots[[i]]<-ggplot() + 
    geom_sf(data = countries[[i]])
}

UMTS_list<-vector("list", n_countries)
names(UMTS_list)<-names_countries
LTE_list<-vector("list", n_countries)
names(LTE_list)<-names_countries
plots_dark<-vector("list", n_countries)
names(plots_dark)<-names_countries

for (i in 1:n_countries) {
  UMTS_list[[i]]<-celltowers_list[[i]] |>
    filter(radio == "UMTS")|>
    st_intersection(countries[[i]])
  
  LTE_list[[i]] <- celltowers_list[[i]] |>
    filter(radio == "LTE")|>
    st_intersection(countries[[i]])
  
  plots_dark[[i]]<-ggplot() + 
    geom_sf(data = countries[[i]], fill = "black", color = "white", size = 0.3) + 
    geom_sf(data = UMTS_list[[i]], shape = ".", color = "#4d88ff", alpha = 0.3) +
    geom_sf(data = LTE_list[[i]], shape = ".", color = "#cc0000", alpha = 0.5)
  
  ggsave(paste0("plots/South America/", names_countries[i], "_cell_towers.png"), 
         plot = plots_dark[[i]], 
         height = 7, width = 7)
  
}

## All countries Plot

ggplot()+
  ## Argentina
  geom_sf(data = countries$Argentina, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Argentina,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Argentina, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## Bolivia
  geom_sf(data = countries$Bolivia, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Bolivia,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Bolivia, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## Brazil
  geom_sf(data = countries$Brazil, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Brazil,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Brazil, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## Chile
  geom_sf(data = countries$Chile, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Chile,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Chile, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## Colombia
  geom_sf(data = countries$Colombia, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Colombia,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Colombia, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## Ecuador
  geom_sf(data = countries$Ecuador, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Ecuador,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Ecuador, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## French Guiana
  geom_sf(data = countries$`French Guiana`, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$`French Guiana`,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$`French Guiana`, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## Guyana
  geom_sf(data = countries$Guyana, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Guyana,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Guyana, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## Paraguay
  geom_sf(data = countries$Paraguay, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Paraguay,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Paraguay, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## Peru
  geom_sf(data = countries$Peru, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Peru,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Peru, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## Uruguay
  geom_sf(data = countries$Uruguay, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Uruguay,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Uruguay, shape = ".", color = "#cc0000", alpha = 0.5)+
  ## Venezuela
  geom_sf(data = countries$Venezuela, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS_list$Venezuela,shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE_list$Venezuela, shape = ".", color = "#cc0000", alpha = 0.5)+
  theme_void() +
  labs(
    title = "<span style='color: white; text-align: center;'>Open Veins of South America </span>",
    subtitle =  "<span style='color: white; text-align: center;'> <span style='color: #cc0000'>4G</span> (LTE) e <span style='color: #4d88ff'>3G</span> (UMTS)</span>",
    caption = "<span style='color: white;'> By Rafael Lopes (@rafalpx) <b>&middot;</b> <br>Data from opencellid.org </span></br>"
  ) +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_markdown(margin = margin(t = 40, b = -60, l = 10), size = 20),
    plot.subtitle = element_markdown(margin = margin(t = 60, b = -120, l = 20), size = 10),
    plot.caption = element_markdown(hjust = 0, margin = margin(r = -10, b = 20, t = -30)),
    text = element_text(family = "Roboto Condensed")
  )

ggsave(paste0("plots/South America/south_ameria_cell_towers.png"), height = 9, width = 7)

#



