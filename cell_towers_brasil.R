library(dplyr)
library(readr)
library(ggplot2)
library(ggtext)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geobr)

celltowers <- read_csv("../Cell Towers/724.csv.gz")|> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

brasil <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf') |>
  filter(name == 'Brazil') 

ggplot() + 
  geom_sf(data = brasil)

UMTS <- celltowers |> 
  filter(radio == "UMTS") |>
  st_intersection(brasil)

LTE <- celltowers |> 
  filter(radio == "LTE") |>
  st_intersection(brasil)

ggplot() + 
  geom_sf(data = brasil, fill = "black", color = "white", size = 0.3) + 
  geom_sf(data = UMTS, shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE, shape = ".", color = "#cc0000", alpha = 0.5)

x <- ggplot() + 
  geom_sf(data = brasil, fill = "black", color = "white", size = 0.3) + 
  geom_sf(data = UMTS, shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE, shape = ".", color = "#cc0000", alpha = 0.5) +
  theme_void() +
  labs(
    title = "<span style='color: white; text-align: center;'>Cell tower distribution with <br> <span style='color: #cc0000'>4G</span> (LTE) and <span style='color: #4d88ff'>3G</span> (UMTS)</span>",
    caption = "<span style='color: white;'> Made by Rafael Lopes <b>&middot;</b> <br>Original plot by Dominic Royé and with codes by Etienne Bacher <b>&middot;</b> Data from opencellid.org </span></br>"
  ) +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_markdown(margin = margin(t = 40, b = -60, l = 10), size = 20),
    plot.caption = element_markdown(hjust = 0, margin = margin(l = 10, b = 20, t = -30)),
    text = element_text(family = "Roboto Condensed")
  )
x
ggsave("my_plot_br.png", plot = x, height = 7, width = 7)


br_states<-read_state(code_state = "all", year = 2010, simplified = T, showProgress = F)
br_states<-br_states %>% 
  st_transform(crs = st_crs(celltowers))


#Portugues Legend
x <- ggplot() + 
  geom_sf(data = brasil, fill = "black", color = "white", size = 0.3) + 
  geom_sf(data = br_states, fill = "black", color = "white", size = 0.3)+
  geom_sf(data = UMTS, shape = ".", color = "#4d88ff", alpha = 0.3) +
  geom_sf(data = LTE, shape = ".", color = "#cc0000", alpha = 0.5) +
  theme_void() +
  labs(
    title = "<span style='color: white; text-align: center;'>Distribuição de torres de celular <br> <span style='color: #cc0000'>4G</span> (LTE) e <span style='color: #4d88ff'>3G</span> (UMTS)</span>",
    caption = "<span style='color: white;'> Feito por Rafael Lopes <b>&middot;</b> <br>Original por Dominic Royé e codigos por Etienne Bacher <b>&middot;</b></br> <br>Data from opencellid.org </span></br>"
  ) +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_markdown(margin = margin(t = 40, b = -60, l = 10), size = 20),
    plot.caption = element_markdown(hjust = 0, margin = margin(l = 10, b = 20, t = -30)),
    text = element_text(family = "Roboto Condensed")
  )
x
ggsave("my_plot_br_pt.png", plot = x, height = 7, width = 7)

#Breaking by states
for (i in unique(br_states$abbrev_state)) {
  state<-br_states %>% 
    filter(abbrev_state == i)
  
  UMTS_state <- celltowers |> 
    filter(radio == "UMTS") |>
    st_intersection(state)
  
  LTE_state <- celltowers |> 
    filter(radio == "LTE") |>
    st_intersection(state)
  
  x_st <- ggplot() + 
    geom_sf(data = state, fill = "black", color = "white", size = 0.3)+
    geom_sf(data = UMTS_state, shape = ".", color = "#4d88ff", alpha = 0.3) +
    geom_sf(data = LTE_state, shape = ".", color = "#cc0000", alpha = 0.5) +
    theme_void() +
    labs(
      title = "<span style='color: white; text-align: center;'>Distribuição de torres de celular <br> <span style='color: #cc0000'>4G</span> (LTE) e <span style='color: #4d88ff'>3G</span> (UMTS)</span>",
      subtitle = paste0("<span style='color: white; text-align: center;'> Estado de ", i, " </span>"),
      caption = "<span style='color: white;'> Feito por Rafael Lopes <b>&middot;</b> <br>Data from opencellid.org </span></br>"
    ) +
    theme(
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      plot.title = element_markdown(margin = margin(t = 40, b = -60, l = 10), size = 20),
      plot.subtitle = element_markdown(margin = margin(t = 60, b = -120, l = 20), size = 10),
      plot.caption = element_markdown(hjust = 0, margin = margin(l = 10, b = 20, t = -30)),
      text = element_text(family = "Roboto Condensed")
    )
  x_st
  
  ggsave(paste0("my_plot_state_", i, ".png"), plot = x_st, height = 7, width = 7)
}





