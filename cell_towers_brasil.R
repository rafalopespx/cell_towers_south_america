library(dplyr)
library(readr)
library(ggplot2)
library(ggtext)
library(sf)
library(rnaturalearth)

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



