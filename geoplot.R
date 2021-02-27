library(ggplot2)
library(ggmap)
library(digest)
library(glue)
source('plots.R')

# Preprocessing ####
file <- paste('valeursfoncieres_', commune, '_', year, '.csv', sep='')
file
df.geoc <- read.csv(file)
df.geoc <- discretize_surface(df.geoc, surface_bins)
glimpse(df.geoc)
summary(df.geoc)
nsales <- nrow(df.geoc)
nsales
colnames(df.geoc)
center_lat <- mean(df.geoc$lat, na.rm = TRUE)
center_lon <- mean(df.geoc$lon, na.rm = TRUE)

facet_names <- c(
  '0-75'='0-75 m2',
  '75-150'='75-150 m2',
  '150-250'='150-250 m2',
  '250-400'='250-400 m2'
)

zoom <- 10
center_loc <- c(center_lon, center_lat)
map <- get_map(center_loc, zoom = zoom, scale = 2)

# VF / m2 ####
title <- build_title(paste('Valeur Foncière/m2 ', local), commune, year)
title
filename <- build_filename('ppm_geog', commune, year)
filename
caption <- build_caption(nsales, max_vf)
#df.geoc <- discretize_surface(df.geoc, surface_bins)
fin_map <- ggmap(map) +
  geom_point(aes(lon, lat, color = Valeur.par.metre),
             size=.5,
             data = df.geoc) +
  scale_colour_stepsn(limits  =c(1000, 4500),
                      colours = brewer.pal(n = 4, name = "RdYlBu")) +
  ggtitle(title) +
  labs(x       = "Longitude",
       y       = "Latitude",
       color   = "€/m2",
       caption = caption) +
  facet_wrap(~Surface.cat, labeller=as_labeller(facet_names))
fin_map
to_png(filename, 45, 30, 600)
filename
save.image(file='Environment.RData')
