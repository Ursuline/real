library(ggplot2)
library(ggmap)
library(digest)
library(glue)
source('plots.R')
#year <- 2020

vf_plot <- function(map){
  # Valeur foncière
  outfile <- build_filename('vf_geog', commune, year)
  dot_size <- 2.0
  limits   <- c(500000, 3500000) # Saint-Didier

  title <- build_title(paste('Valeur Foncière ', local), commune, year)
  caption <- build_caption(nsales, max_vf)

  vf_map <- ggmap(map) +
    geom_point(aes(lon, lat, color = Valeur.fonciere),
               size=dot_size,
               data = df.geoc) +
    scale_colour_stepsn(limits  = limits,
                        colours = brewer.pal(n = 4, name = "RdYlBu")) +
    ggtitle(title) +
    labs(x       = "Longitude",
         y       = "Latitude",
         color   = "prix (€)",
         caption = caption)
  print(vf_map)
  to_png(outfile, vf_map, 45, 30, 600)
  outfile
}

ppm_plot <- function (map){
  # Valeur foncière par m^2
  facet_names <- c(
    '0-75'='0-75 m2',
    '75-150'='75-150 m2',
    '150-250'='150-250 m2',
    '250-400'='250-400 m2'
  )
  outfile <- build_filename('vfpm_geog', commune, year)
  outfile
  dot_size <- 1.0
  limits   <- c(2000, 7000) # Saint-Didier

  title <- build_title(paste('Valeur Foncière/m2 ', local), commune, year)
  title
  caption <- build_caption(nsales, max_vf)

  fin_map <- ggmap(map) +
    geom_point(aes(lon, lat, color = Valeur.par.metre),
               size=dot_size,
               data = df.geoc) +
    scale_colour_stepsn(limits  = limits,
                        colours = brewer.pal(n = 4, name = "RdYlBu")) +
    ggtitle(title) +
    labs(x       = "Longitude",
         y       = "Latitude",
         color   = "€/m2",
         caption = caption) +
    facet_wrap(~Surface.cat, labeller=as_labeller(facet_names))
  print(fin_map)
  to_png(outfile, fin_map, 45, 30, 600)
  outfile
}

infile <- paste(directory, 'valeursfoncieres_', commune, '_', year, '.csv', sep='')
infile

df.geoc <- read.csv(infile)
df.geoc <- discretize_surface(df.geoc, surface_bins)
glimpse(df.geoc)
summary(df.geoc)
nsales <- nrow(df.geoc)
nsales
colnames(df.geoc)
center_lat <- mean(df.geoc$lat, na.rm = TRUE)
center_lon <- mean(df.geoc$lon, na.rm = TRUE)

zoom <- 13
center_loc <- c(center_lon, center_lat)
map <- get_map(center_loc, zoom = zoom, scale = 2)

ppm_plot(map)
vf_plot(map)

save.image(file='Environment.RData')
