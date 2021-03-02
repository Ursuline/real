# Extract lats & longs of addresses from google
library(ggmap)
library(RCurl)
library(jsonlite)
library(leaflet)

source('util.R')
source('google_id.R')
source('parameters_saint_didier.R')

directory <- './data/'
infile  <- paste(directory, 'valeursfoncieres_', year, '.csv', sep='')
outfile <- paste(directory, 'valeursfoncieres_', commune, '_', year, '.csv', sep='')

# Preprocessing ####
print(infile)
df.raw <- read.csv(infile)
df.filter <- filter_data(df.raw, zipcodes, max_vf, local)
df.filter <- feature_engineering(df.filter, max_ppm, surface_bins)
glimpse(df.filter)
summary(df.filter)
nrow(df.filter)
# Concatenate address
df.filter <- df.filter %>%
  mutate(addr = paste(No.voie, Type.de.voie, Voie, Code.postal, Commune, 'France'))
colnames(df.filter)
head(df.filter)

# run the geocode function from ggmap package
address_ggmap <- geocode(location = df.filter$addr, output = "more", source = "google")
glimpse(address_ggmap)
summary(address_ggmap)
colnames(address_ggmap)

df.geoc <- cbind(df.filter, address_ggmap) %>%
  select(-addr, -north, -south, -east, -west, -type, -loctype, -address)

glimpse(df.geoc)
summary(df.geoc)
colnames(df.geoc)

#Save to csv file
#file <- paste('valeursfoncieres_', commune, '_', year, '.csv', sep='')
write.csv(df.geoc, outfile, row.names = FALSE)
outfile
