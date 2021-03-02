# This utility loads the raw data from txt file, filters required columns and outputs to csv

# DVF data from:
# https://www.data.gouv.fr/fr/datasets/demandes-de-valeurs-foncieres/
library(dplyr)
library(data.table)

prefix <- 'valeursfoncieres'
year <- 2020

infile  <- paste(data_dir, prefix, '-', year, '.txt', sep='')
outfile <- paste(data_dir, prefix, '_', year, '.csv', sep='')

df.raw <- fread(infile,
                     header      = TRUE,
                     sep         = '|',
                     dec         = ',',
                     quote       = "'",
                     fill        = TRUE,
                     stringsAsFactors=FALSE)
head(df.raw)
nrow(df.raw)
ncol(df.raw)
colnames(df.raw)
df.raw<- df.raw %>% dplyr::rename('Date.mutation' = 'Date mutation',
                'Nature.mutation' = 'Nature mutation',
                'Valeur.fonciere' = 'Valeur fonciere',
                'No.voie'         = 'No voie',
                'Type.de.voie'    = 'Type de voie',
                'Code.postal'     = 'Code postal',
                'Type.local'      = 'Type local',
                'Surface.reelle.bati' = 'Surface reelle bati',
                'Nombre.pieces.principales' = 'Nombre pieces principales'
                )

df.filter <- df.raw %>% select('Date.mutation',
                               'Nature.mutation',
                               'Valeur.fonciere',
                               'No.voie',
                               'Type.de.voie',
                               'Voie',
                               'Code.postal',
                               'Commune',
                               'Type.local',
                               'Surface.reelle.bati',
                               'Nombre.pieces.principales')
ncol(df.filter)
glimpse(df.filter)
head(df.filter)
tail(df.filter)

# Extract sales only
df.filter <-
  df.filter %>%
  filter(Nature.mutation == 'Vente') %>%
  select(-Nature.mutation) # No need to keep column

df.filter$Valeur.fonciere <- as.numeric(as.character(df.filter$Valeur.fonciere))
df.filter$No.voie <- as.factor(df.filter$No.voie)
df.filter$Code.postal <- as.factor(df.filter$Code.postal)

nrow(df.filter)

head(df.filter)
tail(df.filter)
df.filter %>% summary()
str(df.filter)
nrow(df.filter)

# Save to csv
write.csv(df.filter, outfile, row.names = FALSE)
print(outfile)
