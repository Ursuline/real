library(tidyverse)
# Variable defs ####
###Finistere Nord-Est##
# 29600: Morlaix / 29252 Plouezoc'h
# 29630 plougasnou/Saint Jean du doigt
# 29620 Guimaec / 29241 Locquirec
###Côtes d'Armor Ouest##
# 22310 Plestin-les Greves
# 22300 Trédrez-Locquémeau to Lannion
# 22560 -> Trébeurden / Ile Grande / Plemeur Bodou
# 22730 -> Trégastel
# 22700 Ploumanac'h, Perros, Louanec, La Clarté, Saint Quay
# 22660 Trévélern, Trévou-Tréguignec
# 22710 Penvénan / 22820 Plougrescant / 22220 Tréguier etc
# 22610 Kerbor, Pleubian, Lanmodez, l'Armor
# 22620 l'Arcouest, Ploubazlanec / 22870 Bréhat / 22740: Lézardrieux

zipcodes_finistere <- c('29600', '29252', '29630', '29620', '29241')
zipcodes_cotes_d_armor <- c('22310', '22300','22560', '22730', '22700', '22660', '22710', '22820', '22220', '22610', '22620', '22870', '22740')
zipcodes <- c(zipcodes_finistere, zipcodes_cotes_d_armor)
communes <- c('TREGOR')
year <- 2019
max_vf <- 1000000
local <- 'Maison'
surface_bins <- c(0 ,75, 150, 250, 400)
max_ppm <- 10000 # Maximum price / m2 (to catch outliers)

commune <- str_to_title(communes[1])
commune <- gsub(' ', '-', commune)
print(commune)
