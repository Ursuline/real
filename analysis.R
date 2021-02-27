library(scales)
library(rlang)
library(ggpubr)

library("PerformanceAnalytics")
source('plots.R')
source('util.R')
source('parameters_tregor.R')

# Load geocoded data ####
file <- paste('valeursfoncieres_', commune, '_',year, '.csv', sep='')
df.geoc <- read.csv(file) # Load geocoded file
df.geoc <- discretize_surface(df.geoc, surface_bins)
glimpse(df.geoc)
summary(df.geoc)
nsales <- nrow(df.geoc)
nsales

# Compute some variables for use in Rmd file####
linear_vf <- function(lmVF, month){
  return(lmVF$coefficients[1] + as.numeric(month) * lmVF.med$coefficients[2])
}

df.temp <- df.geoc %>% filter(Code.postal == '93200') %>% select(Surface.reelle.bati)
surface.median.93200 <- median(df.temp$Surface.reelle.bati)
df.temp <- df.geoc %>% filter(Code.postal == '93210') %>% select(Surface.reelle.bati)
surface.median.93210 <- median(df.temp$Surface.reelle.bati)
df.temp <- df.geoc %>% filter(Code.postal == '93450') %>% select(Surface.reelle.bati)
surface.median.93450 <- median(df.temp$Surface.reelle.bati)

df.temp <- df.geoc %>% filter(Code.postal == '93200') %>% select(Valeur.par.metre)
ppm.median.93200 <- median(df.temp$Valeur.par.metre)
df.temp <- df.geoc %>% filter(Code.postal == '93210') %>% select(Valeur.par.metre)
ppm.median.93210 <- median(df.temp$Valeur.par.metre)
df.temp <- df.geoc %>% filter(Code.postal == '93450') %>% select(Valeur.par.metre)
ppm.median.93450 <- median(df.temp$Valeur.par.metre)

df.temp <- df.geoc %>% filter(Code.postal == '93200') %>% select(Valeur.fonciere)
vf.median.93200 <- median(df.temp$Valeur.fonciere)
df.temp <- df.geoc %>% filter(Code.postal == '93210') %>% select(Valeur.fonciere)
vf.median.93210 <- median(df.temp$Valeur.fonciere)
df.temp <- df.geoc %>% filter(Code.postal == '93450') %>% select(Valeur.fonciere)
vf.median.93450 <- median(df.temp$Valeur.fonciere)

df.temp <- df.geoc %>% filter(Code.postal == '93200') %>% select(Nombre.pieces.principales)
no.pieces.median.93200 <- median(df.temp$Nombre.pieces.principales)
df.temp <- df.geoc %>% filter(Code.postal == '93210') %>% select(Nombre.pieces.principales)
no.pieces.median.93210 <- median(df.temp$Nombre.pieces.principales)
df.temp <- df.geoc %>% filter(Code.postal == '93450') %>% select(Nombre.pieces.principales)
no.pieces.median.93450 <- median(df.temp$Nombre.pieces.principales)

volume <- df.geoc %>% group_by(Month) %>% summarize(count = n())
# Max & min volume
min.volume <- min(volume$count)
max.volume <- max(volume$count)
# Months corresponding to max & min volume
max.volume.month <- month.abb[volume$Month[which.max(volume$count)]]
min.volume.month <- month.abb[volume$Month[which.min(volume$count)]]

ppm <- df.geoc %>%
  group_by(Month) %>%
  summarize(VF.med  = median(Valeur.par.metre),
            VF.mean = mean(Valeur.par.metre))

ppm.93200 <- df.geoc %>% filter(Code.postal == '93200') %>%
  group_by(Month) %>%
  summarize(VF.med  = median(Valeur.par.metre),
            VF.mean = mean(Valeur.par.metre))

ppm.93210 <- df.geoc %>% filter(Code.postal == '93210') %>%
  group_by(Month) %>%
  summarize(VF.med  = median(Valeur.par.metre),
            VF.mean = mean(Valeur.par.metre))

ppm.93450 <- df.geoc %>% filter(Code.postal == '93450') %>%
  group_by(Month) %>%
  summarize(VF.med  = median(Valeur.par.metre),
            VF.mean = mean(Valeur.par.metre))

lmVF.med <- lm(VF.med~Month, data = ppm) #Linear regression
summary(lmVF.med) #Review the results

# percent increase from regression over 12 months
ppm.med.delta <-
  as.numeric(100*(11*lmVF.med$coefficients[2])/
               (lmVF.med$coefficients[2] + lmVF.med$coefficients[1]))

lmVF.med <- lm(VF.med~Month, data = ppm) #Linear regression
summary(lmVF.med) #Review the results
lmVF.93200.med <- lm(VF.med~Month, data = ppm.93200) #Linear regression
summary(lmVF.93200.med) #Review the results
lmVF.93210.med <- lm(VF.med~Month, data = ppm.93210) #Linear regression
summary(lmVF.93210.med) #Review the results
lmVF.93450.med <- lm(VF.med~Month, data = ppm.93450) #Linear regression
summary(lmVF.93450.med) #Review the results

linear_vf(lmVF.med, 1)
linear_vf(lmVF.med, 12)

D_VF_2019 <- 100 * (linear_vf(lmVF.med, 12)-linear_vf(lmVF.med, 1))/linear_vf(lmVF.med, 1)

linear_vf(lmVF.93200.med, 1)
linear_vf(lmVF.93200.med, 12)
D_VF_93200.2019 <- 100 * (linear_vf(lmVF.93200.med, 12)-linear_vf(lmVF.93200.med, 1))/linear_vf(lmVF.93200.med, 1)

linear_vf(lmVF.93210.med, 1)
linear_vf(lmVF.93210.med, 12)
D_VF_93210.2019 <- 100 * (linear_vf(lmVF.93210.med, 12)-linear_vf(lmVF.93210.med, 1))/linear_vf(lmVF.93210.med, 1)

linear_vf(lmVF.93450.med, 1)
linear_vf(lmVF.93450.med, 12)
D_VF_93450.2019 <- 100 * (linear_vf(lmVF.93450.med, 12)-linear_vf(lmVF.93450.med, 1))/linear_vf(lmVF.93450.med, 1)

# percent increase from regression over 12 months
ppm.med.delta <-
  as.numeric(100*(11*lmVF.med$coefficients[2])/
               (lmVF.med$coefficients[2] + lmVF.med$coefficients[1]))

plot(ppm$VF.med, pch = 16, col = "blue") #Plot the results
points(ppm$VF.mean, pch = 16, col = "dark red")
abline(lmVF.med, col = "blue") #Add a regression line
abline(lmVF.mean, col = "dark red") #Add a regression line

plot(lmVF.med$residuals, pch = 16, col = "blue")
points(lmVF.mean$residuals, pch = 16, col = "dark red")

# 93200
ppm.93200 <-
  filter(df.geoc, Code.postal == '93200') %>%
  group_by(Month) %>%
  summarize(VF.med  = median(Valeur.par.metre),
            VF.mean = mean(Valeur.par.metre))

lmVF.med.93200 <- lm(VF.med~Month, data = ppm.93200) #Linear regression
lmVF.mean.93200 <- lm(VF.mean~Month, data = ppm.93200) #Linear regression
summary(lmVF.med.93200) #Review the results

# percent increase from regression over 12 months
ppm.med.delta.93200 <-
  as.numeric(100*(11*lmVF.med.93200$coefficients[2])/
               (lmVF.med.93200$coefficients[2] + lmVF.med.93200$coefficients[1]))

plot(ppm.93200$VF.med, pch = 16, col = "blue") #Plot the results
points(ppm.93200$VF.mean, pch = 16, col = "dark red")
abline(lmVF.med.93200, col = "blue") #Add a regression line
abline(lmVF.mean.93200, col = "dark red") #Add a regression line

plot(lmVF.med.93200$residuals, pch = 16, col = "blue")
points(lmVF.mean.93200$residuals, pch = 16, col = "dark red")

# 93210
ppm.93210 <-
  filter(df.geoc, Code.postal == '93210') %>%
  group_by(Month) %>%
  summarize(VF.med  = median(Valeur.par.metre),
            VF.mean = mean(Valeur.par.metre))

lmVF.med.93210 <- lm(VF.med~Month, data = ppm.93210) #Linear regression
lmVF.mean.93210 <- lm(VF.mean~Month, data = ppm.93210) #Linear regression
summary(lmVF.med.93210) #Review the results

# percent increase from regression over 12 months
ppm.med.delta.93210 <-
  as.numeric(100*(11*lmVF.med.93210$coefficients[2])/
               (lmVF.med.93210$coefficients[2] + lmVF.med.93210$coefficients[1]))

plot(ppm.93210$VF.med, pch = 16, col = "blue") #Plot the results
points(ppm.93210$VF.mean, pch = 16, col = "dark red")
abline(lmVF.med.93210, col = "blue") #Add a regression line
abline(lmVF.mean.93210, col = "dark red") #Add a regression line

plot(lmVF.med.93210$residuals, pch = 16, col = "blue")
points(lmVF.mean.93210$residuals, pch = 16, col = "dark red")

# 93450
ppm.93450 <-
  filter(df.geoc, Code.postal == '93450') %>%
  group_by(Month) %>%
  summarize(VF.med  = median(Valeur.par.metre),
            VF.mean = mean(Valeur.par.metre))

lmVF.med.93450 <- lm(VF.med~Month, data = ppm.93450) #Linear regression
lmVF.mean.93450 <- lm(VF.mean~Month, data = ppm.93450) #Linear regression
summary(lmVF.med.93450) #Review the results

# percent increase from regression over 12 months
ppm.med.delta.93450 <-
  as.numeric(100*(11*lmVF.med.93450$coefficients[2])/
               (lmVF.med.93450$coefficients[2] + lmVF.med.93450$coefficients[1]))

plot(ppm.93450$VF.med, pch = 16, col = "blue") #Plot the results
points(ppm.93450$VF.mean, pch = 16, col = "dark red")
abline(lmVF.med.93450, col = "blue") #Add a regression line
abline(lmVF.mean.93450, col = "dark red") #Add a regression line

plot(lmVF.med.93450$residuals, pch = 16, col = "blue")
points(lmVF.mean.93450$residuals, pch = 16, col = "dark red")

qqnorm(lmVF.med$resid)
qqline(lmVF.med$resid)

qqnorm(lmVF.med.93200$resid)
qqline(lmVF.med.93200$resid)

qqnorm(lmVF.med.93210$resid)
qqline(lmVF.med.93210$resid)

qqnorm(lmVF.med.93450$resid)
qqline(lmVF.med.93450$resid)

surface.moyen  <- mean(df.geoc$Surface.reelle.bati)
surface.median <- median(df.geoc$Surface.reelle.bati)
surface.mode   <- getmode(df.geoc$Surface.reelle.bati)

ppm.moyen  <- mean(df.geoc$Valeur.par.metre)
ppm.median <- median(df.geoc$Valeur.par.metre)
ppm.mode   <- getmode(df.geoc$Valeur.par.metre)

vf.moyen  <- mean(df.geoc$Valeur.fonciere)
vf.median <- median(df.geoc$Valeur.fonciere)
vf.mode   <- getmode(df.geoc$Valeur.fonciere)

# Correlation plots ####
df.cor <- df.geoc %>% select(Valeur.par.metre, Valeur.fonciere, Surface.reelle.bati, Nombre.pieces.principales, lon, lat)
corr_plot <- correlation_plot(df.cor, commune, year)
corr_plot

# Volume de ventes ####
title <- build_title('Volume de ventes', commune, year)
filename <- build_filename('volume_par_code_postal', commune, year)
caption <- build_caption(nsales, max_vf)
volume_par_code_postal <- volume_plot(df.geoc, 'Code.postal', title, 'Code postal', 'Nombre de transactions', caption, filename)

# Monthly
df.monthly <- get_summary_df(df.geoc, 'Month', 'Valeur.par.metre')
title <- build_title('Volume de ventes mensuel', commune, year)
filename <- build_filename('volume_mensuel', commune, year)
#caption <- build_caption(nsales, max_vf)
monthly_volume_plot <- plot_summary_data(df.monthly, 'Month', 'cnt', title, 'Mois', 'No de ventes', caption, filename, TRUE)

df.monthly2 <- get_summary_df2(df.geoc, 'Month', 'Code.postal', 'Valeur.par.metre')
title <- build_title('Volume de ventes mensuel', commune, year)
filename <- build_filename('volume_mensuel_par_code_postal', commune, year)
#caption <- build_caption(nsales, max_vf)
monthly_code_postal_volume_plot <- facet_summary_data(df.monthly2, 'cnt', 'Code.postal', title, 'Mois', 'No de ventes', caption, filename, TRUE)

monthly_volume_plot_by_CP <- df.monthly2 %>%
  ggplot(aes(Month, cnt, group=Code.postal, color=as.factor(Code.postal))) +
  geom_line(size = .5) +
  geom_point() +
  labs(title =title,
       caption = caption,
       color = 'Code postal',
       x = 'Mois',
       y = 'No de ventes') +
  scale_x_continuous(breaks = seq(1, 12)) +
  scale_y_continuous(breaks = seq(0, 90, 10))+
  scale_color_brewer(palette='Dark2')
monthly_volume_plot_by_CP

# By surface
df.size <- get_summary_df(df.geoc, 'Surface.cat', 'Valeur.fonciere')
title <- build_title('Volume de ventes', commune, year)
filename <- build_filename('volume_par_surface', commune, year)
surface_volume_plot <- plot_summary_data(df.size, 'Surface.cat', 'cnt', title, 'Surface (m\u00b2)', 'No de ventes', caption, filename, FALSE)

df.size2 <- get_summary_df2(df.geoc, 'Surface.cat', 'Code.postal', 'Valeur.fonciere')

surface_volume_plot_by_CP <- df.size2 %>%
  ggplot(aes(Surface.cat, cnt, group=Code.postal, color=as.factor(Code.postal))) +
  geom_line(size = .5) +
  geom_point() +
  labs(title =title,
       caption = caption,
       color = 'Code postal',
       x = 'Surface (m\u00b2)',
       y = 'No de ventes')+
  scale_y_continuous(breaks = seq(0, 175, 25))+
  scale_color_brewer(palette='Dark2')
surface_volume_plot_by_CP

# Valeur foncière ####
nbins <- 20
summ <- get_stats(df.geoc, 'Valeur.fonciere')
title <- build_title('Valeur foncière', commune, year)
filename <- build_filename('valeur_fonciere', commune, year)
vf_hist_plot <- plot_histogram(df.geoc, 'Valeur.fonciere', title, 'Valeur foncière (\u20ac)', 'No de ventes', caption, summ$avg, summ$med, summ$mode, summ$max, nbins, filename)

df.geog <- df.geoc %>% filter(Code.postal=='93200')
summ <- get_stats(df.geog, 'Valeur.fonciere')
title <- build_title('Valeur foncière 93200', commune, year)
filename <- build_filename('valeur_fonciere_93200', commune, year)
vf_hist_plot.93200 <- plot_histogram(df.geog, 'Valeur.fonciere', title, 'Valeur foncière (\u20ac)', 'No de ventes', caption, summ$avg, summ$med, summ$mode, summ$max, nbins, filename)

df.geog <- df.geoc %>% filter(Code.postal=='93210')
summ <- get_stats(df.geog, 'Valeur.fonciere')
title <- build_title('Valeur foncière 93210', 'La Plaine St-Denis', year)
filename <- build_filename('valeur_fonciere_93210', commune, year)
vf_hist_plot.93210 <- plot_histogram(df.geog, 'Valeur.fonciere', title, 'Valeur foncière (\u20ac)', 'No de ventes', caption, summ$avg, summ$med, summ$mode, summ$max, nbins, filename)

df.geog <- df.geoc %>% filter(Code.postal=='93450')
summ <- get_stats(df.geog, 'Valeur.fonciere')
title <- build_title('Valeur foncière 93450', 'Ile St-Denis', year)
filename <- build_filename('valeur_fonciere_93450', commune, year)
vf_hist_plot.93450 <- plot_histogram(df.geog, 'Valeur.fonciere', title, 'Valeur foncière (\u20ac)', 'No de ventes', caption, summ$avg, summ$med, summ$mode, summ$max, nbins, filename)

df.hist <- get_summary_df(df.geoc, 'Code.postal',  'Valeur.fonciere')
title <- build_title('Valeur foncière médiane', commune, year)
filename <- build_filename('valeur_fonciere_mediane_par_code_postal', commune, year)
vf_par_code_postal_plot <-
  bar_plot(df.hist, 'Code.postal', 'med', title, 'Code postal', "Valeur foncière (\u20ac)", caption, filename)

df.monthly <- get_summary_df2(df.geoc, 'Month', 'Surface.cat', 'Valeur.fonciere')
title <- build_title('Valeur foncière mensuelle moyenne par surface', commune, year, max_vf)
filename <- build_filename('valeur_mensuelle_moyenne_par_surface', commune, year)
monthly_surface_avg_vf_plot <- facet_summary_data(df.monthly, 'avg', title, 'Mois', 'No de ventes', caption, filename, TRUE)

df.size <- get_summary_df(df.geoc, 'Surface.cat', 'Valeur.fonciere')

title <- build_title('Valeur foncière', commune, year)
filename <- build_filename('valeur_fonciere_mediane_par_surface', commune, year)
surface_med_vf_plot <- plot_summary_data(df.size, 'Surface.cat', 'med', title, 'Surface (m\u00b2)', 'Valeur foncière médiane (\u20ac)', caption, filename, TRUE)

title <- build_title('Valeur foncière', commune, year)
df.size <- get_summary_df2(df.geoc, 'Surface.reelle.bati', 'Code.postal', 'Valeur.fonciere')
vf_mediane_plot <- df.size %>% ggplot(aes(x=Surface.reelle.bati, y=med, color=as.factor(Code.postal))) +
  geom_point() +
  labs(title=title,
       caption=caption,
       x = 'Surface (m\u00b2)',
       y = 'VF médiane (\u20ac)',
       color = "Code postal") +
  scale_y_continuous(labels = comma) +
  geom_smooth(method = lm, se = FALSE, color = 'yellow4', size = 1, linetype='dashed')+
  stat_cor(label.x = 110, label.y = -10000, color = 'yellow4') +
  stat_regline_equation(label.x = 10, label.y = -10000, color = 'yellow4')+
  scale_color_brewer(palette='Dark2')
vf_mediane_plot
# title <- build_title('Valeur foncière moyenne par surface', commune, year, max_vf)
# filename <- build_filename('valeur_fonciere_moyenne_par_surface', commune, year)
#surface_avg_vf_plot <- plot_summary_data(df.size, 'Surface.cat', 'avg', title, 'Surface', 'Prix moyen (\u20ac)', filename, TRUE)
title <- build_title('Valeur foncière', commune, year)
filename <- build_filename('valeur_fonciere_par_surface', commune, year)
surface_vf_plot <- violin_plot(df.geoc, 'Surface.cat', 'Valeur.fonciere', title, 'Surface (m\u00b2)', "VF (\u20ac)", caption, filename, FALSE)
#VF by postal code
title <- build_title('Valeur foncière', commune, year)
filename <- build_filename('valeur_fonciere_par_CP', commune, year)
vf_par_code_postal_plot <-
  violin_plot(df.geoc, 'Code.postal', 'Valeur.fonciere', title, 'Code postal', "Valeur foncière (\u20ac)", caption, filename, FALSE)

# Surface plots ####
nbins <- 20
summ <- data.frame(avg=mean(df.geoc$Surface.reelle.bati),
                   med=median(df.geoc$Surface.reelle.bati),
                   mode=getmode(df.geoc$Surface.reelle.bati),
                   max=max(df.geoc$Surface.reelle.bati),
                   min=min(df.geoc$Surface.reelle.bati),
                   stringsAsFactors=FALSE)
title <- build_title('Surface des appartements', commune, year)
filename <- build_filename('surface', commune, year)
surface_hist_plot <- plot_histogram(df.geoc, 'Surface.reelle.bati', title, 'Surface réeelle batie', 'No de ventes', caption, summ$avg, summ$med, summ$mode, summ$max, nbins, filename)

title <- build_title('Surface des appartements', commune, year)
filename <- build_filename('surface_par_code_postal', commune, year)
caption <- build_caption(nsales, max_vf)
surface_par_code_postal_plot <-
  violin_plot(df.geoc, 'Code.postal', 'Surface.reelle.bati', title, 'Code postal', "Surface (m\u00b2)", caption, filename, FALSE)

df.monthly <- get_summary_df(df.geoc, 'Month', 'Surface.reelle.bati')
title <- build_title('Évolution mensuelle de la surface', commune, year)
filename <- build_filename('surface_mediane_mensuelle', commune, year)
monthly_med_surface_plot <- plot_summary_data(df.monthly, 'Month', 'med', title, 'Mois', 'Surface médiane (m\u00b2)', caption , filename, TRUE)

# Nombre de pièces plots ####
nbins <- 6
summ <- get_summary_df(df.geoc, 'Commune', 'Nombre.pieces.principales')
title <- build_title('No de pièces', commune, year, max_vf)
filename <- build_filename('nbre_de_pieces', commune, year)
npieces_hist_plot <- plot_histogram(df.geoc, 'Nombre.pieces.principales', title, 'Nombre de pièces', 'No de ventes', caption, summ$avg, summ$med, summ$mode, summ$max, nbins, filename)

df.hist <- get_summary_df(df.geoc, 'Code.postal', 'Nombre.pieces.principales')
title <- build_title('No de pièces', commune, year)
filename <- build_filename('npieces_médian_par_code_postal', commune, year)
no_de_pieces_par_code_postal_plot <-
  bar_plot(df.hist, 'Code.postal', 'med', title, 'Code postal', "Nombre de pièces médian", caption, filename)

df.monthly <- get_summary_df(df.geoc, 'Month', 'Nombre.pieces.principales')
title <- build_title('No pièces', commune, year, max_vf)
filename <- build_filename('nbre_de_pieces_mediane_mensuelle', commune, year)
monthly_med_npieces_plot <- plot_summary_data(df.monthly, 'Month', 'med', title, 'Mois', 'No pièces (médiane)', caption, filename, TRUE)

# prix au m2 plots ####
nbins <- 20
summ <- get_stats(df.geoc, 'Valeur.par.metre')
summ
title <- build_title('Valeur foncière au m\u00b2', commune, year)
filename <- build_filename('ppm_volume', commune, year)
ppm_hist_plot <- plot_histogram(df.geoc, 'Valeur.par.metre', title, '\u20ac/m\u00b2', 'No de ventes', caption, summ$avg, summ$med, summ$mode, summ$max, nbins, filename)
normality_test(df.geoc$Valeur.par.metre, title)

df.monthly <- get_summary_df(df.geoc, 'Month', 'Valeur.par.metre')
title <- build_title('Valeur foncière médian au m\u00b2', commune, year)
filename <- build_filename('ppm_mediane_mensuelle', commune, year)
plot_summary_data(df.monthly, 'Month', 'med', title, 'Mois', 'Prix médian au m\u00b2 (\u20ac)', caption, filename, TRUE)

df.size <- get_summary_df(df.geoc, 'Surface.cat', 'Valeur.par.metre')
title <- build_title('Valeur foncière au m\u00b2', commune, year)
filename <- build_filename('ppm_mediane_par_surface', commune, year)
plot_summary_data(df.size, 'Surface.cat', 'med', title, 'Surface (m\u00b2)', 'Prix médian (\u20ac)', caption, filename, FALSE)

title <- build_title('Valeur foncière au m\u00b2', commune, year)
filename <- build_filename('ppm_par_code_postal', commune, year)
ppm_par_code_postal_plot <-
  violin_plot(df.geoc, 'Code.postal', 'Valeur.par.metre', title, 'Code postal', "VF (\u20ac)", caption, filename, FALSE)

title <- build_title('Valeur foncière au m\u00b2', commune, year)
filename <- build_filename('ppm_par_surface', commune, year)
ppm_par_surface_plot <-
  violin_plot(df.geoc, 'Surface.cat', 'Valeur.par.metre', title, 'Surface (m\u00b2)', "VF/m\u00b2 (\u20ac)", caption, filename, FALSE)

ppm_par_surface_code_postal_plot <-
  violin_plot(df.geoc, 'Code.postal', 'Valeur.par.metre', title, 'Code postal', "VF/m\u00b2 (\u20ac)", caption, filename, TRUE)
colnames(df.geoc)
# Price/m2 by month and surface ####
df.monthly <- get_summary_df(df.geoc, 'Month', 'Valeur.par.metre')
title <- build_title('Évolution mensuelle de la valeur foncière au m\u00b2', commune, year)
ppm_monthly_plot <- plot_summary_data(df.monthly, 'Month', 'med', title, 'Mois', 'VF médiane/m\u00b2', caption, filename, TRUE)

df.monthly2 <- get_summary_df2(df.geoc, 'Month', 'Code.postal', 'Valeur.par.metre')
title <- build_title('Évolution de la valeur foncière au m\u00b2', commune, year)

ppm_monthly_plot_by_CP <- df.monthly2 %>%
  ggplot(aes(Month, med, group=Code.postal, color=as.factor(Code.postal))) +
  geom_line(size = .5) +
  geom_point() +
  labs(title =title,
       caption = caption,
       color = 'Code postal',
       x = 'Mois',
       y = 'VF médiane/m\u00b2') +
  scale_x_continuous(breaks = seq(1, 12)) +
  scale_color_brewer(palette='Dark2') +
  geom_smooth(method='lm', se=FALSE, size=.25)
ppm_monthly_plot_by_CP

summary <- get_summary_df2(df.geoc, 'Surface.cat', 'Month', 'Valeur.par.metre')

title <- build_title('Évolution mensuelle du prix au m\u00b2 par surface', commune, year)
filename <- build_filename('ppm_mediane_mensuelle_par_surface', commune, year)
monthly_surface_med_ppm_plot <- plot_summary_data2(summary, 'Month', 'Surface.cat', 'med', title, 'Mois', 'VF/m\u00b2 (\u20ac)', caption, filename)

# Save environment ####
save.image(file='StDenisEnvironment.RData')

