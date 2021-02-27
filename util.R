# Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

preprocess <- function(df.raw){
  df.filter <- filter_data(df.raw, zipcodes, max_vf, local)
  df.filter <- feature_engineering(df.filter, max_ppm, surface_bins)
  return(df.filter)
}

load_geocoded_data <- function(file){
  df.geoc <- read.csv(file) # Load file
  df.geoc <- discretize_surface(df.geoc, surface_bins)
  return(df.geoc)
}

prep_data <- function(df){
  df.clean <- df
  # convert date factor to date
  df.clean$Date.dmy <- as.Date(df.clean$Date.dmy, format = "%Y-%m-%d")
  # extract day from date
  df.clean <- df.clean %>% mutate(day = format(Date.dmy, "%d"))
  # convert day to integer
  df.clean$day <- df.clean$day %>% as.integer(df.clean$day)

  # remove unecessary columns
  keep_cols <- c('Surface.reelle.bati', 'Nombre.pieces.principales',
                 'Month', 'day', 'lon', 'lat')
  # place class as first column
  df.clean <- df.clean %>% select(c(class, keep_cols))
  # df.clean <- dummy.data.frame(df.clean, names = "Code.postal")
  # Scale predictors
  df.clean[, -c(1)] <- scale(df.clean[, -c(1)], center = TRUE, scale = TRUE)
  glimpse(df.clean)

  return(df.clean)
}

feature_engineering <- function(df, max_ppm, surface_bins){
  # Extract month from date
  df <- df %>% mutate(Year = format(Date.dmy, "%Y"),
                      Month = format(Date.dmy, "%m"),
                      Month_Year = format(Date.dmy, "%Y-%m"))
  # Compute price per m2
  df <- df %>% mutate(Valeur.par.metre = Valeur.fonciere/Surface.reelle.bati)
  # Filter
  df  <- df %>% filter(Valeur.par.metre < max_ppm)
  discretize_surface(df, surface_bins)
  df <- discretize_surface(df, surface_bins)
  return(df)
}

# replace_surface_level <- function(level){
#   level <- gsub("(|]", "c", level)
#   level <- gsub(",", "-", level)
#   return(level)
# }
#
# replace_surface_level('(12,58]')

discretize_surface <- function(df, surface_bins){
  df$Surface.cat <- cut(df$Surface.reelle.bati, surface_bins)

  levels(df$Surface.cat)[levels(df$Surface.cat) ==   '(0,75]'] <- "0-75"
  levels(df$Surface.cat)[levels(df$Surface.cat) ==  '(75,150]'] <- "75-150"
  levels(df$Surface.cat)[levels(df$Surface.cat) ==  '(150,250]'] <- "150-250"
  levels(df$Surface.cat)[levels(df$Surface.cat) =='(250,400]'] <- "250-400"

  return(df)
}

get_stats<- function(df, colName){
  summ <- data.frame(avg=mean(df[,colName]),
                     med=median(df[,colName]),
                     mode=getmode(df[,colName]),
                     max=max(df[,colName]),
                     min=min(df[,colName]),
                     stringsAsFactors=FALSE)
  return(summ)
}

# Raw data cleanup
filter_data <- function(df, zipcodes, max_vf, local){
  df.filter <-
    df %>%
    filter(Code.postal %in% zipcodes) %>%
    filter(Valeur.fonciere <= max_vf) %>%
    filter(grepl('Maison', Type.local)) %>%
    select(-Type.local) # No need to keep column

  df.filter$No.voie <- as.factor(df.filter$No.voie)
  df.filter$Code.postal <- as.factor(df.filter$Code.postal)
  df.filter$Date.dmy <- as.Date(df.filter$Date.mutation, format = "%d/%m/%Y")

  #Cleanup
  df.filter  <- df.filter %>% select(-Date.mutation)

  # Remove any duplicate rows
  df.filter <- unique(df.filter)

  return(df.filter)
}

# Wrapper for df%>% group_by(groupName) %>% summarize(colName)
get_summary_df <- function(df, groupName, colName){
  summary_df <- df %>%
    group_by(!! sym(groupName)) %>%
    summarize(tot  = sum(!! sym(colName)),
              avg  = mean(!! sym(colName)),
              med  = median(!! sym(colName)),
              min  = min(!! sym(colName)),
              max  = max(!! sym(colName)),
              sd   = sd(!! sym(colName)),
              mode = getmode(!!sym(colName)),
              cnt  = n())
  if(groupName == 'Month'){
    summary_df$Month <- as.integer(summary_df$Month)
  }
  return(summary_df)
}

# Wrapper for df%>% group_by(groupName1, groupName2) %>% summarize(colName)
get_summary_df2 <- function(df, groupName1, groupName2, colName){
  summary_df <- df %>%
    group_by(!!sym(groupName1), !!sym(groupName2)) %>%
    summarize(tot  = sum(!! sym(colName)),
              avg  = mean(!! sym(colName)),
              med  = median(!! sym(colName)),
              min  = min(!! sym(colName)),
              max  = max(!! sym(colName)),
              sd   = sd(!! sym(colName)),
              mode = getmode(!!sym(colName)),
              cnt  = n())
  if(groupName1 == 'Month' || groupName2 == 'Month'){
    summary_df$Month <- as.integer(summary_df$Month)
  }
  return(summary_df)
}
