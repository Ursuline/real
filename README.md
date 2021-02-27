Plots the price per square meter of real-estate sales in a set of French zipcodes during a given time period

google api_key should be in google_id.R file

Script descriptions and sequence:

1. run pre_process.R to extract relevant columns from original txt file and save to csv
from then on, all work with csv file
raw data from: 
https://www.data.gouv.fr/fr/datasets/demandes-de-valeurs-foncieres/

2. geocode.R extracts lat-long from google for addresses specified in parameters.R and saves to csv

3. analysis.R performs basic statistical analysis of the data

4. geoplot.R makes geographic plots

Utility files:
parameters_???.R has the variable definitions for target data
plots.R has all plots except ggmaps
util.R has utilities common to all scripts

TO DO:
generalize analysis.R 
evolution.R in progress
