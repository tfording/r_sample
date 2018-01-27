# NAME: Tyler Fording 
# DATE: Nov 4, 2017

#####################################
###         TASK 1                ###
#####################################

## Initially I like to try to plan out my work flow a so I have clear goals with the data. 
## Since this is a problem set, that is already sort of lined out for me, but I'm going to go through each 
## step and make an initial plan, identify functions that will be usful in completing the goal, 
## and write myself notes. To give myself a slight differentiation from the commented directions, I'll use
## '##' to indicate my notes.

# Load tidyverse library
library(tidyverse)

# Load "storms" data frome tidyverse
storms <- dplyr::storms
storms  ## To get a sense of what is in this tibble
## All of the columns already seem to be typed correctly, with the exception of the dates being broken into multiple columns.
## Depending on the use of the data I might consider grouping these columns into a single <dttm> column for the sake of readability.


# Create a subset of storms that start with letter "A"
## This will be a filter
Astorms <- storms %>%
  filter(grepl("A", name))
Astorms


# Create a subset of storms that start with letter "Z"
## This will also be a filter
Zstorms <- storms %>%
  filter(grepl("Z", name))
Zstorms


# Join these subsets data by row to make "AZstorms"
## This will require an outer join
AZstorms <- full_join(Astorms, Zstorms, all=TRUE) 
AZstorms

# test by column of new "AZstorms" to see if a column is all NA values
## sapply, is.na(), all() 
AZstorms %>%
  sapply(function(x)all(is.na(x)))
## No column is entirely NA values


# Create a "nameStat" column that combines the "name" and "status" column to create "name_status"
## This will require a mutate with a paste
AZstorms2 <- AZstorms%>%
  mutate(nameStat = paste(name, status, sep = '_'))  
## The status is a space separate string, so this leaves the column in the format (name_status status::AL011993_tropical depression) 
AZstorms2


# Create a "presWind" column that divides the "pressure" column by the "wind" column
## This will be a mutate(presWind = pressure/wind)
AZstorms3 <- AZstorms2%>% 
  mutate(presWind = pressure / wind)
AZstorms3


# determine the quantile values for the "presWind" column
quantile(AZstorms3$presWind, na.rm = TRUE)
## 0%        25%        50%        75%       100% 
## 6.146667  22.111111  33.300000  33.800000 101.500000  


# Create an "outlier" column that has "TRUE" for "presWind" values below 25% or above 75% and "FALSE otherwise
## This will require a conditional expression if, elif, else
## Base R's ifelse seems to be the way to go here.
AZstorms4 <- AZstorms3 %>%
  mutate(outlier = ifelse((presWind >= 22.11111) & (presWind <= 33.8), FALSE, TRUE))

  
# write the "AZstorms" table to a tsv file (tab-separated variable)
out_dir = "/Users/TFording/Desktop/AZstorms.tsv"
write_tsv(AZstorms4, out_dir, na = "NA", append=FALSE, col_names = TRUE)






















