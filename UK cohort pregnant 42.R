library(readr)

uk42_persongrid<- read_delim("bcs70_2012_persongrid.tab", "\t", escape_double = FALSE, trim_ws = TRUE)

save(uk42_persongrid, file = "uk42_persongrid.rda")


# --------------------------------------------------- #

# ---- Libraries -----
library(anchors)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(psych)
library(ryouready)
library(compare)
# ---- Load data -----

load("uk42_persongrid.rda")

#Extract variables on birth and death of children
uk42_child <- 
  uk42_persongrid %>%
  filter(B9GRTOK == 4 & B9GDOBY > 0) %>%
  select(BCSID, B9GDOBY, B9GDOBM, B9GDIEDY, B9GDIEDM)

#Change names of variables
names(uk42_child) <- c("id", "byear", "bmonth", "dyear", "dmonth")

#Change all - values to NA
uk42_child <- uk42_child %>%
  mutate_all(funs(replace(., . == -1, NA)))

uk42_child <- uk42_child %>%
  mutate_all(funs(replace(., . == -9, NA)))

# Add 0 in front of 1 lenght months
uk42_child$bmonth <- sprintf("%02s", uk42_child$bmonth)

#Create dob variable as date format
uk42_child$dob <- as.Date(paste0("01", uk42_child$bmonth, uk42_child$byear), format = "%d%m%Y")

# Create a new df with first birth for each ID
uk42_first_birth <- uk42_child %>%
  group_by(id) %>%
  mutate(min_42 = min(dob)) %>%
  select(id, min_42) %>%
  distinct()

#Combine first birth from age 30 and age 42 as well as interview date at 30
uk_combined_first_birth <- full_join(uk42_first_birth, uk30_workfile[c("bcsid", "min", "intdate")], by = c("id" = "bcsid"))
names(uk_combined_first_birth)[3] <- "min_30"
uk_combined_first_birth$intdate <- as.Date(uk_combined_first_birth$intdate, format = "%d%m%Y")

#Remove double NA in date of giving birth
uk_combined_first_birth <- uk_combined_first_birth[!is.na(uk_combined_first_birth$min_42) | !is.na(uk_combined_first_birth$min_30),]

#Change day of birth in 30 column to 01 for comparison with age 42 & change into date format
substr(uk_combined_first_birth$min_30, start = 1, stop = 2) <- "01"
uk_combined_first_birth$min_30 <- as.Date(uk_combined_first_birth$min_30, format = "%d%m%Y")

# semi_join to find cases that have given a birth date before the interview date when they were 30 but didn't when they were 30,
# anti_join to find the opposite
temp <- uk_combined_first_birth[!is.na(uk_combined_first_birth$min_42) & is.na(uk_combined_first_birth$min_30),] %>%
  filter(min_42 < intdate)

semi_join(temp, uk30_workfile, by = c("id" = "bcsid"))
