library(readr)

uk30_derived <- read_delim("bcs6derived.tab", "\t", escape_double = FALSE, trim_ws = TRUE)
uk30<- read_delim("bcs2000.tab", "\t", escape_double = FALSE, trim_ws = TRUE)

save(uk30, file = "uk30.rda")
save(uk30_derived, file = "uk30_derived.rda")

# --------------------------------------------------- #

# ---- Libraries -----
library(anchors)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(psych)
library(ryouready)
# ---- Load data -----

load("uk30.rda")
load("uk30_derived.rda")

# --- Extract wanted columns

variable_list <- c("bcsid", "wave", "intdate", "dmsex", "everpreg", "anychd", "chd16f", "chd13f", "chdage3", "chdage4", "chd5_16", "chd16", "chd16n01", "chd16n02",
                   "chd16n03", "chd16n04", "chd16n05", "chd16n06", "chd16n07", "chd16n09", "chd16n10", "adptnu01", "adptnu02", "adptnu03", "adptnu04", "adptnu05", 
                   "adptnu06", "adptnu07", "adptnu08", "adptnu09", "adptnu10", "chd0_6", "ownchild"
                   )


# Combine all variables
uk30_workfile <- uk30[variable_list]
uk30_workfile <- left_join(uk30_workfile, uk30[grep("bcsid|^preg", names(uk30))])
uk30_workfile <- uk30_workfile %>%
  left_join(uk30[grep("bcsid|^preg", names(uk30))]) %>%
  left_join(uk30[c(1, 83:135)])

# Change name of the first pregnancy variable to include 1
uk30_workfile <- uk30_workfile %>%
                  rename(prege1 = prege, 
                   prega1 = prega,
                   preged1 = preged,
                   pregem1 = pregem,
                   pregey1 = pregey)


# Create birthday & min age
bday <- format(as.Date("1970-04-08"), "%d%m%Y")
minage <- format(as.Date("1983-04-08"), "%d%m%Y")


#Recode NAs to -1 for ease


uk30_workfile <- uk30_workfile %>%
  mutate_all(funs(replace(., is.na(.), -1)))

uk30_workfile <- uk30_workfile %>%
  mutate_all(funs(replace(., . == 9999, -1)))

uk30_workfile <- uk30_workfile %>%
  mutate_all(funs(replace(., . == 99, -1)))

uk30_workfile <- uk30_workfile %>%
  mutate_all(funs(replace(., . == 98, -1)))

uk30_workfile <- uk30_workfile %>%
  mutate_all(funs(replace(., . == 9998, -1)))

#change all columns to dbl
uk30_workfile[,4:317] <- lapply(uk30_workfile[,4:317], as.numeric)

# Change date format

uk30_workfile$intdate <- format(as.Date(uk30_workfile$intdate, format = "%d%m%Y"), "%d%m%Y")

# Change all dates to date format
for(i in c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36)){
  temp <- paste0("prege", i)
  
  uk30_workfile[uk30_workfile[temp] != -1,][[temp]] <-  sprintf("%08s", uk30_workfile[uk30_workfile[temp] != -1,][[temp]])
  uk30_workfile[uk30_workfile[temp] != -1,][[temp]] <- format(as.Date(uk30_workfile[uk30_workfile[temp] != -1,][[temp]], "%d%m%Y"), "%d%m%Y")
}

# Some cases of twin pregancies where live births are listed as 'baby 2' etc, while 'baby 1' is still birth -> 
# reorder so that this pregnancy appears as a live birth, not still birth

for(i in c(1, 6, 11, 16, 21, 26)){
  
  uk30_workfile$prega_temp_1 <- uk30_workfile[[paste0("prega", i)]]
  
  uk30_workfile["prega_temp_1"][!is.na(uk30_workfile[paste0("prega", 1)]) == 1 & !is.na(uk30_workfile[paste0("prega", i)]) == 2,] <- 1
  uk30_workfile[paste0("prega", i+1)][uk30_workfile[paste0("prega", i+1)] == 1 & uk30_workfile[paste0("prega", i)] == 2,] <- 2
  
  uk30_workfile[[paste0("prega", i)]] <- uk30_workfile$prega_temp_1

}


# Find rows with birth year present but birth date missing and add to a temporary df
# uk30_workfile_temp <- uk30_workfile[uk30_workfile[grep("^prege[[:digit:]]", names(uk30_workfile))] == -1 & uk30_workfile[grep("^pregey", names(uk30_workfile))] != -1,]



# Extract rows where birth date is missing  but birth year is available -------

# Create empty df with only ID and variables related to birth date
missing_bday <- uk30_workfile[0, ]

# Populate missing_bday with rows that have a birth year but missing birth date
for(i in c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36)){
  bdate <- paste0("prege", i)
  byear <- paste0("pregey", i)
  
  df <- uk30_workfile[uk30_workfile[bdate] == -1 & uk30_workfile[byear] != -1,]
  missing_bday <- bind_rows(missing_bday, df)
  
  }

# Keep only unique rows
missing_bday <- unique(missing_bday)

# Changes birth month to 7 if year is available but month is missing
for(i in c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36)){
  bmonth <- paste0("pregem", i)
  byear <- paste0("pregey", i)
  
  if(nrow(missing_bday[missing_bday[bmonth] == -1 & missing_bday[byear] != -1,]) > 0)
    missing_bday[missing_bday[bmonth] == -1 & missing_bday[byear] != -1,][[bmonth]] <- 7
  
}

# Changes birth day to 1 if year is available but day is missing
for(i in c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36)){
  birthday <- paste0("preged", i)
  byear <- paste0("pregey", i)
  
  if(nrow(missing_bday[missing_bday[birthday] == -1 & missing_bday[byear] != -1,]) > 0)
  missing_bday[missing_bday[birthday] == -1 & missing_bday[byear] != -1,][[birthday]] <- 1
  
}

# Add leading 0 to day and month
for(i in c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36)){
  birthday <- paste0("preged", i)
  bmonth <- paste0("pregem", i)
  
  missing_bday[[birthday]]<- sprintf("%02s", missing_bday[[birthday]])
  missing_bday[[bmonth]] <- sprintf("%02s", missing_bday[[bmonth]])
  
}

# Add birth date to all rows that have at least a birth year.
for(i in c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36)){
  birthdate <- paste0("prege", i)
  byear <- paste0("pregey", i)
  bmonth <- paste0("pregem", i)
  birthday <- paste0("preged", i)

  df <- missing_bday[missing_bday[birthdate] == -1 & missing_bday[byear] != -1,]
  missing_bday[[birthdate]] <- as.character(missing_bday[[birthdate]])
  date_format <- format(as.Date(paste0(df[[birthday]],df[[bmonth]], as.character(df[[byear]])), "%d%m%Y"), "%d%m%Y")
  missing_bday[missing_bday[birthdate] == -1 & missing_bday[byear] != -1,][[birthdate]] <- date_format
  
}


#change all columns to double & all columns with dates to date
uk30_workfile[-c(1, 3, grep("^prege[[:digit:]]", names(uk30_workfile)))] <- lapply(uk30_workfile[-c(1, 3, grep("^prege[[:digit:]]", names(uk30_workfile)))], as.numeric)
missing_bday[-c(1, 3, grep("^prege[[:digit:]]", names(missing_bday)))] <- lapply(missing_bday[-c(1, 3, grep("^prege[[:digit:]]", names(missing_bday)))], as.numeric)



#Return all fixed dates to uk30_workfile
uk30_workfile <- anti_join(uk30_workfile, missing_bday, by = "bcsid")
uk30_workfile <- bind_rows(uk30_workfile, missing_bday)




# Create temporary df with the outcomes for each pregnancy
temp <- uk30_workfile[c(1, grep("^prege[[:digit:]]", names(uk30_workfile)), grep("^prega[[:digit:]]", names(uk30_workfile)))]
temp[,paste0("outcome", c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36))] <- FALSE

for(i in c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36)){
  alive <- paste0("prega", i)
  bdate <- paste0("prege", i)
  outcome <- paste0("outcome", i)
  
  if(nrow(temp[temp[bdate] != -1 & temp[alive] == 1,]) > 0)
  temp[temp[bdate] != -1 & temp[alive] == 1,][outcome] <- TRUE
}
# Add the temp df back into the main df
uk30_workfile <- left_join(uk30_workfile, temp[c(1, 40:58)], by = "bcsid")

#Create rows for the births where the child was alive
uk30_workfile[,paste0("live_birth", c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36))] <- NA

for(i in c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36)){
  outcome <- paste0("outcome", i)
  bdate <- paste0("prege", i)
  live <- paste0("live_birth", i)
  uk30_workfile[uk30_workfile[outcome] == T & uk30_workfile[bdate] != -1,][[live]] <- uk30_workfile[uk30_workfile[outcome] == T & uk30_workfile[bdate] != -1,][[bdate]]
}

#Create rows for the births where the child didn't survive
uk30_workfile[,paste0("aborted_birth", c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36))] <- NA

for(i in c(1:8, 11:12, 16:17, 21:23, 26:27, 31, 36)){
  outcome <- paste0("outcome", i)
  bdate <- paste0("prege", i)
  live <- paste0("aborted_birth", i)
  uk30_workfile[uk30_workfile[outcome] == F & uk30_workfile[bdate] != -1,][[live]] <- uk30_workfile[uk30_workfile[outcome] == F & uk30_workfile[bdate] != -1,][[bdate]]
}

# Remove the outcome columns again
uk30_workfile <-  uk30_workfile[-grep("outcome", names(uk30_workfile))]

min_aborted_birth <- uk30_workfile[c(1, grep("^aborted_birth[[:digit:]]", names(uk30_workfile)))] %>%
  gather("pregnancy", "date", -bcsid) %>%
  group_by(bcsid) %>%
  filter(date > 0) %>%
  summarise( min_abort = format(min(as.Date(date, "%d%m%Y")),"%d%m%Y"))

min_birth <- uk30_workfile[c(1, grep("^live_birth[[:digit:]]", names(uk30_workfile)))] %>%
  gather("pregnancy", "date", -bcsid) %>%
  group_by(bcsid) %>%
  filter(date > 0) %>%
  summarise( min = format(min(as.Date(date, "%d%m%Y")),"%d%m%Y"))

uk30_workfile <- left_join(uk30_workfile, min_birth, by = "bcsid")
uk30_workfile <- left_join(uk30_workfile, min_aborted_birth, by = "bcsid")


# Remove outliers (before age of 13 & after interview date)
invalid <- uk30_workfile[!is.na(uk30_workfile$min),] %>%
  filter((as.Date(min, "%d%m%Y") < as.Date(minage, "%d%m%Y") | as.Date(min, "%d%m%Y") > as.Date(intdate, "%d%m%Y")))

uk30_workfile <- anti_join(uk30_workfile, invalid, by = "bcsid")

# Create age at first birth
uk30_workfile$age_at_first_birth <- as.numeric((as.Date(uk30_workfile$min, "%d%m%Y") - as.Date(bday, "%d%m%Y")) / 365.25)
uk30_workfile$age_at_first_aborted_birth <- as.numeric((as.Date(uk30_workfile$min_abort, "%d%m%Y") - as.Date(bday, "%d%m%Y")) / 365.25)

#Cleanup
rm(df, invalid, min_aborted_birth, min_birth, missing_bday, temp, alive, bdate, bday, birthdate, birthday, bmonth, byear,
   date_format, i, live, minage, outcome, variable_list)

