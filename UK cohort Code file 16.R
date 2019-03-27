# ---- UK COHORT, AGE 16 ---- #
# ---- Code file, Sebastian Kalucza ----------------------- #

# ---- Importing data (once) ----
library(readr)

uk16_derived <- read_delim("bcs4derived.tab", "\t", escape_double = FALSE, trim_ws = TRUE)
uk16<- read_delim("bcs7016x.tab", "\t", escape_double = FALSE, trim_ws = TRUE)

save(uk16, file = "uk16.rda")
save(uk16_derived, file = "uk16_derived.rda")

# --------------------------------------------------- #

# ---- Libraries -----
library(anchors)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(psych)


# ---- Load data -----

load("uk16.rda")
load("uk16_derived.rda")

# --- functions and objects -----
na_recode <- function(x){ifelse(x < 0, NA, x)}


# --- Extract wanted columns

uk16_workfile <- uk16_derived[,c(1,10,12)]
names(uk16_workfile) <- tolower(names(uk16_workfile))

variable_list <- c("bcsid", "sex86", "hd4.1", "hd4.2", "hc10g.3", "hc11a.2", "hc11", "jb33a10", "c5e8", "c5e2", "c5e14", paste0("c5i", 1:12), 
                   "c6.3a", "c6.3b", "c6.4a", "c6.4b", "f20tot_i", "f20tot_o", "f45", paste0("f66", letters[1:6]), "gb4a1",
                   "gb4a3", "gb6a", "gb5a", "jc9", paste0("c5r", 1:15), "gd2", "ge4a", "rd4.1", "rd2.1", "ha8", "hb1", "hb9.2", "hb9.3",
                   "hb9.5", "hb9.6", paste0("hb11a.", 1:10), "ha7.1", "hc16a.1", "hc14.9", "hc18a.1", "hc19a.1", "hd21.1", "hd21.3")


uk16_workfile <- left_join(uk16[variable_list], uk16_workfile)

#Change variable names in uk16_workfile

variable_names <- c("id", "sex", "drink_first", "drink_first_friends", "found_guilty", "cautioned", "cautioned_police", "married_next_year", 
                    "importance_marriage", "importance_job", "importance_children", "si_concentration", "si_decisions", "si_face_problems", 
                    "si_happy", "si_enjoy", "si_useful", "si_sleep", "si_strain", "si_difficulties", "si_depressed", "si_confidence", "si_worthless",
                    "siblings_older_h", "siblings_younger_h", "siblings_older_e", "siblings_younger_e", "sports_school", "sports_other", 
                    "get_on", "share_p_school", "share_p_money", "share_p_family", "share_p_health", "share_p_friends", "share_p_career",
                    "parent_shout", "parent_hit", "approval_friends", "approval_free_time", "shared_interests", "w_p_visits", "w_p_games", "w_p_pub",
                    "w_p_outdoor", "w_p_sports", "w_p_hobby_o", "w_p_hobby_i", "w_p_shopping", "w_p_holiday", "w_p_dance", "w_p_church",
                    "w_p_cinema", "w_p_meal", "w_p_restaurant", "w_p_music", "religion", "5_years", "weight", "height", "pill_use", "boy_girlfriend",
                    "sex_once", "sex_several", "sex_one_p", "sex_several_p", "contrac_withdraw", "contrac_condom", "contrac_safe", "contrac_pill", 
                    "contrac_jelly", "contrac_other", "contrac_no_sex", "contrac_luck", "contrac_none_listed", "contrac_unknown", "menarche_first", "stolen_from",
                    "discriminated", "forced", "threatened", "alcohol_father", "alcohol_mother", "malaise_total", "malaise_grouped"
                    )

names(uk16_workfile) <- variable_names

# Recode yes/no variables to 1/0
uk16_workfile <- replace.value(uk16_workfile, c("found_guilty", "cautioned_police", "married_next_year", "sex_once", "sex_several", "sex_one_p", "sex_several_p", 
                               "contrac_withdraw", "contrac_condom", "contrac_safe", "contrac_pill", "contrac_jelly", "contrac_other", "contrac_no_sex",
                               "contrac_luck", "contrac_none_listed", "contrac_unknown"), from = 2, to = 0)

uk16_workfile <- replace.value(uk16_workfile, c("stolen_from", "discriminated", "forced", "threatened"), from = 1, to = 0)
uk16_workfile <- replace.value(uk16_workfile, c("stolen_from", "discriminated", "forced", "threatened"), from = 2, to = 1)


# Create sum for self image questions -----
uk16_workfile$si_sum <- rowSums(uk16_workfile[grep("si_", names(uk16_workfile))])

# Create sum of total siblings -----
uk16_workfile$siblings_total <- rowSums(uk16_workfile[grep("siblings", names(uk16_workfile))])

# Set all sums of siblings more than 5 to 5
uk16_workfile$siblings_total[uk16_workfile$siblings_total > 4] <- 5


# Determine birth order 
uk16_workfile$birth_order <- 1 + rowSums(uk16_workfile[grep("siblings_younger", names(uk16_workfile))])


# Create mean for activity with parents questions -----
uk16_workfile$shared_activities_parents <- rowMeans(uk16_workfile[grep("w_p_", names(uk16_workfile))])

# Create BMI -----
uk16_workfile <- uk16_workfile %>%
  mutate(bmi = weight / (height^2))

# Recode missings -----

uk16_workfile[uk16_workfile < 0 ] <- NA


# See who is trusted the most, creates a table with how many each choice was picked, and orders them in decreasing order -----

for(i in 1:6){
# Easier way to use paste() to create column  subsetting
  uk16_workfile[[paste0("most_trusted_", i)]] <- apply(uk16_workfile[grep("share_p_", names(uk16_workfile))], MARGIN = 1, 
                                     FUN = function(x) { 
                                       res <-  names(sort(table(x), decreasing = T))[i] 
                                       ifelse(is.null(res), NA_character_, res)
                                       }
                                     ) %>% c(recursive=TRUE)

# Creates columns with count of how often each choice was picked for the 6 questions ------
  uk16_workfile[[paste0("most_trusted_count_", i)]] <-  apply(uk16_workfile[grep("share_p_", names(uk16_workfile))], MARGIN = 1, 
                                     FUN = function(x) { 
                                       res <-  sort(table(x), decreasing = T)[i] 
                                       ifelse(is.null(res), NA_character_, res)
                                      }
                                    ) %>% c(recursive=TRUE)
}

for(i in 2:6){
  # Checks whether the count of who they come to the most with problems is the same for several choices and sets those that
  # aren't to NA -----
  uk16_workfile[[paste0("most_trusted_count_", i)]][uk16_workfile$most_trusted_count_1 != uk16_workfile[paste0("most_trusted_count_", i)]] <- NA
  
  #Sets all values of most trusted person that doesn't have as many counts as the one before to NA ------
  uk16_workfile[[paste0("most_trusted_", i)]][is.na(uk16_workfile[paste0("most_trusted_count_", i)])] <- NA
}



#Remove all but the first most_trusted_count column
count_columns <- paste0("most_trusted_count_", 2:6)
uk16_workfile <- uk16_workfile[,!(names(uk16_workfile) %in% count_columns)]
rm(count_columns)
names(uk16_workfile)[92] <- "most_trusted_count"

          
# ---- Describe outcome measures -----

ggplot(uk42, aes(x = B9SCQ31A, color = as.factor(B9CMSEX))) + 
  theme_minimal() +
  geom_line(aes(fill = ..count..),  stat = "bin", binwidth = 1) +
  labs(x = "Warwick Edinburgh Mental Wellbeing Item 1") +
  scale_color_manual(name = "Sex", labels= c("Men", "Women"), values = c("1" = "cadetblue4", "2" = "firebrick2"))

# Cronbachs Alpha

alpha.war <- alpha(x = warwick[-(1:2)]) # 0.92

# Distribution of items

item.titles <- c("feeling optimistic about the future", "feeling useful", "feeling relaxed", 
                 "feeling interested in other people", "had energy to spare", 
                 "dealing with problems well", "thinking clearly", "feeling good about myself",
                 "feeling close to other people", "feeling confident", "able to make up my own mind about things",
                 "feeling loved", "interested in new things", "feeling cheerful")

names(warwick)[3:16] <- item.titles

warwick <- gather(warwick, "item", "score", -BCSID, -B9CMSEX)

ggplot(warwick, aes(x = score, color = as.factor(B9CMSEX))) + 
  theme_minimal() +
  geom_line(aes(),  stat = "bin", binwidth = 1) +
  facet_wrap(~ item) +
  labs(x = "Warwick Edinburgh Mental Wellbeing Item 1-14. (I've been...)") +
  scale_color_manual(name = "Sex", labels= c("Men", "Women"), values = c("1" = "cadetblue4", "2" = "firebrick2"))


# Distribution of total scores

ggplot(warwick, aes(x = score, color = as.factor(B9CMSEX))) + 
  theme_minimal() +
  geom_line(aes(),  stat = "bin", binwidth = 1) +
  labs(x = "Warwick Edinburgh Mental Wellbeing Item 1") +
  scale_color_manual(name = "Sex", labels= c("Men", "Women"), values = c("1" = "cadetblue4", "2" = "firebrick2"))

describe(uk42$BD9WEMWB) # 18% missing

ggplot(uk42, aes(x = BD9WEMWB, group = as.factor(B9CMSEX))) + 
  theme_minimal() +
  geom_density(aes(color = as.factor(B9CMSEX))) +
  labs(title = "Distribution of Warwick Edinburgh Mental Wellbeing Scale, age 42", x = "Warwick Edinburgh Mental Wellbeing Score") +
  scale_color_manual(name = "Sex", labels= c("Men", "Women"), value s = c("1" = "cadetblue4", "2" = "firebrick2"))


#--------------------------------------------------#
# Mental health and behavioural measures at 16 ----

# Malaise Score
uk16_mal <- select(uk16_derived, 1, 10:14)
uk16_mal[2:6] <- lapply(uk16_mal[2:6], na_recode)

uk16sex <- select(uk16, bcsid, sex86)
uk16_mal <- left_join(uk16_mal, uk16sex, by = c("BCSID" = "bcsid"))

# distribution malaise score among 16 year old females
uk16_mal%>%filter(sex86 == 2)%>%ggplot(aes(x = BD4MAL)) + 
  theme_minimal() +
  geom_line(aes(),  stat = "bin", binwidth = 2) +
  geom_vline(xintercept = 14, linetype = "dotted",  color = "firebrick2") +
  labs(x = "Malaise Score, women age 16. (Dotted line = grouping cut off)")

# distribution malaise score, Mothers of 16 year old females
uk16_mal%>%filter(sex86 == 2)%>%ggplot(aes(x = BD4MMAL)) + 
  theme_minimal() +
  geom_line(aes(),  stat = "bin", binwidth = 2) +
  geom_vline(xintercept = 11, linetype = "dotted", color = "skyblue4") +
  geom_vline(xintercept = 14, linetype = "dotted", color = "firebrick2") +
  labs(x = "Malaise Score, mothers of women age 16. (Dotted line = grouping cut off A resp B)") 

uk16_mal%>%filter(sex86 == 2)%>%Hmisc::describe()
# MAL missing 2683/5800 (46.2%)
# MMAL missing 1421/5800 (24.5%)

# ---- Rutter Score ----

rutter <- select(uk16_derived, BCSID, BD4RUTT, BD4MRUTG)
rutter <- left_join(rutter, uk16sex, by = c("BCSID" = "bcsid"))

  # Distribution of Rutter

Hmisc::describe(rutter$BD4MRUTG)

rutter[2:3] <- lapply(rutter[2:3], na_recode)

rutter%>%filter(sex86 == 2)%>%ggplot(aes(x = BD4RUTT)) + 
  theme_minimal() +
  geom_line(aes(),  stat = "bin", binwidth = 2) +
  geom_vline(xintercept = 7, linetype = "dotted",  color = "skyblue4") +
  geom_vline(xintercept = 12, linetype = "dotted",  color = "orange2") +
  geom_vline(xintercept = 38, linetype = "dotted",  color = "firebrick3") +
  labs(x = "Rutter Score, women age 16. (Dotted line = grouping cut off)")

# Missing Rutter (female) = 1803/5800 (31.1%)

# ---- Vocabulary (standardized reading score and reading age) ----

vocab <- select(uk16_derived, BCSID, BD4READ, BD4RDAGE)
vocab <- left_join(vocab, uk16sex, by = c("BCSID" = "bcsid"))
vocab[2:3] <- lapply(vocab[2:3], na_recode)

vocab%>%filter(sex86 == 2)%>%Hmisc::describe() # Missing BD4READ 4408/5800 (76%), Missing reading age 3126/5800 (53.9%)


# ---- Social Class ----

ses <- select(uk16_derived, BCSID, BD4PSOC)
ses$BD4PSOC <- na_recode(ses$BD4PSOC)
ses <- left_join(ses, uk16sex, by = c("BCSID" = "bcsid"))

ses%>%filter(sex86 == 2)%>%Hmisc::describe()

ses%>%filter(sex86 == 2)%>%ggplot(aes(x = BD4PSOC)) + 
  theme_minimal() +
  geom_bar(fill = "skyblue3") +
  labs(x = "Social class from fathers occupation, women age 16")
