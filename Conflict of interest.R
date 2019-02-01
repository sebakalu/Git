install.packages("here")
install.packages("readxl")
install.packages("tidyr")
install.packages("dplyr")
install.packages("openxlsx")

library(here)
library(readxl)
library(tidyr)
library(dplyr)
library(openxlsx)



#Import the file
file_location <- here()
xl_file <- read_excel(path = paste0(file_location,"/EMF Reviewer selection Conflicts of Interest.xlsx"))



# Make list of lists with reviewers:
xl_file$X__6 <- strsplit(xl_file$X__6, I(','))
xl_file$X__6 <- lapply(xl_file$X__6, trimws, "b")
xl_file$X__6 <- as.list(xl_file$X__6)

#Create list of lists with reviewers and conflicting authors
reviewers <- vector("list", nrow(xl_file))



for(i in 1:nrow(xl_file)) {
  names(reviewers)[i] <- paste0(xl_file$X__4[i],"_",i)
  reviewers[i] <- data.frame(xl_file$X__6[i], stringsAsFactors = F)
}

#Make Dataframe with reviewers 
reviewers_df <- data.frame(do.call(cbind,reviewers))
rm(reviewers)

#Remove all NA & Surname column
reviewers_df <- reviewers_df[!sapply(reviewers_df, function(x) all(is.na(x)))]
reviewers_df <- reviewers_df[2:ncol(reviewers_df)]

#Make long df
reviewers_df <- gather(reviewers_df, reviewer, author)
reviewers_df <- distinct(reviewers_df)

# Separate reviewer name
reviewers_df <- reviewers_df %>%
  separate(reviewer, c("reviewer", "row"), sep = "_")

# # Add 1 to row (because first row wasn't imported)
reviewers_df$row <- as.numeric(reviewers_df$row) + 1

#make list of lists with authors
authors_df <- xl_file[1:3,] %>%
  select(contains("EMJS"))
for(i in 1:ncol(authors_df)){
authors_df[i] <- lapply(authors_df[i], strsplit, I("\r\n"))
}



# Make Authors df long
authors_df <- gather(authors_df, project, author)
authors_df <- unnest(authors_df)
authors_df$author <-  trimws(authors_df$author, "b")

# Create conflict dataframe
conflicts <- inner_join(reviewers_df, authors_df, by = "author")

# Remove unneeded dfs
rm(authors_df)
rm(reviewers_df)

# Turn reviewers back into string
for(i in 1:nrow(xl_file)) {
  xl_file$X__6[i] <- paste0(unlist(xl_file$X__6[i]), collapse = ", " )
}

#Prepare the conflicts df
conflicts$author <- "CONFLICT"
conflicts <- distinct(conflicts)
conflicts <- conflicts[,-3]
names(conflicts) <- c("Reviewer Surname", "Row", "Application")

#Write to Excel sheet
write.xlsx(conflicts, "Conflicts.xlsx", sheetName="Conflicts")