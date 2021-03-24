# import libraries
library(taxotools)

# extract higher taxa for next set of review
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]

# generate canonical name for genus and below
df <- cast_canonical(df,
                     canonical="canonical", 
                     genus = "genus", 
                     species = "specificEpithet",
                     subspecies = "infraspecificEpithet")

# canonical names for taxa ranked subgenus and above - get the lowest ranking term and put it here!
for(i in 1:nrow(higher_taxa)){
higher_taxa$canonical[i] <- ifelse(!is.na(higher_taxa$subgenus[i]), paste(higher_taxa$subgenus[i]),
                  ifelse(!is.na(higher_taxa$genus[i]), paste(higher_taxa$genus[i]),
                         ifelse(!is.na(higher_taxa$subfamily[i]), paste(higher_taxa$subfamily[i]),
                                ifelse(!is.na(higher_taxa$family[i]), paste(higher_taxa$family[i]),
                                       ifelse(!is.na(higher_taxa$superfamily[i]), paste(higher_taxa$superfamily[i]),
                                              ifelse(!is.na(higher_taxa$infraorder[i]), paste(higher_taxa$infraorder[i]),
                                                     ifelse(!is.na(higher_taxa$order[i]), paste(higher_taxa$order[i]),
                                                            ifelse(!is.na(higher_taxa$class[i]), paste(higher_taxa$class[i]),
                                                                   ifelse(!is.na(higher_taxa$phylum[i]), paste(higher_taxa$phylum[i]),
                                                                          ifelse(!is.na(higher_taxa$kingdom[i]), paste(higher_taxa$kingdom[i]), "review"))))))))))
}

# Extract rows from higher taxa that need review
flag <- c('review')
review_canonical <- higher_taxa[(higher_taxa$canonical %in% flag), ]
write.csv(review_canonical,"~/GitHub/ixodes-tpt/output/review_canonical.csv", row.names = FALSE) # these need review
higher_taxa <- higher_taxa[(higher_taxa$canonical %!in% flag), ]

# read in cleaned review file
review_canonical <- read.csv("~/GitHub/ixodes-tpt/input/review_canonical.csv", encoding="UTF-8")
higher_taxa <- rbind(higher_taxa, review_canonical)

# add higher taxa back to df for remainder of de-duplication
df <- rbind(higher_taxa, df)


# extract blank canonical names for review
df_review <- df[which(lapply(df$canonical, nchar) == 0), ]
df <- df[which(lapply(df$canonical, nchar) != 0),] # remove blank canonicals from working file

# flag duplicate canonical names
df$dup <- c(duplicated(df$canonical, fromLast = TRUE)  | duplicated(df$canonical))

# extract duplicates from less-favored sources
# ITIS
duplicates <- df[which(grepl('^ITIS', df$TPTID) == TRUE &
                   df$dup == TRUE), ] # ITIS canonical names that are duplicated in other sources
ITIS_dupes <- nrow(duplicates)
duplicates_not <- df[which(grepl('^ITIS', df$TPTID) == TRUE &
                 df$dup == FALSE), ] # ITIS canonical names that are not duplicated in other sources
df <- df[which(grepl('^ITIS', df$TPTID) == FALSE), ] # remove all ITIS rows
df <- rbind(df, duplicates_not) # return ITIS rowas that are not dupes to working file

# Missing data check ITIS
if(TOTAL != nrow(df) + 
   nrow(duplicates)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check ITIS

# FMNH
duplicate_append <- df[which(grepl('^FMNH', df$TPTID) == TRUE &
                         df$dup == TRUE), ] # FMNH canonical names that are duplicated in other sources
FMNH_dupes <- nrow(duplicate_append)
duplicates <- rbind(duplicates, duplicate_append)
duplicates_not <- df[which(grepl('^FMNH', df$TPTID) == TRUE &
                             df$dup == FALSE), ] # FMNH canonical names that are not duplicated in other sources
df <- df[which(grepl('^FMNH', df$TPTID) == FALSE), ] # remove all FMNH rows
df <- rbind(df, duplicates_not) # return FMNH rows that are not dupes to working file

# Missing data check FMNH
if(TOTAL != nrow(df) + 
   nrow(duplicates)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check FMNH

# NMNH
duplicate_append <- df[which(grepl('^NMNH', df$TPTID) == TRUE &
                               df$dup == TRUE), ] # NMNH canonical names that are duplicated in other sources
NMNH_dupes <- nrow(duplicate_append)
duplicates <- rbind(duplicates, duplicate_append)
duplicates_not <- df[which(grepl('^NMNH', df$TPTID) == TRUE &
                                    df$dup == FALSE), ] # NMNH canonical names that are not duplicated in other sources
df <- df[which(grepl('^NMNH', df$TPTID) == FALSE), ] # remove all NMNH rows
df <- rbind(df, duplicates_not) # return NMNH rows that are not dupes to working file

# Missing data check NMNH
if(TOTAL != nrow(df) + 
   nrow(duplicates)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check NMNH

# GBIF
duplicate_append <- df[which(grepl('^GBIF', df$TPTID) == TRUE &
                               df$dup == TRUE), ] # GBIF canonical names that are duplicated in other sources
GBIF_dupes <- nrow(duplicate_append)
duplicates <- rbind(duplicates, duplicate_append)
duplicates_not <- df[which(grepl('^GBIF', df$TPTID) == TRUE &
                                    df$dup == FALSE), ] # GBIF canonical names that are not duplicated in other sources
df <- df[which(grepl('^GBIF', df$TPTID) == FALSE), ] # remove all GBIF rows
df <- rbind(df, duplicates_not) # return GBIF rows that are not dupes to working file

# Missing data check GBIF
if(TOTAL != nrow(df) + 
   nrow(duplicates)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check GBIF

# BYU
duplicate_append <- df[which(grepl('^BYU', df$TPTID) == TRUE &
                               df$dup == TRUE), ] # BYU canonical names that are duplicated in other sources
BYU_dupes <- nrow(duplicate_append)
duplicates <- rbind(duplicates, duplicate_append)
duplicates_not <- df[which(grepl('^BYU', df$TPTID) == TRUE &
                                    df$dup == FALSE), ] # BYU canonical names that are not duplicated in other sources
df <- df[which(grepl('^BYU', df$TPTID) == FALSE), ] # remove all BYU rows
df <- rbind(df, duplicates_not) # return BYU rows that are not dupes to working file

# Missing data check BYU
if(TOTAL != nrow(df) + 
   nrow(duplicates)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check BYU

# CoL
duplicate_append <- df[which(duplicated(df$canonical)),] # CoL canonical names that are duplicated in other sources
CoL_dupes <- nrow(duplicate_append)
duplicates <- rbind(duplicates, duplicate_append)
df <- df[which(!duplicated(df$canonical)),] # deduplicated list

# Missing data check Final
if(TOTAL != nrow(df) + 
   nrow(duplicates)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check Final

# add duplicates and uniques to source summary dataframe
BYU <- nrow(df[grep("BYU*", df$TPTID), ])
COL <- nrow(df[grep("CoL*", df$TPTID), ]) # number of deduplicated records in CoL Source
FMNH <- nrow(df[grep("FMNH*", df$TPTID), ]) # number of deduplicated records in FMNH Source
GBIF <- nrow(df[grep("GBIF*", df$TPTID), ]) # number of deduplicated records in GBIF Source
ITIS <- nrow(df[grep("ITIS*", df$TPTID), ]) # number of deduplicated records in ITIS Source
NMNH <- nrow(df[grep("NMNH*", df$TPTID), ]) # number of deduplicated records in NMNH Source
TOTAL <- BYU + COL + FMNH + GBIF + ITIS + NMNH # number of deduplicated records in all sources
TOTAL_Dupes <- BYU_dupes + CoL_dupes + FMNH_dupes + GBIF_dupes + ITIS_dupes + NMNH_dupes # number of duplicated records in all sources
source_summary$duplicated_names <- c(BYU_dupes, CoL_dupes, FMNH_dupes, GBIF_dupes, ITIS_dupes, NMNH_dupes, TOTAL_Dupes)
source_summary$unique_names <- c(BYU, COL, FMNH, GBIF, ITIS, NMNH, TOTAL)


# separate sources
BYU_Siphonaptera <- df[which(grepl('Byu*', df$TPTID) == TRUE), ]
CoL_Siphonaptera <- df[which(grepl('Col*', df$TPTID) == TRUE), ]
FMNH_Siphonaptera <- df[which(grepl('Fmnh*', df$TPTID) == TRUE), ]
GBIF_Siphonaptera <- df[which(grepl('Gbif*', df$TPTID) == TRUE), ]
ITIS_Siphonaptera <- df[which(grepl('Itis*', df$TPTID) == TRUE), ]
NMNH_Siphonaptera <- df[which(grepl('Nmnh*', df$TPTID) == TRUE), ]

df <- rbind(BYU_Siphonaptera, CoL_Siphonaptera, FMNH_Siphonaptera, GBIF_Siphonaptera, ITIS_Siphonaptera, NMNH_Siphonaptera)
