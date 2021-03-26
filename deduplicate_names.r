# flag duplicate canonical names
df$reason <- c(duplicated(df$canonicalName, fromLast = TRUE)  | duplicated(df$canonicalName))

# extract duplicates from less-favored sources
# FMNH
duplicates <- df[which(grepl('^FMNH', df$TPTdataset) == TRUE &
                         df$reason == TRUE), ] # FMNH canonical names that are duplicated in other sources
FMNH_dupes <- nrow(duplicates)
# duplicates <- rbind(duplicates, duplicate_append)
duplicates_not <- df[which(grepl('^FMNH', df$TPTdataset) == TRUE &
                             df$reason == FALSE), ] # FMNH canonical names that are not duplicated in other sources
df <- df[which(grepl('^FMNH', df$TPTdataset) == FALSE), ] # remove all FMNH rows
df <- rbind(df, duplicates_not) # return FMNH rows that are not dupes to working file

# Missing data check FMNH
if(TOTAL != nrow(df) + 
   nrow(duplicates) +
   nrow(df_removed)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check FMNH

# NMNH
duplicate_append <- df[which(grepl('^NMNH', df$TPTdataset) == TRUE &
                               df$reason == TRUE), ] # NMNH canonical names that are duplicated in other sources
NMNH_dupes <- nrow(duplicate_append)
duplicates <- rbind(duplicates, duplicate_append)
duplicates_not <- df[which(grepl('^NMNH', df$TPTdataset) == TRUE &
                                    df$reason == FALSE), ] # NMNH canonical names that are not duplicated in other sources
df <- df[which(grepl('^NMNH', df$TPTdataset) == FALSE), ] # remove all NMNH rows
df <- rbind(df, duplicates_not) # return NMNH rows that are not dupes to working file

# Missing data check NMNH
if(TOTAL != nrow(df) + 
   nrow(duplicates) +
   nrow(df_removed)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check NMNH

# BYU
duplicate_append <- df[which(grepl('^BYU', df$TPTdataset) == TRUE &
                                df$reason == TRUE), ] # BYU canonical names that are duplicated in other sources
BYU_dupes <- nrow(duplicate_append)
duplicates <- rbind(duplicates, duplicate_append)
duplicates_not <- df[which(grepl('^BYU', df$TPTdataset) == TRUE &
                              df$reason == FALSE), ] # BYU canonical names that are not duplicated in other sources
df <- df[which(grepl('^BYU', df$TPTdataset) == FALSE), ] # remove all BYU rows
df <- rbind(df, duplicates_not) # return BYU rows that are not dupes to working file

# Missing data check NMNH
if(TOTAL != nrow(df) + 
   nrow(duplicates) +
   nrow(df_removed)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
   print('all records accounted for')
} # End of Missing data check NMNH

# GBIF
duplicate_append <- df[which(duplicated(df$canonicalName)),] # GBIF canonical names that are duplicated in other sources
GBIF_dupes <- nrow(duplicate_append)
duplicates <- rbind(duplicates, duplicate_append)
df <- df[which(!duplicated(df$canonicalName)),] # deduplicated list

# Missing data check Final
if(TOTAL != nrow(df) + 
   nrow(duplicates) +
   nrow(df_removed)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
} else {
  print('all records accounted for')
} # End of Missing data check Final

# combine removed records
duplicates$reason <- "duplicate"
df_removed <- rbind(df_removed, duplicates)

# add duplicates and uniques to source summary dataframe
BYU <- nrow(df[grep("BYU*", df$TPTdataset), ])
FMNH <- nrow(df[grep("FMNH*", df$TPTdataset), ]) # number of deduplicated records in FMNH Source
GBIF <- nrow(df[grep("GBIF*", df$TPTdataset), ]) # number of deduplicated records in GBIF Source
NMNH <- nrow(df[grep("NMNH*", df$TPTdataset), ]) # number of deduplicated records in NMNH Source
TOTAL <- BYU + FMNH + GBIF + NMNH # number of deduplicated records in all sources
TOTAL_Dupes <- BYU_dupes + FMNH_dupes + GBIF_dupes + NMNH_dupes # number of duplicated records in all sources
source_summary$removed_names <- c(BYU_dupes, FMNH_dupes, GBIF_dupes, NMNH_dupes, TOTAL_Dupes) # create column in sources data frame for number of duplicate names

source_summary$unique_names <- c(BYU, FMNH, GBIF, NMNH, TOTAL)# create column in sources data frame for number of unique names


write.csv(df,"~/GitHub/ixodes-tpt/output/TPT_Siphonaptera.csv", row.names = FALSE) # write out final list
write.csv(df_removed,"~/GitHub/ixodes-tpt/output/TPT_Siphonaptera_duplicates.csv", row.names = FALSE) # write out file of duplicates removed
write.csv(source_summary,"~/GitHub/ixodes-tpt/output/Siphonaptera_sources.csv", row.names = FALSE) # write out reconciliation

print("proceed to levenshtein distance name matching")