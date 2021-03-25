# Look for missing higher taxa

# extract higher taxa for next set of review
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]

# TODO can we streamline this process and make sure it repeats for all columns in higher_taxa?
suggested_adds <- data.frame (taxonName  = c(),
                              taxonRank = c()) # Suggested additions to the names list
suggested_remove <- data.frame (taxonName  = c(),
                            taxonRank = c()) # Suggested removals from the names list

# genus
# Get unique list of genera from df and higher_taxa
unique_df <- unique(df$genus) # unique genera in the working data frame
unique_ht <- unique(higher_taxa$genus) # unique genera in the higher taxa data frame
unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
# compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
no_higher_taxon <- unique_df[unique_df %!in% unique_ht] # there is not a unique genus entry for these genera
taxa_not_used <- unique_ht[unique_ht %!in% unique_df] # there is a genus taxon, but no species for it
suggested_append <- as.data.frame(no_higher_taxon) # create data frame for suggested adds to missing_taxa
suggested_append$taxonRank <- "genus" # add reason
suggested_adds <- rbind(suggested_adds,suggested_append) # add rows to suggested_adds data frame
unused_taxa <- as.data.frame(taxa_not_used) # create data frame for suggested removals
unused_taxa$taxonRank <- "genus" # add reason
suggested_remove <- rbind(suggested_remove, unused_taxa) # add rows to missing_taxa data frame
# End genus
# Repeat for other levels up....

