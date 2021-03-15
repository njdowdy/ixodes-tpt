# import
library(taxotools)

# load data
# df <- read.csv('input/Tick Taxonomy NMNH - Sheet1.csv')
df <- read.csv('input/Flea checklist-full taxonomy udpated 12.2019.csv', encoding = 'UTF-8')

# check encoding

# number of starting records for verification
starting_records <- nrow(df)

colnames(df) <- tolower(colnames(df)) # lower case column names

containsTaxonomy <- function(x) ifelse(!is.na(x), 
                                       grepl('domain', tolower(x), perl = TRUE) |
                                       grepl('kingdom', tolower(x), perl = TRUE) |
                                       grepl('regnum', tolower(x), perl = TRUE) |
                                       grepl('phylum', tolower(x), perl = TRUE) |
                                       grepl('class', tolower(x), perl = TRUE) |
                                       grepl('legio', tolower(x), perl = TRUE) |
                                       grepl('cohort', tolower(x), perl = TRUE) |
                                       grepl('order', tolower(x), perl = TRUE) |
                                       grepl('famil', tolower(x), perl = TRUE) |
                                       grepl('trib', tolower(x), perl = TRUE) |
                                       grepl('genus', tolower(x), perl = TRUE) |
                                       grepl('species', tolower(x), perl = TRUE) |
                                       grepl('sectio', tolower(x), perl = TRUE) |
                                       grepl('variet', tolower(x), perl = TRUE) |
                                       grepl('form', tolower(x), perl = TRUE) |
                                       grepl('clade', tolower(x), perl = TRUE) |
                                       grepl('series', tolower(x), perl = TRUE) |
                                       grepl('author', tolower(x), perl = TRUE) |
                                       grepl('publi', tolower(x), perl = TRUE) |
                                       grepl('year', tolower(x), perl = TRUE) |
                                       grepl('status', tolower(x), perl = TRUE) |
                                       grepl('rank', tolower(x), perl = TRUE) |
                                       grepl('name', tolower(x), perl = TRUE) |
                                       grepl('epithet', tolower(x), perl = TRUE)) 

nonTaxonomyColumns <- df[ , -which(!(names(df) %in% names(which(sapply(names(df), containsTaxonomy) == FALSE))))] # retain columns that do not relate to taxonomy
df <- df[ , -which(!(names(df) %in% names(which(sapply(names(df), containsTaxonomy) == TRUE))))] # remove columns that do not relate to taxonomy

# define DarwinCore conversion function
convert2DwC <- function(df_colname) {
                       x <- gsub('.*subspecies.*','infraspecificEpithet',df_colname)
                       x <- gsub('.*rank.*','taxonRank',x)
                       x <- gsub('.*author.*','scientificNameAuthorship',x)
                       x <- gsub('.*year.*','namePublishedInYear',x)
                       x
                       } # this needs work

colnames(df) <- convert2DwC(colnames(df)) # convert to DarwinCore terms

# ensure scientificNameAuthorship meets DarwinCore standard for ICZN
# source: xxxx
df$scientificNameAuthorship <- paste(df$scientificNameAuthorship,
                                     df$namePublishedInYear, sep = ', ')
# fix cases like: (Jordan & Rothschild), 1922
# regex: [x.replace(')', '')+')' for x in df$scientificNameAuthorship if re.search(r'[a-z]),', '', x)]
fixAuth <- function(x) ifelse(grepl('[a-z]),',x), paste(gsub(')', '',x),')',sep=''),x)
# hi Vijay!
# hi teresa!

# darwinCoreTaxonTerms <- c("kingdom", "phylum", "class", "order", "family",
#                           "genus", "subgenus", "species", "specificEpithet", 
#                           "scientificName", "infraspecificEpithet", "taxonRank",
#                           "higherClassification", "namePublishedInYear", 
#                           "scientificNameAuthorship", "taxonomicStatus", 
#                           "nomenclaturalStatus", "namePublishedIn")

# basic string cleaning functions
toproper <- function(x) ifelse(!is.na(x), paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))),NA) # fix capitalization
removePunc <- function(x) ifelse(!is.na(x), gsub('[[:punct:]]+','',x)) # remove punctuation (but not spaces)
containsPunc <- function(x) ifelse(!is.na(x), grepl('[[:punct:]]', x, perl = TRUE))
removeEncoding <- function(x) ifelse(!is.na(x), gsub("\xa0", "", x))

# siphonaptera dataset: remove '\xa0' chars from relevant fields
df$superfamily <- array(as.character(unlist(lapply(df$superfamily, removeEncoding))))
df$genus <- array(as.character(unlist(lapply(df$genus, removeEncoding))))

# NOTE: there are other encoding problems of accented characters
# I am not familiar with this encoding style
# search: '<e' in author names in particular

# fix capitalization for both genus and species
# ignore author, publication
# toproper all other fields
for(i in 1:ncol(df)) {
  name <- colnames(df)[i]
  if(grepl('infraspecificepithet', tolower(name), perl = TRUE)|
     grepl('variety', tolower(name), perl = TRUE) |
     grepl('form', tolower(name), perl = TRUE) |
     grepl('species', tolower(name), perl = TRUE)
  ) {
    df[,i] <- sapply(df[,i], tolower)
  } else if(grepl('author', tolower(name), perl = TRUE) |
            grepl('publi', tolower(name), perl = TRUE)) {
  } else {
    df[,i] <- sapply(df[,i], toproper)
  }
}

# select single-word specific_epithets
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)
'%!in%' <- function(x,y)!('%in%'(x,y)) 
no_genus_has_species <- df[which(lapply(df$species, name_length) != 0 & lapply(df$genus, name_length) == 0),] # no genus, but has species
no_genus_has_subspecies <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 & lapply(df$genus, name_length) == 0),] # no genus, but has subspecies
no_species_has_subspecies <- df[which(lapply(df$species, name_length) == 0 & lapply(df$infraspecificEpithet, name_length) != 0),] # no species, but has subspecies
incomplete_epithet <- rbind(no_genus_has_species, no_species_has_subspecies, no_genus_has_subspecies)
# pull out higher taxa
single_epithet <- df[which(lapply(df$species, name_length) == 1 & lapply(df$genus, name_length) == 1),] # single-name species AND genus
multi_epithet <- df[which(lapply(df$species, name_length) > 1 | lapply(df$genus, name_length) > 1),] # multi-name species OR genus

multi_subsp <- single_epithet[which(lapply(single_epithet$infraspecificEpithet, name_length) > 1),] # multi-name subspecies
single_epithet <- single_epithet[which(lapply(single_epithet$infraspecificEpithet, name_length) <= 1),] # single subspecific name OR no subspecies

# strip spaces from ends of strings
single_epithet$genus <- lapply(single_epithet$genus, trimws)
single_epithet$species <- lapply(single_epithet$species, trimws)
single_epithet$infraspecificEpithet <- lapply(single_epithet$infraspecificEpithet, trimws)

# remove sp's
sp_wildcards <- c('sp', 'sp.', 'spp', 'spp.', 'sp.nov.', 'sp nov', 'sp. nov.', 
                  'prob', 'prob.', 'probably', 'unid', 'unidentified',
                  'spnov1')
variable_sp1 <- paste('sp', as.character(c(0:9)), sep='')
variable_sp2 <- paste('sp.', as.character(c(0:9)), sep='')
variable_sp3 <- paste('sp. ', as.character(c(0:9)), sep='')
sp_wildcards <- c(sp_wildcards, variable_sp1, variable_sp2, variable_sp3)
removed_sp <- single_epithet[which(single_epithet$species %in% sp_wildcards), ]
single_epithet <- single_epithet[which(single_epithet$species %!in% sp_wildcards), ]

# test for names containing punctuation
punctuated_species <- single_epithet[which(lapply(single_epithet$genus, containsPunc) == TRUE |
                                             lapply(single_epithet$species, containsPunc) == TRUE |
                                             lapply(single_epithet$infraspecificEpithet, containsPunc) == TRUE),]
single_epithet <- single_epithet[which(lapply(single_epithet$genus, containsPunc) == FALSE &
                                         lapply(single_epithet$species, containsPunc) == FALSE &
                                         lapply(single_epithet$infraspecificEpithet, containsPunc) == FALSE),]

# remove very short names for manual verification
short_names_CHECK <- single_epithet[which(lapply(single_epithet$species, nchar) < 4 |
                         lapply(single_epithet$genus, nchar) < 4),] # very short specific_epithet OR genus
single_epithet <- single_epithet[which(lapply(single_epithet$species, nchar) >= 4 &
                                         lapply(single_epithet$genus, nchar) >= 4),] 

# insert some code to check that all "incomplete_epithet" higher taxonomy is present in "single_epithet"
# if not add that genus back into single_epithet with 'sp' for the epithet
# !(unique(incomplete_epithet$genus) %in% unique(single_epithet$genus))
incomplete_epithet_genera <- unique(incomplete_epithet$genus)
incomplete_epithet_genera <- incomplete_epithet_genera[-which(incomplete_epithet_genera == '')] # remove any empty strings as genera
single_epithet_genera <- array(as.character(unlist(unique(single_epithet$genus)))) # why doesn't unique(single_epithet$genus) work?
missing_genera <- incomplete_epithet_genera[!(incomplete_epithet_genera %in% single_epithet_genera)]

if(length(missing_genera) != 0){
  missing_genera <- incomplete_epithet[incomplete_epithet$genus %in% missing_genera,]
  missing_genera$species <- rep('sp', nrow(missing_genera)) # set species to 'sp'
  missing_genera$infraspecificEpithet <- rep('sp', nrow(missing_genera)) # set subspecies to 'sp'
  # single_epithet <- rbind(single_epithet, missing_genera) # not sure if we want to do this without checking first
  # if the above line is approved, we may need to adjust the 'verification passed' check below
}

# combine all rows requiring expert review
if(all(unique(removed_sp$genus) %in% unique(single_epithet$genus))){
  for_expert_review = rbind(incomplete_epithet, short_names_CHECK)
  nrow_expert_review = nrow(for_expert_review) + nrow(removed_sp)
} else {
  for_expert_review = rbind(incomplete_epithet, short_names_CHECK, removed_sp)
  nrow_expert_review = nrow(for_expert_review)
}
rm(incomplete_epithet, short_names_CHECK, removed_sp)

# for additional parsing
unparsed = rbind(multi_epithet, multi_subsp, punctuated_species)
rm(multi_epithet, multi_subsp, punctuated_species)

# successfully parsed
parsed = single_epithet
rm(single_epithet)

# verify no records were lost
verification_passed = FALSE
if(starting_records != nrow(parsed) + 
   nrow(unparsed) + 
   nrow_expert_review) {
} else {
  verification_passed = TRUE
}

if(verification_passed) {
  # generate canonical name
  parsed <- cast_canonical(parsed,
                                   canonical="canonical", 
                                   genus = "genus", 
                                   species = "species",
                                   subspecies = "infraspecificEpithet")
  
  # check Levenshtein's Distance (e.g., misspellings) [may need to do before canonical name generation]
  # Watch for: Ornithodoros vunkeri; Ornithodoros yukeri; Ornithodoros yunkeri
  library(stringdist)
  temp <- c()
  similar_names <-c()
  compared_names <- c()
  cutoff_distance <- 2
  df2 <- c()
  io <- FALSE
  for(i in 1:length(parsed$canonical)){
    if(!(parsed$canonical[i] %in% similar_names)){ # testing
      for(j in 1:length(parsed$canonical)){
        score <- stringdist(parsed$canonical[i], parsed$canonical[j], "dl")
        temp <- c(temp, score)
      }
      if(any(temp %in% c(1:cutoff_distance))){
        if(io){
          df2 <- cbind(df2, temp)
          wc = wc + 1
        } else {
          df2 <- as.data.frame(temp)
          rownames(df2) <- parsed$canonical
          io <- TRUE
          wc <- 1
        }
        colnames(df2)[which(colnames(df2) == "temp")] <- parsed$canonical[i]
        similar <- rownames(df2)[which(df2[,wc]==min(df2[,wc][which(df2[,wc]>0)]))]
        comp_name <- rep(parsed$canonical[i], length(similar))
        similar_names <- c(similar_names, similar)
        compared_names <- c(compared_names, comp_name)
      }
      temp <- c()
    }
    if(i %% 10 == 0){
      print(paste('Completed iteration:', i, 'out of', length(parsed$canonical), 'iterations (', round(i/length(parsed$canonical),2)*100,'% DONE)'))
    }
  }
  print('FINISHED!')
  check_mat <- as.data.frame(cbind(compared_names, similar_names))

  # check for duplicate names 
  duplicates <- parsed[which(duplicated(parsed$canonical)),]
  parsed <- parsed[which(!duplicated(parsed$canonical)),] # deduplicated list
  
  # synonymize subspecies example: Amblyomma triguttatum triguttatum = Amblyomma triguttatum
  parsed <- synonymize_subspecies(parsed)
  # parsed$genus <- array(as.character(unlist((parsed$genus)))) # sometimes needed to sort by variable in RStudio
  # parsed$species <- array(as.character(unlist((parsed$species)))) # sometimes needed to sort by variable in RStudio
  
  # number unique
  nominate_species <- parsed[parsed$accid == 0, ]
  trinomial_nominate_names <- nominate_species[which(nominate_species$infraspecificepithet != ''),]
  binomial_nominate_names <- nominate_species[which(nominate_species$infraspecificepithet == ''),]
  if(nrow(trinomial_nominate_names) + 
     nrow(binomial_nominate_names) == nrow(nominate_species)){
    rm(nominate_species)
  } else {
    print("Please check your nominate_species data frame for issues.")
  }
  subspecies <- parsed[parsed$accid != 0, ]
  
  # handle incomplete_epithet
  # handle multi-word names
  
  # handle authors, years
  
  
} else {
  print('Verification was not passed. Some records appear to have been lost. Script was terminated. Please address errors.')
}
