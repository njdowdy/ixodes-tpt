# Read in my files
library(readxl)
BYU_Siphonaptera_Synonyms <- read_excel("~/GitHub/ixodes-tpt/input/BYU Siphonaptera Synonyms.xlsx", 
                                          +     sheet = "Siphonaptera BYU Syn DwC full")
BYU_Siphonaptera <- read_excel("~/GitHub/ixodes-tpt/input/BYU Siphonaptera.xlsx", 
                               +     sheet = "Siphonaptera BYU DwC full")
CoL_Siphonaptera <- read_excel("~/GitHub/ixodes-tpt/input/CoL Siphonaptera.xlsx", 
                               +     sheet = "Siphonaptera CoL DwC full")
FMNH_Siphonaptera <- read_excel("~/GitHub/ixodes-tpt/input/FMNH Siphonaptera.xlsx", 
                                +     sheet = "Siphpnaptera FMNH DwC full")
GBIF_Siphonaptera <- read_excel("~/GitHub/ixodes-tpt/input/GBIF Siphonaptera.xlsx", 
                                +     sheet = "Siphonaptera GBIF DwC full")
ITIS_Siphonaptera <- read_excel("~/GitHub/ixodes-tpt/input/ITIS Siphonaptera.xlsx", 
                                +     sheet = "Siphonaptera ITIS DwC full")
NMNH_Siphonaptera <- read_excel("~/GitHub/ixodes-tpt/input/NMNH Siphonaptera.xlsx", 
                                +     sheet = "Siphonaptera NMNH DwC full")

# create source summary dataframe
BYUSYN <- nrow(BYU_Siphonaptera_Synonyms) # number of original records in BYU Synonym Source
BYU <- nrow(BYU_Siphonaptera) # number of original records in BYU Source
COL <- nrow(CoL_Siphonaptera) # number of original records in CoL Source
FMNH <- nrow(FMNH_Siphonaptera) # number of original records in FMNH Source
GBIF <- nrow(GBIF_Siphonaptera) # number of original records in GBIF Source
ITIS <- nrow(ITIS_Siphonaptera) # number of original records in ITIS Source
NMNH <- nrow(NMNH_Siphonaptera) # number of original records in NMNH Source
TOTAL <- nrow(BYU_Siphonaptera_Synonyms) + 
  nrow(BYU_Siphonaptera) + 
  nrow(CoL_Siphonaptera) + 
  nrow(FMNH_Siphonaptera) + 
  nrow(GBIF_Siphonaptera) + 
  nrow(ITIS_Siphonaptera) + 
  nrow(NMNH_Siphonaptera) # number of original records in all sources
source_code <- c("BYU", "BYUSYN", "CoL", "FMNH", "GBIF", "ITIS", "NMNH", "Total")
original_name_count <- c(BYU, BYUSYN, COL, FMNH, GBIF, ITIS, NMNH, TOTAL)
source_summary <- data.frame(source_code, original_name_count)

# combine all sources for cleaning
df <- rbind(BYU_Siphonaptera,BYU_Siphonaptera_Synonyms, CoL_Siphonaptera, FMNH_Siphonaptera, GBIF_Siphonaptera, ITIS_Siphonaptera, NMNH_Siphonaptera)

# verify number of names
# verify no records were lost
verification_passed = FALSE # set variable to FALSE
if(TOTAL != nrow(df)) { # test that TOTAL is equal to number of names in combined data frame
} else {
  verification_passed = TRUE # if true, change variable to TRUE
}

if(verification_passed) { # if variable is TRUE
  print('name count verified') # print message that count was verified
  } else { # if variable is FALSE
  print('Verification was not passed. Some records appear to have been lost. Script was terminated. Please address errors.') # print message that count was not verified
  }

# proceed to tpt_text_cleaning script