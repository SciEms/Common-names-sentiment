### Exploring IUCN Red List common names ###

## Script 1 - Load and clean ##

# import data

cn <- read_xlsx("data/animalia_export.xlsx")

# tidy data

cn2 <- setNames(cn, c("species_ID","kingdom","phylum", "class", "order", "family", "genus", "species", "authority",
                      "infraspecific_rank", "infraspecific_name", "infraspecific_authority", "stock/subpopulation",
                      "synonyms", "common_name_eng", "common_name_fre", "common_name_spa", "red_list_status",
                      "red_list_criteria", "red_list_criteria_vers", "year_assessed", "population_trend",
                      "petitioned"))
cn2$phylum <- as.factor(cn2$phylum)
cn2$class <- as.factor(cn2$class)
cn2$order <- as.factor(cn2$order)
cn2$family <- as.factor(cn2$family)
cn2$red_list_status <- as.factor(cn2$red_list_status)

length(unique(cn2$species_ID))

# extract species with english common names
cn3 <- subset(cn2, !is.na(cn2$common_name_eng)) # 38696 obs. wiht eng common names out of 64845
#turn all lower case
cn3$common_name_eng <- tolower(cn3$common_name_eng)

#renaming and sorting order
# in 2001 LOW RISK NT and LC made into own categories and CD merged into NT
# renamed old categories as such
cn3$red_list_status <- as.character(cn3$red_list_status) # convert to character to use ifelse()
cn3$red_list_status <- ifelse(cn3$red_list_status == "LR/cd", "NT",
                              ifelse(cn3$red_list_status == "LR/lc", "LC",
                                     ifelse(cn3$red_list_status == "LR/nt", "NT", cn3$red_list_status)))
cn3$red_list_status <- as.factor(cn3$red_list_status) # convert back to factor
cn3$red_list_status <- factor(cn3$red_list_status, levels=c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD"))

write.csv(cn3, "data/names_clean.csv")
