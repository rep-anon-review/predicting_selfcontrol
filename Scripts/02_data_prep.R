

# Load packages -----------------------------------------------------------

library(here)
library(psych)
library(stringr)
library(tidyverse)
library(corrplot)
library(readr)


# Load data ---------------------------------------------------------------

codebook <- readxl::read_excel(here("Data", "codebook.xlsx"))

# Get the variables that are needed for this project
names_trait_items <- codebook %>% filter(measured_at == "BL") %>% pull(item_name) %>% na.omit()
names_ESM_items <- codebook %>% filter(grepl("ESM", measured_at)) %>% pull(item_name) %>% na.omit()
demo_vars <- c("Participant", "DemSex", "DemAge", "DemStudent3")

# full dataset containing also variables that are not considered in this study
# full_dat <- read_delim(here("Data", "SNF_2020_combined.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE)

# raw_dat <- full_dat %>% select(all_of(c(demo_vars, names_trait_items, names_ESM_items)))
# write_csv(raw_dat, here("Data", "raw_dat.csv"))

# load the raw data
dat <- read_csv(here("Data", "raw_dat.csv"))

# there are two versions for ESM items, A and B, which have to be merged
# Note that both forms differ only with regard to variables that were *not* included in the present study 
no_version_names <- str_replace(names_ESM_items, "^[AB]_", "") %>% unique() # remove the version number & duplicates


# merge both questionnaire versions 
for(var_name in no_version_names){
  var_A <- paste0("A_", var_name)
  var_B <- paste0("B_", var_name)
  dat[[var_name]] <- ifelse(is.na(dat[[var_A]]), dat[[var_B]], dat[[var_A]])
}


ESM_dat <- dat[no_version_names] # save the combined variables 
ESM_dat$Participant <- dat$Participant # add ID variable 

# Create person-indices based on ESM data ---------------------------------

ESM_dat <- ESM_dat %>% 
            mutate(success_init = rowMeans(dplyr::select(., eINIT_SUCCESSest, eINIT_SUCCESSsatis), na.rm = TRUE),
                   success_pers = rowMeans(dplyr::select(., eCONT_SUCCESSest, eCONT_SUCCESSsatis), na.rm = TRUE),
                   success_inhi = rowMeans(dplyr::select(., eHIBIT_SUCCESSest, eHIBIT_SUCCESSsatis), na.rm =TRUE)) %>%
            mutate(across(c(success_init, success_pers, success_inhi), ~replace(., is.nan(.), NA))) %>%
            mutate(success_all = rowMeans(dplyr::select(., success_init, success_pers, success_inhi), na.rm = TRUE),
                   intensity_all = rowMeans(dplyr::select(., eINIT_CONFLstrength, eCONT_CONFLstrength, eHIBIT_CONFLstrength), na.rm = TRUE)) %>% 
           group_by(Participant) %>% 
           mutate(n_per_person = sum(!is.na(Con_Exp)), # number of non-missings per person 
                  n_any_con = sum(Con_Exp == 1 | Con_Exp == 2 | Con_Exp == 3, na.rm = TRUE), # total number of conflicts across all conflict types total number of 
                  n_con_init = sum(Con_Exp == 1, na.rm =TRUE), # total number of initiation conflicts per person 
                  n_con_pers = sum(Con_Exp == 2, na.rm = TRUE), # total number of persistence conflicts per person 
                  n_con_inhi = sum (Con_Exp == 3, na.rm = TRUE), # total number of inhibition conflicts per person 
                  freq_con_all = n_any_con/n_per_person, # relative frequency of conflicts across all conflict types per person, 
                  freq_con_init = ifelse(n_con_init != 0, n_con_init/n_per_person, NA), # relative frequency of inhibition conflicts per person 
                  freq_con_pers = ifelse(n_con_pers != 0, n_con_pers/n_per_person, NA), # relative frequency of persistence conflicts per person 
                  freq_con_inhi = ifelse(n_con_inhi != 0, n_con_inhi/n_per_person,NA), # relative frequency of inhibition conflicts per person 
                  m_intensity_all = mean(intensity_all, na.rm = TRUE),
                  m_intensity_init = mean(eINIT_CONFLstrength, na.rm = TRUE),
                  m_intensity_pers = mean(eCONT_CONFLstrength, na.rm =TRUE),
                  m_intensity_inhi = mean(eHIBIT_CONFLstrength, na.rm = TRUE),
                  m_success_all = mean(success_all, na.rm = TRUE), 
                  m_success_init = mean(success_init, na.rm =TRUE), 
                  m_success_pers = mean(success_pers, na.rm =TRUE),
                  m_success_inhi = mean(success_inhi, na.rm = TRUE)) %>% 
                  ungroup()


# Information on the ESM data 
ESM_dat_all <- ESM_dat %>% filter(!is.na(Con_Exp)) # all valid observations
nrow(ESM_dat_all) # 26174 observations
length(unique(ESM_dat_all$Participant)) # 503 Participants 
ESM_dat_con <- ESM_dat %>% filter(Con_Exp %in% c(1,2,3)) # all observations in which a conflict was reported 
nrow(ESM_dat_con) # 9639 reported conflicts 
length(unique(ESM_dat_con$Participant)) # 492 Participants = exclusion of 9 participants who did not report at least one conflict 




# Descriptive statistics of the ESM variables -----------------------------
# only keep between-person variables from the ESM data 
BP_ESM_dat <- ESM_dat_con %>% dplyr::select(Participant, n_per_person:m_success_inhi) %>% distinct(Participant, .keep_all = TRUE)
BP_ESM_dat <- BP_ESM_dat %>% mutate(across(-Participant, ~ as.numeric(.)))

# calculate means and standard deviations of the ESM variables 
ESM_vars <- BP_ESM_dat %>% select(-Participant) %>% names()

ESM_des <- BP_ESM_dat %>% summarise(across(all_of(ESM_vars), 
                   list(mean = ~ mean(., na.rm = TRUE), 
                        sd = ~ sd(., na.rm = TRUE)), 
                   .names = "{.fn}_{.col}"))
ESM_des_prep <- ESM_des %>% pivot_longer(cols = everything(), 
                                         names_to = c("statistic", "variable"), 
                                         names_pattern = "([^_]+)_(.+)") %>%
                            pivot_wider(names_from = statistic,
                                        values_from = value,
                                        names_sep = "_")

if (!dir.exists("Tables")) dir.create("Tables", recursive = TRUE)
# save descriptive statistics
writexl::write_xlsx(ESM_des_prep, here("Tables", "ESM_variables_descriptives.xlsx"))


# plot the distributions
if (!dir.exists("Plots")) dir.create("Plots", recursive = TRUE)
png(filename = here("Plots", "state_vars_distributions.png"), width = 1200, height = 1000)

par(mfrow = c(5, 4),         
    oma = c(0, 0, 6, 0),     
    mar = c(4, 4, 2, 1),      
    cex.main = 2,           
    cex.lab = 1.3,            
    cex.axis = 1.2)    

for(v in ESM_vars){
  plot <- hist(BP_ESM_dat[,v, drop = TRUE],  freq = TRUE, cex.axis = 1, breaks= 30,  cex.lab = 1, xlab= "", ylab="", main = v)
}

mtext("Distributions of state vars", outer = TRUE, cex = 1.5)  
dev.off()

# Trait items  ------------------------------------------------------------
trait_dat <- dat %>% select(all_of(c(demo_vars, names_trait_items))) %>% 
             filter(rowSums(is.na(.)) <= 100) # only keep the rows where the responses to the baseline measures are stored 

# check for missings in trait items 
trait_dat %>% 
  select(all_of(names_trait_items)) %>% 
  filter(rowSums(is.na(.)) > 0) %>% 
  nrow() # if we do not impute the missings, we would loose 91 participants 

# recode trait items 
## most items were measured on a 7 point scale except the bfi and MISCS items 
five_point_scale_items <- names_trait_items[grepl("bfi|MISCS", names_trait_items)]
seven_point_scale_items <- names_trait_items[!names_trait_items %in% five_point_scale_items]

to_recode_five_point_scale <- codebook %>% 
                              filter(item_name %in% five_point_scale_items) %>% 
                              filter(recoded_items == "R") %>% 
                              dplyr::select(item_name) %>% pull()

to_recode_seven_point_scale <- codebook %>% 
                              filter(item_name %in% seven_point_scale_items) %>% 
                              filter(recoded_items == "R") %>% 
                              dplyr::select(item_name) %>% pull()

trait_dat[to_recode_five_point_scale] <- 6 - trait_dat[to_recode_five_point_scale] 
trait_dat[to_recode_seven_point_scale] <- 8 - trait_dat[to_recode_seven_point_scale] 

# calculate means and standard deviations of the trait variables 
trait_des <- trait_dat %>% 
           summarise(across(all_of(names_trait_items), 
                           list(mean = ~ mean(., na.rm = TRUE), 
                                sd = ~ sd(., na.rm = TRUE)), 
                           .names = "{.fn}_{.col}"))

trait_des_prep <- trait_des %>% pivot_longer(
                    cols = everything(),
                    names_to = c("statistic", "variable"),
                    names_pattern = "([^_]+)_(.+)"
                  ) %>%
                    pivot_wider(names_from = statistic,
                                values_from = value,
                                names_sep = "_")

# save descriptive statistics
writexl::write_xlsx(trait_des_prep, here("Tables", "all_trait_items_descriptives.xlsx"))


# Combine data ------------------------------------------------------------

#add 'ESM' ending to ESM variables to facilitate later steps 
names(BP_ESM_dat) <- ifelse(names(BP_ESM_dat) == "Participant", 
                            "Participant", 
                            paste0(names(BP_ESM_dat), "_ESM"))

# combine both data frames 
prep_dat <- inner_join(BP_ESM_dat, trait_dat, by ="Participant")

# save data 
write_csv(prep_dat, here("Data", "prep_data.csv"))


# Information on the final sample -----------------------------------------------

# Final number of participants
n_participants <- nrow(prep_dat)

# One person only filled out the ESM part but not the trait meausres
id_no_trait_data <- setdiff(BP_ESM_dat$Participant, trait_dat$Participant) # ID 218 did not provide trait data 

# Final Number of included conflicts with the participant without trait data removed 
n_observations <- ESM_dat_con %>% filter(!Participant == id_no_trait_data) %>% nrow() 

# ESM observations (i.e., reported conflicts) per person
n_per_person <- n_observations/n_participants

# Percentage of females
females <- prep_dat %>% filter(DemSex == 2) %>% nrow()
females/n_participants

# Percentage students
students <- prep_dat %>% filter(DemStudent3 =="Y") %>% nrow()
students/n_participants

# Age information 
table(is.na(prep_dat$DemAge)) # 12 participants have missing data on the age variable
describe(prep_dat$DemAge)

# missings per person among the trait items 
missings_pers<- prep_dat %>% select(all_of(names_trait_items)) %>% 
                reframe(missing_per_person = rowSums(is.na(.)))
describe(missings_pers$missing_per_person) # only very little missing data per person

# missings per item among the trait items 
missings_vars <- prep_dat %>% select(all_of(names_trait_items)) %>% 
  summarise(across(everything(), ~ sum(is.na(.))))
long_missings_vars <- missings_vars %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count")
describe(long_missings_vars$missing_count) # also only very little missing data per item 











