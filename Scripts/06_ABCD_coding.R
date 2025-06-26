# check which packages are needed
library(here)
library(psych)
library(stringr)
library(tidyverse)
library(readxl)
library(writexl)

# load data
data <- read_csv(here("Data", "prep_data.csv"))
codebook <- readxl::read_excel(here("Data", "codebook.xlsx"))
source(here("Scripts", "01_functions.r"))



# Coding Testset 1 --------------------------------------------------------

rater1 <- read_excel(here("ABCD Coding", "Pre-Coding-No1", "Pre-coding-13-11-2023_R1.xlsx"))
rater2 <- read_excel(here("ABCD Coding", "Pre-Coding-No1", "Pre-coding-13-11-2023_R2.xlsx"))
rater3 <- read_excel(here("ABCD Coding", "Pre-Coding-No1", "Pre-coding-13-11-2023_R3.xlsx"))
rater4 <- read_excel(here("ABCD Coding", "Pre-Coding-No1", "Pre-coding-13-11-2023_R4.xlsx"))
rater5 <- read_excel(here("ABCD Coding", "Pre-Coding-No1", "Pre-coding-13-11-2023_R5.xlsx"))
rater6 <- read_excel(here("ABCD Coding", "Pre-Coding-No1", "Pre-coding-13-11-2023_R6.xlsx"))

a_codings_t1 <- data.frame(cbind(rater1$`Affect %`, rater2$`Affect %`, rater3$`Affect %`, rater4$`Affect %`, rater5$`Affect %`, rater6$`Affect %`))
b_codings_t1 <- data.frame(cbind(rater1$`Behavior %`, rater2$`Behavior %`, rater3$`Behavior %`, rater4$`Behavior %`, rater5$`Behavior %`, rater6$`Behavior %`))
c_codings_t1 <- data.frame(cbind(rater1$`Cognition %`, rater2$`Cognition %`, rater3$`Cognition %`, rater4$`Cognition %`, rater5$`Cognition %`, rater6$`Cognition %`))
d_codings_t1 <- data.frame(cbind(rater1$`Desire %`, rater2$`Desire %`, rater3$`Desire %`, rater4$`Desire %`,rater5$`Desire %`, rater6$`Desire %`))


# Compute ICCs
icc_a_codings_t1 <- ICC(a_codings_t1)
icc_b_codings_t1 <- ICC(b_codings_t1)
icc_c_codings_t1 <- ICC(c_codings_t1)
icc_d_codings_t1 <- ICC(d_codings_t1)

#Single rater 
round(icc_a_codings_t1$results["Single_raters_absolute", "ICC"],2) #.63
round(icc_b_codings_t1$results["Single_raters_absolute", "ICC"],2) #.69
round(icc_c_codings_t1$results["Single_raters_absolute", "ICC"],2) #.54
round(icc_d_codings_t1$results["Single_raters_absolute", "ICC"],2) #.42


# ICC average of six random raters
round(icc_a_codings_t1$results["Average_raters_absolute", "ICC"],2) #.91
round(icc_b_codings_t1$results["Average_raters_absolute", "ICC"],2) #.93
round(icc_c_codings_t1$results["Average_raters_absolute", "ICC"],2) #.87
round(icc_d_codings_t1$results["Average_raters_absolute", "ICC"],2) #.81





# Prepare trait items form main ABCD coding -------------------------------------
# set.seed(1)
# items_to_code <- codebook %>% filter(measured_at == "BL") %>% select(item_name,item_text_german, item_text_english) %>% na.omit()
# random_order <- items_to_code[sample(nrow(items_to_code)),] # randomly change the order 
# writexl::write_xlsx(random_order, "coding_items_random_order.xlsx") # save a version with item names 
# coding_items <- random_order %>% select(item_text_german)
# writexl::write_xlsx(coding_items, "coding_items") # save only the items 



# Main Coding -------------------------------------------------------------

rater_1 <- read_excel(here("ABCD coding", "Main coding", "ABCD-coding-TSC-project_R1.xlsx"))
rater_2 <- read_excel(here("ABCD coding", "Main coding", "ABCD-coding-TSC-project_R2.xlsx"))
rater_3 <- read_excel(here("ABCD coding", "Main coding", "ABCD-coding-TSC-project_R3.xlsx"))
rater_4 <- read_excel(here("ABCD coding", "Main coding", "ABCD-coding-TSC-project_R4.xlsx"))
rater_5 <- read_excel(here("ABCD coding", "Main coding", "ABCD-coding-TSC-project_R5.xlsx"))
rater_6 <- read_excel(here("ABCD coding", "Main coding", "ABCD-coding-TSC-project_R5.xlsx"))

#load names of the items 
item_names <- readxl::read_excel(here("ABCD coding", "Main coding", "coding_items_random_order.xlsx"))

a_codings <- cbind(rater_1$`Affect %`, rater_2$`Affect %`, rater_3$`Affect %`, rater_4$`Affect %`, rater_5$`Affect %`, rater_6$`Affect %`)
b_codings <- cbind(rater_1$`Behavior %`, rater_2$`Behavior %`, rater_3$`Behavior %`, rater_4$`Behavior %`, rater_5$`Behavior %`, rater_6$`Behavior %`)
c_codings <- cbind(rater_1$`Cognition %`, rater_2$`Cognition %`,rater_3$`Cognition %`, rater_4$`Cognition %`, rater_5$`Cognition %`, rater_6$`Cognition %`)
d_codings <- cbind(rater_1$`Desire %`, rater_2$`Desire %`, rater_3$`Desire %`, rater_4$`Desire %`, rater_5$`Desire %`, rater_6$`Desire %`)

# compute ICCs
icc_a_codings <- ICC(a_codings)
icc_b_codings <- ICC(b_codings)
icc_c_codings <- ICC(c_codings)
icc_d_codings <- ICC(d_codings)

#Single rater 
round(icc_a_codings$results["Single_raters_absolute", "ICC"],2) #.54
round(icc_b_codings$results["Single_raters_absolute", "ICC"],2) #.45
round(icc_c_codings$results["Single_raters_absolute", "ICC"],2) #.51
round(icc_d_codings$results["Single_raters_absolute", "ICC"],2) #.25


# ICC average of six random raters
round(icc_a_codings$results["Average_raters_absolute", "ICC"],2) #.87
round(icc_b_codings$results["Average_raters_absolute", "ICC"],2) #.83
round(icc_c_codings$results["Average_raters_absolute", "ICC"],2) #.86
round(icc_d_codings$results["Average_raters_absolute", "ICC"],2) #.67




# Average ABCD rating for each item  --------------------------------------

ABCD_coding <- item_names %>% dplyr::select(item_names_spss)
names(ABCD_coding) <- "item_name"
ABCD_coding$affect <- rowMeans(a_codings)
ABCD_coding$behavior <- rowMeans(b_codings)
ABCD_coding$cognition <- rowMeans(c_codings)
ABCD_coding$desire <- rowMeans(d_codings)
# save the results 
write_xlsx(ABCD_coding,here("ABCD Coding", "ABCD results", "ABCD_codig_all_items.xlsx"))


# ABCD Ratings for the trait predictors in the different models -----------

## Frequency across conflicts ----------------------------------------------
final_items_freq_all <- read_csv(here("ML results", "final_var_frequency_all.csv")) %>% pull()
ABCD_freq_all <-  ABCD_coding %>% filter(item_name %in% final_items_freq_all)

composites <- list(
  dutifulness_compliance_5 = c("ipc2_5", "ipc3_5", "ipc3_6", "scs04"),
  impulse_inhibition_2 = c("hoyle3", "hoyle6")
)

# Define single items for renaming
single_items <- c(
  bfi1 = "talkativeness_1",
  bisf14 = "inattention_1",
  ipc2_2 = "tidiness_1", 
  MISCS25 = "metacognitive_regulation_1",
  ERQ5 = "reappraisal_1", 
  MISCS4 = "metacognitive_knowledge_1",
  hoyle12 = "behavior_initiation_1"
)

# compute averages for the trait predictors
ABCD_freq_all_results <- ABCD_prep(ABCD_freq_all, composites, single_items) %>% rename(trait_predictor = item_name)

# save results 
write_xlsx(ABCD_freq_all_results,here("ABCD Coding", "ABCD results", "ABCD_freq_all_results.xlsx"))


## Intensity across conflicts ----------------------------------------------
final_items_intensity_all <- read_csv(here("ML results", "final_var_intensity_all.csv")) %>% pull()
ABCD_intensity_all <-  ABCD_coding %>% filter(item_name %in% final_items_intensity_all)

composites <- list(behavior_initiation_6 = c("hoyle12", "hoyle4", "ipc5_6", "ipc5_7", "ips1", "scs06"),
                   worry_5 = c("bfi10", "bisbas13", "bisbas19", "bisbas24", "bisbas8"),
                   temptation_resistance_2 = c("hoyle3", "scs01"),
                   metacognitive_regulation_2 = c("MISCS23", "MISCS25"))


single_items <- c(bisbas3 = "drive_1",
                  ipc2_4 = "perfectionism_1",
                  ipc1_5 = "selfconfidence_1", 
                  MISCS21 = "metacognitive_knowledge_1", 
                  bfi1 = "talkativeness_1")

ABCD_intensity_all_results <- ABCD_prep(ABCD_intensity_all, composites, single_items) %>% rename(trait_predictor = item_name)
write_xlsx(ABCD_intensity_all_results,here("ABCD Coding", "ABCD results", "ABCD_intensity_all_results.xlsx"))

## Success across conflicts ------------------------------------------------
final_items_success_all <- read_csv(here("ML results", "final_var_success_all.csv")) %>% pull()
ABCD_success_all <-  ABCD_coding %>% filter(item_name %in% final_items_success_all)


# compute composite scores for the final ML analysis 
composites <- list(capacity_selfcontrol_8 = c("ipc4_3", "ipc4_6", "ipc5_2", "ips5", "scs09", "stst10",
                                              "stst14","stst16"), 
                   metacognition_5 = c("MISCS11", "MISCS12", "MISCS15", "MISCS21", "MISCS4"),
                   impulsive_behavior_2 = c("bisf3", "bisf5"))

# Define single items for renaming
single_items <- c(ipc6_2 = "error_avoidance_1", 
                  ipc3_8 = "honesty_1")

ABCD_success_all_results <- ABCD_prep(ABCD_success_all, composites, single_items) %>% rename(trait_predictor = item_name)
write_xlsx(ABCD_success_all_results,here("ABCD Coding", "ABCD results", "ABCD_success_all_results.xlsx"))


## Frequency initiation conflicts ------------------------------------------
final_items_freq_init <- read_csv(here("ML results", "final_var_frequency_init.csv")) %>% pull()
ABCD_freq_init <-  ABCD_coding %>% filter(item_name %in% final_items_freq_init)


# compute composite scores for the final ML analysis 
composites <- list(diligence_compliance_4 = c("ipc2_5", "ipc3_1", "ipc6_6", "scs04"),
                   capacity_selfcontrol_4 = c("hoyle12", "hoyle3", "stst10", "stst16"))


# Define single items for renaming
single_items <- c(
  bisf14 = "inattention_1",
  MISCS4 = "metacognitive_knowledge_1"
)

ABCD_freq_init_results <- ABCD_prep(ABCD_freq_init, composites, single_items) %>% rename(trait_predictor = item_name)
write_xlsx(ABCD_freq_init_results,here("ABCD Coding", "ABCD results", "ABCD_freq_init_results.xlsx"))


## Frequency persistence conflicts -----------------------------------------
final_items_freq_pers <- read_csv(here("ML results", "final_var_frequency_pers.csv")) %>% pull()
ABCD_freq_pers <-  ABCD_coding %>% filter(item_name %in% final_items_freq_pers)


# compute composite scores for the final ML analysis 
composites <- list(regulation_5 = c("ERQ5", "ERQ3", "MISCS12", "MISCS19", "MISCS23"), 
                   diligence_compliance_2 = c("ipc2_5", "ipc3_6")) 


single_items <- c(bfi1 = "talkativeness_1",
                  ipc2_2 = "tidiness_1",
                  ips2 = "postpone_tasks_1",
                  MISCS4 = "metacognitive_knowledge_1")

ABCD_freq_pers_results <- ABCD_prep(ABCD_freq_pers, composites, single_items) %>% rename(trait_predictor = item_name)
write_xlsx(ABCD_freq_pers_results,here("ABCD Coding", "ABCD results", "ABCD_freq_pers_results.xlsx"))


## Intensity initiation conflicts ------------------------------------------
final_items_intensity_init <- read_csv(here("ML results", "final_var_intensity_init.csv")) %>% pull()
ABCD_intensity_init <-  ABCD_coding %>% filter(item_name %in% final_items_intensity_init)

# Just rename the items 
ABCD_intensity_init[1, "item_name"] <- "worry_1"
ABCD_intensity_init[2, "item_name"] <- "ipc5_7"

ABCD_intensity_init_results <- ABCD_intensity_init %>% rename(trait_predictor = item_name)

write_xlsx(ABCD_intensity_init_results,here("ABCD Coding", "ABCD results", "ABCD_intensity_init_results.xlsx"))


## Intensity persistence conflicts -----------------------------------------
final_items_intensity_pers <- read_csv(here("ML results", "final_var_intensity_pers.csv")) %>% pull()
ABCD_intensity_pers <-  ABCD_coding %>% filter(item_name %in% final_items_intensity_pers)

composites <- list(worry_5 = c("bisbas13", "bisbas19", "bisbas22", "bisbas24", "bisbas8"), 
                   behavior_initiation_3 = c("hoyle4", "ipc5_6", "ips9"),
                   temptation_resistance_2 = c("hoyle1", "hoyle3"), 
                   social_impulsivity_3 = c("bfi1", "scs04", "stst4"))

single_items <- c(bisbas9 = "drive_1",
                  ipc1_5 = "selfconfidence_1",
                  ipc2_4 = "perfectionism_1", 
                  ips2 = "postpone_tasks_1", 
                  MISCS21 = "metacognitive_knowledge_1")

ABCD_intensity_pers_results <- ABCD_prep(ABCD_intensity_pers, composites, single_items) %>% rename(trait_predictor = item_name)
write_xlsx(ABCD_intensity_pers_results,here("ABCD Coding", "ABCD results", "ABCD_intensity_pers_results.xlsx"))


## Success initiation conflicts --------------------------------------------
final_items_success_init <- read_csv(here("ML results", "final_var_success_init.csv")) %>% pull()
ABCD_success_init <-  ABCD_coding %>% filter(item_name %in% final_items_success_init)


# compute composite scores for the final ML analysis 
composites <- list(behavior_initiation_6 = c("hoyle13", "ipc5_2", "ipc5_5", "ipc5_7", "ips5", "ips7"),
                   attentional_impulsivity_2 = c("bisf13", "scs08"),
                   impulsive_behavior_3 = c("bisf3", "bisf5", "stst1"),
                   preserverance_2 = c("stst10", "stst14"),
                   metacognitive_knowledge_4 = c("MISCS11", "MISCS12", "MISCS18", "MISCS4"))



# Define single items for renaming
single_items <- c(bfi1 = "talkativeness_1",
                  bfi11 = "depression_1",
                  bisbas4 = "persistence_after_success_1",
                  ipc4_6 = "plan_implementation_1",
                  scs09 = "working_towards_goals_1")

ABCD_success_init_results <- ABCD_prep(ABCD_success_init, composites, single_items) %>% rename(trait_predictor = item_name)
write_xlsx(ABCD_success_init_results,here("ABCD Coding", "ABCD results", "ABCD_success_init_results.xlsx"))


## Success persistence conflicts -------------------------------------------
final_items_success_pers <- read_csv(here("ML results", "final_var_success_pers.csv")) %>% pull()
ABCD_success_pers <-  ABCD_coding %>% filter(item_name %in% final_items_success_pers)

# compute composite scores for the final ML analysis 
composites <- list(self_discipline_5= c("ipc1_8", "ipc5_2", "ips5", "scs06", "stst12"),
                   metacognition_3 = c("MISCS12", "MISCS15", "MISCS4"))


# Define single items for renaming
single_items <- c(hoyle20 = "ease_persistence_1", 
                  bfi11 = "depression_1", 
                  bisbas7 = "reward_responsiveness_1", 
                  ipc1_4 = "competence_1", 
                  stst16 = "refocusing_after_distraction_1", 
                  stst17 = "persistence_despite_fatigue_1")

ABCD_success_pers_results <- ABCD_prep(ABCD_success_pers, composites, single_items) %>% rename(trait_predictor = item_name)
write_xlsx(ABCD_success_pers_results,here("ABCD Coding", "ABCD results", "ABCD_success_pers_results.xlsx"))

