

# Load packages and data  -------------------------------------------------

library(tidyverse)
library(mlr3verse)
library(paradox)
library(mlr3tuning)
library(glmnet)
library(DALEX)
library(future)
library(performance)
library(paran)
library(gridExtra)
library(here)
library(forcats)
library(lgr)
library(RColorBrewer)
source(here("Scripts", "01_functions.R"))

# load data
data <- read_csv(here("Data", "prep_data.csv"))
codebook <- readxl::read_excel(here("Data", "codebook.xlsx"))


# set colors for plot
colors <- brewer.pal(11, "Spectral")  # Access the Spectral palette with 11 colors
bar_colors <- colors[9:10]  # Select colors 9 and 10 for the bar plot



# Preparatory work for RQ1 & RQ2 ------------------------------------------


## Frequency across conflicts ----------------------------------------------
final_items_freq_all <- read_csv(here("ML results", "final_var_frequency_all.csv")) %>% pull()
freq_all_dat <- data %>% dplyr::select(all_of(final_items_freq_all))

# assign items to composites based on the results of the EFA 
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


# prepare variables
freq_all_dat_final <- prepare_vars(freq_all_dat, single_items, composites)

# add outcome variable 
freq_all_dat_final <- cbind(freq_all_dat_final, data[, "freq_con_all_ESM"]) %>% filter(!is.na(freq_con_all_ESM))


#create dataset that contains the composite scores 
freq_all_composite <- freq_all_dat_final %>% dplyr::select(dutifulness_compliance_5, impulse_inhibition_2,talkativeness_1,
                                                        metacognitive_regulation_1, inattention_1, tidiness_1, metacognitive_knowledge_1,
                                                        reappraisal_1, behavior_initiation_1, freq_con_all_ESM)

# create dataset for the robustness check in which only the item with the highest loading is used 

freq_all_robust <- freq_all_dat_final %>% dplyr::select(ipc3_6, hoyle6,inattention_1, tidiness_1, metacognitive_regulation_1,
                                                     reappraisal_1, talkativeness_1, metacognitive_knowledge_1,
                                                     behavior_initiation_1,  freq_con_all_ESM) %>% 
  rename(dutifulness_compliance = ipc3_6, impulse_inhibition = hoyle6, 
         metacognitive_regulation = metacognitive_regulation_1, inattention = inattention_1, 
         tidiness = tidiness_1, reappraisal = reappraisal_1, metacognitive_knowledge = metacognitive_knowledge_1,
         behavior_initiation = behavior_initiation_1, talkativeness = talkativeness_1)


cors_freq_all_c <- final_cors(freq_all_composite, "freq_con_all_ESM")
betas_freq_all_c <- final_betas(freq_all_composite, "freq_con_all_ESM")

cors_freq_all_r <- final_cors(freq_all_robust, "freq_con_all_ESM")
betas_freq_all_r <- final_betas(freq_all_robust, "freq_con_all_ESM")

# plot the correlations
png(filename = here("Plots", "Final_correlations_frequency_all.png"),  width = 1800, height = 1000)
par(mfrow = c(1,2), 
    cex = 2)
coefficient_plot(cors_freq_all_c, left_margin = 16)
coefficient_plot(cors_freq_all_r, left_margin = 16)
dev.off()



## Intensity across conflicts ----------------------------------------------

final_items_intensity_all <- read_csv(here("ML results", "final_var_intensity_all.csv")) %>% pull()
intensity_all_dat <- data %>% dplyr::select(all_of(final_items_intensity_all))

# assign items to composites based on the results of the EFA 
composites <- list(behavior_initiation_6 = c("hoyle12", "hoyle4", "ipc5_6", "ipc5_7", "ips1", "scs06"),
                   worry_5 = c("bfi10", "bisbas13", "bisbas19", "bisbas24", "bisbas8"),
                   temptation_resistance_2 = c("hoyle3", "scs01"),
                   metacognitive_regulation_2 = c("MISCS23", "MISCS25"))


# Define single items for renaming
single_items <- c(bisbas3 = "drive_1",
                  ipc2_4 = "perfectionism_1",
                  ipc1_5 = "selfconfidence_1", 
                  MISCS21 = "metacognitive_knowledge_1", 
                  bfi1 = "talkativeness_1")


## recode an item to match the direction of its composite score
intensity_all_dat$ips1 <- 8 - intensity_all_dat$ips1

intensity_all_dat_final <- prepare_vars(intensity_all_dat, single_items, composites)
intensity_all_dat_final <- cbind(intensity_all_dat_final, data[, "m_intensity_all_ESM"])

#create dataset that contains the composite scores 
intensity_all_composite <- intensity_all_dat_final %>% dplyr::select(behavior_initiation_6, worry_5, temptation_resistance_2, 
                                                                  metacognitive_regulation_2, drive_1, perfectionism_1,
                                                                  selfconfidence_1, metacognitive_knowledge_1, talkativeness_1, 
                                                                  m_intensity_all_ESM)
# create dataset for the robustness check in which only the item with the highest loading is used 
intensity_all_robust <- intensity_all_dat_final %>% dplyr::select(ips1, bisbas24, hoyle3, MISCS25, 
                                                               drive_1, perfectionism_1,selfconfidence_1, 
                                                               metacognitive_knowledge_1, talkativeness_1,
                                                               m_intensity_all_ESM) %>%
  rename(behavior_initiation = ips1, worry= bisbas24, temptation_resistance = hoyle3, 
         metacognitive_regulation = MISCS25, 
         drive = drive_1, perfectionism = perfectionism_1, selfconfidence = selfconfidence_1, 
         metacognitive_knowledge = metacognitive_knowledge_1, talkativeness = talkativeness_1)



cors_intensity_all_c <- final_cors(intensity_all_composite, "m_intensity_all_ESM")
betas_intensity_all_c <- final_betas(intensity_all_composite, "m_intensity_all_ESM")

cors_intensity_all_r <- final_cors(intensity_all_robust, "m_intensity_all_ESM")
betas_intensity_all_r <- final_betas(intensity_all_robust, "m_intensity_all_ESM")

# plot the correlations
png(filename = here("Plots", "Final_correlations_intensity_all.png"),  width = 1800, height = 1000)
par(mfrow = c(1,2), 
    cex = 2)
coefficient_plot(cors_intensity_all_c, left_margin = 16)
coefficient_plot(cors_intensity_all_r, left_margin = 16)
dev.off()



## Success across conflicts ------------------------------------------------
final_items_success_all <- read_csv(here("ML results", "final_var_success_all.csv")) %>% pull()
success_all_dat <- data %>% dplyr::select(final_items_success_all)


# compute composite scores for the final ML analysis 
composites <- list(capacity_selfcontrol_8 = c("ipc4_3", "ipc4_6", "ipc5_2", "ips5", "scs09", "stst10",
                                              "stst14","stst16"), 
                   metacognition_5 = c("MISCS11", "MISCS12", "MISCS15", "MISCS21", "MISCS4"),
                   impulsive_behavior_2 = c("bisf3", "bisf5"))

# Define single items for renaming
single_items <- c(ipc6_2 = "error_avoidance_1", 
                  ipc3_8 = "honesty_1")

# recode items to match composite scores
success_all_dat$ips5 <- 8 - success_all_dat$ips5

success_all_dat_final <- prepare_vars(success_all_dat, single_items, composites)
success_all_dat_final <- cbind(success_all_dat_final, data[, "m_success_all_ESM"]) %>%filter(!is.na(m_success_all_ESM))

#create dataset that contains the composite scores 
success_all_composite <- success_all_dat_final %>% dplyr::select(capacity_selfcontrol_8, metacognition_5,
                                                                 impulsive_behavior_2, error_avoidance_1,
                                                                 honesty_1, m_success_all_ESM)

#robustness check: do not use the composite scores but instead take the items with the highest loadings 
success_all_robust <- success_all_dat_final %>% dplyr::select(scs09, MISCS12, bisf3, error_avoidance_1,
                                       honesty_1, m_success_all_ESM) %>%
                          rename(
                            capacity_selfcontrol = scs09,
                            metacognition = MISCS12,
                            impulsive_behavior = bisf3,
                            error_avoidance = error_avoidance_1, 
                            honesty = honesty_1
                          )


cors_success_all_c <- final_cors(success_all_composite, "m_success_all_ESM")
betas_success_all_c <- final_betas(success_all_composite, "m_success_all_ESM")

cors_success_all_r <- final_cors(success_all_robust, "m_success_all_ESM")
betas_success_all_r <- final_betas(success_all_robust, "m_success_all_ESM")

# plot the correlations
png(filename = here("Plots", "Final_correlations_success_all.png"),  width = 1800, height = 1000)
par(mfrow = c(1,2), 
    cex = 2)
coefficient_plot(cors_success_all_c, left_margin = 16)
coefficient_plot(cors_success_all_r, left_margin = 16)
dev.off()


## Frequency initiation conflicts ------------------------------------------

final_items_freq_init <- read_csv(here("ML results", "final_var_frequency_init.csv")) %>% pull()
freq_init_dat <- data %>% dplyr::select(all_of(final_items_freq_init))

# compute composite scores for the final ML analysis 
composites <- list(diligence_compliance_4 = c("ipc2_5", "ipc3_1", "ipc6_6", "scs04"),
                   capacity_selfcontrol_4 = c("hoyle12", "hoyle3", "stst10", "stst16"))


# Define single items for renaming
single_items <- c(
  bisf14 = "inattention_1",
  MISCS4 = "metacognitive_knowledge_1"
)

freq_init_dat_final <- prepare_vars(freq_init_dat, single_items, composites)

freq_init_dat_final <- cbind(freq_init_dat_final, data[, "freq_con_init_ESM"]) %>% filter(!is.na(freq_con_init_ESM))

#create dataset that contains the composite scores 
freq_init_composite <- freq_init_dat_final %>% dplyr::select(capacity_selfcontrol_4, diligence_compliance_4, inattention_1,
                                                          metacognitive_knowledge_1, freq_con_init_ESM)

#robustness check: only use the item with the highest loading
freq_init_robust <- freq_init_dat_final %>% dplyr::select(scs04, stst16, inattention_1,
                                                       metacognitive_knowledge_1, freq_con_init_ESM) %>% 
  rename(diligence_compliance = scs04,capacity_selfcontrol= stst16, inattention = inattention_1,
         metacognitive_knowledge = metacognitive_knowledge_1)


cors_freq_init_c <- final_cors(freq_init_composite, "freq_con_init_ESM")
betas_freq_init_c <- final_betas(freq_init_composite, "freq_con_init_ESM")

cors_freq_init_r <- final_cors(freq_init_robust, "freq_con_init_ESM")
betas_freq_init_r <- final_betas(freq_init_robust, "freq_con_init_ESM")

# plot the correlations
png(filename = here("Plots", "Final_correlations_frequency_init.png"),  width = 1800, height = 1000)
par(mfrow = c(1,2), 
    cex = 2)
coefficient_plot(cors_freq_init_c, left_margin = 16)
coefficient_plot(cors_freq_init_r, left_margin = 16)
dev.off()


## Frequency persistence conflicts -----------------------------------------

final_items_freq_pers <- read_csv(here("ML results", "final_var_frequency_pers.csv")) %>% pull()
freq_pers_dat <- data %>% dplyr::select(all_of(final_items_freq_pers))


# compute composite scores for the final ML analysis 
composites <- list(regulation_5 = c("ERQ5", "ERQ3", "MISCS12", "MISCS19", "MISCS23"), 
                   diligence_compliance_2 = c("ipc2_5", "ipc3_6")) 


single_items <- c(bfi1 = "talkativeness_1",
                  ipc2_2 = "tidiness_1",
                  ips2 = "postpone_tasks_1",
                  MISCS4 = "metacognitive_knowledge_1")


freq_pers_dat_final <- prepare_vars(freq_pers_dat, single_items, composites)

freq_pers_dat_final <- cbind(freq_pers_dat_final, data[, "freq_con_pers_ESM"]) %>% filter(!is.na(freq_con_pers_ESM))

#create dataset that contains the composite scores 
freq_pers_composite <- freq_pers_dat_final %>% dplyr::select(regulation_5, diligence_compliance_2, tidiness_1,
                                                          postpone_tasks_1, metacognitive_knowledge_1,
                                                          talkativeness_1, freq_con_pers_ESM)

#robustness check: only use the item with the highest loading
freq_pers_robust <- freq_pers_dat_final %>% dplyr::select(MISCS19, ipc2_5, tidiness_1, postpone_tasks_1,
                                                       metacognitive_knowledge_1, talkativeness_1, freq_con_pers_ESM) %>% 
  rename(diligence_compliance = ipc2_5, regulation = MISCS19, 
         tidiness = tidiness_1, postpone_tasks = postpone_tasks_1, 
         metacognitive_knowledge = metacognitive_knowledge_1, talkativeness = talkativeness_1)

cors_freq_pers_c <- final_cors(freq_pers_composite, "freq_con_pers_ESM")
betas_freq_pers_c <- final_betas(freq_pers_composite, "freq_con_pers_ESM")

cors_freq_pers_r <- final_cors(freq_pers_robust, "freq_con_pers_ESM")
betas_freq_pers_r <- final_betas(freq_pers_robust, "freq_con_pers_ESM")

# plot the correlations
png(filename = here("Plots", "Final_correlations_frequency_pers.png"),  width = 1800, height = 1000)
par(mfrow = c(1,2), 
    cex = 2)
coefficient_plot(cors_freq_pers_c, left_margin = 16)
coefficient_plot(cors_freq_pers_r, left_margin = 16)
dev.off()




## Intensity initiation conflicts ------------------------------------------
final_items_intensity_init <- read_csv(here("ML results", "final_var_intensity_init.csv")) %>% pull()
intensity_init_dat <- data %>% dplyr::select(all_of(final_items_intensity_init))

intensity_init_dat <- cbind(intensity_init_dat, data[, "m_intensity_init_ESM"]) %>%
  filter(!is.na(m_intensity_init_ESM))

intensity_init_dat_final <- intensity_init_dat %>% rename(behavior_initiation_1 = ipc5_7, worry_1 = bisbas24)


intensity_init_final <- intensity_init_dat_final %>% dplyr::select(behavior_initiation_1, worry_1, m_intensity_init_ESM)

# no robsutness check since no composite scores were computed

cors_intensity_init <- final_cors(intensity_init_final, "m_intensity_init_ESM")
betas_intensity_init <- final_betas(intensity_init_final, "m_intensity_init_ESM")


# plot the correlations
png(filename = here("Plots", "Final_correlations_intensity_init.png"),  width = 900, height = 1000)
par(mfrow = c(1,1), 
    cex = 2)
coefficient_plot(cors_freq_pers_c, left_margin = 16)
dev.off()


## Intensity persistence conflicts -----------------------------------------
final_items_intensity_pers <- read_csv(here("ML results", "final_var_intensity_pers.csv")) %>% pull()
intensity_pers_dat <- data %>% dplyr::select(all_of(final_items_intensity_pers))

# compute composite scores for the final ML analysis 
composites <- list(worry_5 = c("bisbas13", "bisbas19", "bisbas22", "bisbas24", "bisbas8"), 
                   behavior_initiation_3 = c("hoyle4", "ipc5_6", "ips9"),
                   temptation_resistance_2 = c("hoyle1", "hoyle3"), 
                   social_impulsivity_3 = c("bfi1", "scs04", "stst4"))

single_items <- c(bisbas9 = "drive_1",
                  ipc1_5 = "selfconfidence_1",
                  ipc2_4 = "perfectionism_1", 
                  ips2 = "postpone_tasks_1", 
                  MISCS21 = "metacognitive_knowledge_1")

# recode item to match direction of the composite score 
intensity_pers_dat[c("ips9", "scs04", "stst4")] <- 8 - intensity_pers_dat[c("ips9", "scs04", "stst4")]
intensity_pers_dat_final <- prepare_vars(intensity_pers_dat, single_items, composites)


intensity_pers_dat_final <- cbind(intensity_pers_dat_final, data[, "m_intensity_pers_ESM"]) %>%
  filter(!is.na(m_intensity_pers_ESM))

#create dataset that contains the composite scores 
intensity_pers_composite <- intensity_pers_dat_final %>% 
  dplyr::select(worry_5, behavior_initiation_3, temptation_resistance_2, 
                social_impulsivity_3, drive_1, perfectionism_1, postpone_tasks_1, selfconfidence_1,
               metacognitive_knowledge_1, m_intensity_pers_ESM)

#robustness check: only use the item with the highest loading
intensity_pers_robust <- intensity_pers_dat_final %>% dplyr::select(bisbas8, ips9, hoyle3, scs04, 
                                                                 drive_1, selfconfidence_1,
                                                                 perfectionism_1, postpone_tasks_1, metacognitive_knowledge_1, 
                                                                  m_intensity_pers_ESM) %>% 
  rename(worry = bisbas8, behavior_initation = ips9, temptation_resistance = hoyle3, 
         social_impulsivity = scs04, drive = drive_1,
         selfconfidence = selfconfidence_1, perfectionism = perfectionism_1, 
         postpone_tasks = postpone_tasks_1, metacognitive_knowledge = metacognitive_knowledge_1)


cors_intensity_pers_c <- final_cors(intensity_pers_composite, "m_intensity_pers_ESM")
betas_intensity_pers_c <- final_betas(intensity_pers_composite, "m_intensity_pers_ESM")

cors_intensity_pers_r <- final_cors(intensity_pers_robust, "m_intensity_pers_ESM")
betas_intensity_pers_r <- final_betas(intensity_pers_robust, "m_intensity_pers_ESM")

# plot the correlations
png(filename = here("Plots", "Final_correlations_intensity_pers.png"),  width = 1800, height = 1000)
par(mfrow = c(1,2), 
    cex = 2)
coefficient_plot(cors_intensity_pers_c, left_margin = 16)
coefficient_plot(cors_intensity_pers_r, left_margin = 16)
dev.off()




## Success initiation conflicts --------------------------------------------
final_items_success_init <- read_csv(here("ML results", "final_var_success_init.csv")) %>% pull()
success_init_dat <- data %>% dplyr::select(all_of(final_items_success_init))


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


## recode variables with negative loadins
success_init_dat[,c("ips5", "ips7", "scs08", "stst1")] <-  8 - success_init_dat[,c("ips5", "ips7", "scs08","stst1")]  


success_init_dat <- prepare_vars(success_init_dat, single_items, composites)

#create dataset that contains the composite scores 
success_init_dat_final <- cbind(success_init_dat, data[, "m_success_init_ESM"]) %>% filter(!is.na(m_success_init_ESM))
success_init_composite <-success_init_dat_final %>% dplyr::select(behavior_initiation_6, attentional_impulsivity_2,
                                                                  impulsive_behavior_3, preserverance_2, metacognitive_knowledge_4,
                                                                  depression_1, talkativeness_1, working_towards_goals_1,
                                                                  plan_implementation_1,persistence_after_success_1, m_success_init_ESM) 


#robustness check: do not use the composite scores but instead take the items with the highest loadings 
success_init_robust <- success_init_dat_final %>% dplyr::select(ipc5_7, scs08, stst10, MISCS18, bisf5, talkativeness_1, depression_1, 
                                                                working_towards_goals_1, plan_implementation_1, persistence_after_success_1, m_success_init_ESM) %>% 
  rename(behavior_initiation = ipc5_7,  attentional_impulsivity= scs08, preserverance = stst10, metacognitive_knowledge = MISCS18, 
         impulsive_behavior = bisf5, talkativeness = talkativeness_1, depression = depression_1, working_towards_goals = working_towards_goals_1, 
         plan_implementation = plan_implementation_1, persistence_after_success = persistence_after_success_1)


cors_success_init_c <- final_cors(success_init_composite, "m_success_init_ESM")
betas_success_init_c <- final_betas(success_init_composite, "m_success_init_ESM")

cors_success_init_r <- final_cors(success_init_robust, "m_success_init_ESM")
betas_success_init_r <- final_betas(success_init_robust, "m_success_init_ESM")

# plot the correlations
png(filename = here("Plots", "Final_correlations_success_init.png"),  width = 1800, height = 1000)
par(mfrow = c(1,2), 
    cex = 2)
coefficient_plot(cors_success_init_c, left_margin = 16)
coefficient_plot(cors_success_init_r, left_margin = 16)
dev.off()




## Success persistence conflicts -------------------------------------------
final_items_success_pers <- read_csv(here("ML results", "final_var_success_pers.csv")) %>% pull()
success_pers_dat <- data %>% dplyr::select(all_of(final_items_success_pers))

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
                  

## first recode some variables that have to match the direction of their composite score

success_pers_dat$ips5 =  8- success_pers_dat$ips5

success_pers_dat <- prepare_vars(success_pers_dat, single_items, composites)


#create dataset that contains the composite scores 
success_pers_dat_final <- cbind(success_pers_dat, data[, "m_success_pers_ESM"]) %>% filter(!is.na(m_success_pers_ESM))
success_pers_composite <-success_pers_dat_final %>% dplyr::select(self_discipline_5, metacognition_3, ease_persistence_1,
                                                                  depression_1, reward_responsiveness_1,competence_1, 
                                                                  refocusing_after_distraction_1, persistence_despite_fatigue_1, m_success_pers_ESM)




#robustness check: do not use the composite scores but instead take the items with the highest loadings 
success_pers_robust <- success_pers_dat_final %>% dplyr::select(ipc5_2, MISCS12, ease_persistence_1, depression_1, reward_responsiveness_1,
                                                                competence_1, refocusing_after_distraction_1, persistence_despite_fatigue_1, m_success_pers_ESM) %>% 
                          rename(self_discipline = ipc5_2, metacognition = MISCS12, depression = depression_1, reward_responsiveness = reward_responsiveness_1, 
                                 ease_persistence = ease_persistence_1, competence = competence_1, refocusing_after_distraction = refocusing_after_distraction_1, 
                                 persistence_despite_fatigue = persistence_despite_fatigue_1)


cors_success_pers_c <- final_cors(success_pers_composite, "m_success_pers_ESM")
betas_success_pers_c <- final_betas(success_pers_composite, "m_success_pers_ESM")

cors_success_pers_r <- final_cors(success_pers_robust, "m_success_pers_ESM")
betas_success_pers_r <- final_betas(success_pers_robust, "m_success_pers_ESM")

# plot the correlations
png(filename = here("Plots", "Final_correlations_success_pers.png"),  width = 1800, height = 1000)
par(mfrow = c(1,2), 
    cex = 2)
coefficient_plot(cors_success_pers_c, left_margin = 16)
coefficient_plot(cors_success_pers_r, left_margin = 16)
dev.off()




# RQ1 & RQ2 (Figure 2)  --------------------------------------------------------
### Plot the correlational results using the composite scores for Figure 2 of the manuscript 
#create empty data frame for all aspects for inhibition conflicts which could not be predicted 
inhib <- data.frame(var = character(0), abs_mean = numeric(0), direction = character(0))

# list containing the correlational results using composite scors 
stats_data_list <- list(cors_freq_init_c, cors_intensity_init, cors_success_init_c, 
                        cors_freq_pers_c, cors_intensity_pers_c, cors_success_pers_c, 
                        inhib, inhib, inhib, 
                        cors_freq_all_c, cors_intensity_all_c, cors_success_all_c)



graphics.off()  # Fully reset graphics
png(filename = here("Plots", "Overview_final_correlations.png"),  width = 3600, height = 3000, res = 300)
par(mfrow = c(1,1))

# Define a layout matrix (5 rows, 4 columns: 1 row for column labels, 1 column for row labels)
layout_matrix <- matrix(
  c(0, 1, 2, 3,   
    4, 5, 6, 7,   
    8, 9, 10, 11,  
    12, 13, 14, 15,  
    16, 17, 18, 19),  
  nrow = 5, byrow = TRUE
)

layout(layout_matrix, widths = c(1.5, 3, 3, 3), heights = c(0.5, 2, 2, 2, 2))  

# Define column and row names
column_names <- c("Frequency", "Intensity", "Success")
row_names <- c("Initiation Conflicts", "Persistence Conflicts", "Inhibition Conflicts", "Overall")


# Reduce margins for text-only plots
par(mar = c(1, 1, 1, 1))

# Insert column names at the top
for (i in 1:3) {
  plot.new()  # Create an empty plot space inside layout
  text(0.5, 0.5, column_names[i], cex = 1.5, font = 2)
}

plot_index <- 1  

for (row in 1:4) {
   par(mar = c(1, 2, 4, 2))  # Reduce side margins to pull text closer
  plot.new()
  text(0.4, 0.5, row_names[row], cex = 1.5, font = 2, srt = 90)
  
  for (col in 1:3) {
    coefficient_plot(stats_data_list[[plot_index]])
    plot_index <- plot_index + 1
  }
}

dev.off()

# Additional results RQ1 & RQ2 ------------------------------------------
### also plot the results of the partial regression coefficients using the composite scores and the robustness check

# list containing the partial regression results using composite scores
beta_composites_list <- list(betas_freq_init_c, betas_intensity_init, betas_success_init_c, 
                        betas_freq_pers_c, betas_intensity_pers_c, betas_success_pers_c, 
                        inhib, inhib, inhib, 
                        betas_freq_all_c, betas_intensity_all_c, betas_success_all_c)

graphics.off()  # Fully reset graphics
png(filename = here("Plots", "Overview_final_betas.png"),  width = 3600, height = 3000, res = 300)
par(mfrow = c(1,1))

# Define a layout matrix (5 rows, 4 columns: 1 row for column labels, 1 column for row labels)
layout_matrix <- matrix(
  c(0, 1, 2, 3,   
    4, 5, 6, 7,   
    8, 9, 10, 11,  
    12, 13, 14, 15,  
    16, 17, 18, 19),  
  nrow = 5, byrow = TRUE
)

layout(layout_matrix, widths = c(1.5, 3, 3, 3), heights = c(0.5, 2, 2, 2, 2))  

# Define column and row names
column_names <- c("Frequency", "Intensity", "Success")
row_names <- c("Initiation Conflicts", "Persistence Conflicts", "Inhibition Conflicts", "Overall")


# Reduce margins for text-only plots
par(mar = c(1, 1, 1, 1))

# Insert column names at the top
for (i in 1:3) {
  plot.new()  # Create an empty plot space inside layout
  text(0.5, 0.5, column_names[i], cex = 1.5, font = 2)
}

plot_index <- 1  

for (row in 1:4) {
  par(mar = c(1, 2, 4, 2))  # Reduce side margins to pull text closer
  plot.new()
  text(0.4, 0.5, row_names[row], cex = 1.5, font = 2, srt = 90)
  
  for (col in 1:3) {
    coefficient_plot(beta_composites_list[[plot_index]])
    plot_index <- plot_index + 1
  }
}

dev.off()


# robustness check
beta_robust_list <- list(betas_freq_init_r, betas_intensity_init, betas_success_init_r,
                        betas_freq_pers_r, betas_intensity_pers_r, betas_success_pers_r,
                        inhib, inhib, inhib,
                        betas_freq_all_r, betas_intensity_all_r, betas_success_all_r)

graphics.off()  # Fully reset graphics
png(filename = here("Plots", "Overview_final_betas_robust.png"),  width = 3600, height = 3000, res = 300)
par(mfrow = c(1,1))

# Define a layout matrix (5 rows, 4 columns: 1 row for column labels, 1 column for row labels)
layout_matrix <- matrix(
  c(0, 1, 2, 3,   
    4, 5, 6, 7,   
    8, 9, 10, 11,  
    12, 13, 14, 15,  
    16, 17, 18, 19),  
  nrow = 5, byrow = TRUE
)

layout(layout_matrix, widths = c(1.5, 3, 3, 3), heights = c(0.5, 2, 2, 2, 2))  

# Define column and row names
column_names <- c("Frequency", "Intensity", "Success")
row_names <- c("Initiation Conflicts", "Persistence Conflicts", "Inhibition Conflicts", "Overall")


# Reduce margins for text-only plots
par(mar = c(1, 1, 1, 1))

# Insert column names at the top
for (i in 1:3) {
  plot.new()  # Create an empty plot space inside layout
  text(0.5, 0.5, column_names[i], cex = 1.5, font = 2)
}

plot_index <- 1  

for (row in 1:4) {
  par(mar = c(1, 2, 4, 2))  # Reduce side margins to pull text closer
  plot.new()
  text(0.4, 0.5, row_names[row], cex = 1.5, font = 2, srt = 90)
  
  for (col in 1:3) {
    coefficient_plot(beta_robust_list[[plot_index]])
    plot_index <- plot_index + 1
  }
}

dev.off()


# RQ3 (Table 6) -----------------------------------------------------------

# create output table 
set.seed(18237236)

model <- c("frequency across self-control conflicts", "intensity across self-control conflicts", "success across self-control conflicts",
           "frequency initiation conflicts", "frequency persistence conflicts", "frequency inhibition conflicts", 
           "intensity initiation conflicts", "intensity persistence conflicts", "intensity inhibition conflicts", 
           "success initiation conflicts", "success persistence conflicts", "success inhibition conflicts")

final_items <- list(final_items_freq_all, final_items_intensity_all, final_items_success_all, 
                    final_items_freq_init, final_items_freq_pers, NA, 
                    final_items_intensity_init, final_items_intensity_pers, NA, 
                    final_items_success_init, final_items_success_pers, NA)
outcome_vars <- c("freq_con_all_ESM", "m_intensity_all_ESM", "m_success_all_ESM", 
                  "freq_con_init_ESM", "freq_con_pers_ESM", NA, 
                  "m_intensity_init_ESM", "m_intensity_pers_ESM", NA, 
                  "m_success_init_ESM", "m_success_pers_ESM", NA)


# Initialize output table
results_table <- data.frame(
  Model = character(),
  Narrow_items = integer(),
  R2_narrow = numeric(),
  Other_items = integer(),
  Delta_R2 = numeric(),
  SC_items = character(),
  Other_items = character(),
  stringsAsFactors = FALSE
)

for (i in seq_along(model)) {
  
  # Handle inhibition models explicitly
  if (grepl("inhibition", model[i])) {
    results_table <- rbind(results_table, data.frame(
      Model = model[i],
      Narrow_items = 0,
      R2_narrow = NA,
      Other_items = 0,
      Delta_R2 = NA,
      SC_items = "",
      Other_items = "",
      stringsAsFactors = FALSE
    ))
    next
  }
  
  # Get full item set
  all_items <- final_items[[i]]
  
  # Get narrow SC items
  narrow_items <- codebook %>%
    filter(item_name %in% final_items[[i]], narrow_SC == "yes") %>%
    pull(item_name)
  
  # Subset data for narrow model
  narrow_data <- data %>%
    dplyr::select(all_of(narrow_items), all_of(outcome_vars[i])) %>%
    filter(!is.na(.data[[outcome_vars[i]]]))

  all_data <- data %>% 
    dplyr::select(all_of(all_items), all_of(outcome_vars[i])) %>%
    filter(!is.na(.data[[outcome_vars[i]]]))
  
  # Run unregularized models
  results_narrow <- unregularized_analysis(narrow_data, outcome_vars[i])
  output_narrow <- performance_comparison(results_narrow)
  r2_narrow <- output_narrow$avg_rsq
  
  results_all <- unregularized_analysis(all_data, outcome_vars[i])
  output_all <- performance_comparison(results_all)
  delta_r2 <- output_all$avg_rsq - r2_narrow
  
  # Create item summaries
  narrow_list <- paste(narrow_items, collapse = ", ")
  other_items <- setdiff(all_items, narrow_items)
  other_list <- paste(other_items, collapse = ", ")
  
  # Add to table
  results_table <- rbind(results_table, data.frame(
    Model = model[i],
    Narrow_items = length(narrow_items),
    R2_narrow = round(r2_narrow, 3),
    Other_items = length(other_items),
    Delta_R2 = round(delta_r2, 3),
    SC_items = narrow_list,
    Other_items = other_list,
    stringsAsFactors = FALSE
  ))
}

writexl::write_xlsx(results_table, here("Tables", "RQ3_Output.xlsx"))



