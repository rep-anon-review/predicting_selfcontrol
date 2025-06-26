# Load packages and data --------------------------------------------------
library(tidyverse)
library(here)
library(corrplot)
library(psych)
library(writexl)
library(grid)
library(performance)
source(here("Scripts", "01_functions.R"))

data <- read_csv(here("Data", "prep_data.csv"))
codebook <- readxl::read_excel(here("Data", "codebook.xlsx"))


# Frequency across conflicts ----------------------------------------------
final_items_freq_all <- read_csv(here("ML results", "final_var_frequency_all.csv")) %>% pull()
freq_all_dat <- data %>% dplyr::select(all_of(final_items_freq_all))

# plot the distributions of the final variables
if (!dir.exists("Plots")) dir.create("Plots", recursive = TRUE)
png(filename = here("Plots", "freq_all_items_distributions.png"),  width = 1200, height = 1000)

par(mfrow = c(4, 4),         
    oma = c(0, 0, 6, 0),
    mar = c(4, 4, 2, 1),
    cex.main = 2,           
    cex.lab = 1.3,            
    cex.axis = 1.2)    

for(v in final_items_freq_all){
  plot <- hist(freq_all_dat[,v, drop = TRUE],  freq = TRUE, cex.axis = 1, breaks= 30,  cex.lab = 1, xlab= "", ylab="", main = v)
}

mtext("Distributions of final items for the frequency across conflicts", outer = TRUE, cex = 1.5) 
dev.off()


# bivariate correlations among the items 
par(mfrow = c(1,1))
png(filename = here("Plots", "freq_all_items_correlations.png"),  width = 1200, height = 1000)
freq_all_items_cors <- cor(freq_all_dat, use ="complete.obs")
corrplot(freq_all_items_cors, type = "lower", method = "square", tl.cex= 2)
grid.text("Correlations between the final items for the frequency across conflicts",
          x = 0.5, y = 0.95, gp = gpar(fontsize = 20))
dev.off()


# check requirements for EFA
check_factorstructure(freq_all_dat)

# Identifying the number of factors
fa.parallel(freq_all_dat, n.iter=10000) # 3 factors 
vss(freq_all_dat) # suggest 1 factors 

# Test the different solutions using EFA

# Three factors oblimin rotation
EFA_freq_all_obl_3<- fa(freq_all_dat, nfactors = 3, rotate= "oblimin")
loadings_freq_all_obl_3 <- data.frame(matrix(EFA_freq_all_obl_3$loadings, ncol = 3)) %>%
  # omit all laodings below .1 for enhanced readibility 
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>%
  round(2)

# Three factors promax rotation
EFA_freq_all_pro_3<- fa(freq_all_dat, nfactors = 3, rotate= "promax")
loadings_freq_all_pro_3 <- data.frame(matrix(EFA_freq_all_pro_3$loadings, ncol = 3)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

#  exploratory two factor solution oblimin rotation
EFA_freq_all_obl_2<- fa(freq_all_dat, nfactors = 2, rotate= "oblimin")
loadings_freq_all_obl_2 <- data.frame(matrix(EFA_freq_all_obl_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>%
  round(2)


# exploratory two factor solution promax rotation
EFA_freq_all_pro_2<- fa(freq_all_dat, nfactors = 2, rotate= "promax")
loadings_freq_all_pro_2 <- data.frame(matrix(EFA_freq_all_pro_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>%
  round(2)


# add item text 
item_text_freq_all <- codebook %>% 
  filter(item_name %in% final_items_freq_all) %>% 
  dplyr::select(item_name, item_text_english)

# bring them in order 
item_text_freq_all <- item_text_freq_all[match(final_items_freq_all, item_text_freq_all$item_name), ]

# add bivariate correlations between items and predictor 
freq_predictor_items_cors <- round(cor(freq_all_dat, data$freq_con_all_ESM, use ="complete.obs"),2)

if (!dir.exists("EFA results")) dir.create("EFA results", recursive = TRUE)
loadings_freq_all_obl_3 <- cbind(item_text_freq_all, loadings_freq_all_obl_3, freq_predictor_items_cors)
write_xlsx(loadings_freq_all_obl_3, here("EFA results", "freq_all_obl_3.xlsx"))

loadings_freq_all_pro_3 <- cbind(item_text_freq_all, loadings_freq_all_pro_3, freq_predictor_items_cors)
write_xlsx(loadings_freq_all_pro_3, here("EFA results", "freq_all_pro_3.xlsx"))

loadings_freq_all_obl_2 <- cbind(item_text_freq_all, loadings_freq_all_obl_2, freq_predictor_items_cors)
write_xlsx(loadings_freq_all_obl_2, here("EFA results", "freq_all_obl_2.xlsx"))

loadings_freq_all_pro_2 <- cbind(item_text_freq_all, loadings_freq_all_pro_2, freq_predictor_items_cors)
write_xlsx(loadings_freq_all_pro_2, here("EFA results", "freq_all_pro_2.xlsx"))



# Intensity across conflicts ----------------------------------------------
final_items_intensity_all <- read_csv(here("ML results", "final_var_intensity_all.csv")) %>% pull()
intensity_all_dat <- data %>% dplyr::select(all_of(final_items_intensity_all))


png(filename = here("Plots", "intensity_all_items_distributions.png"),  width = 1200, height = 1000)

par(mfrow = c(5, 4),         
    oma = c(0, 0, 6, 0),
    mar = c(4, 4, 2, 1),
    cex.main = 2,           
    cex.lab = 1.3,            
    cex.axis = 1.2)    

for(v in final_items_intensity_all){
  plot <- hist(intensity_all_dat[,v, drop = TRUE],  freq = TRUE, cex.axis = 1, breaks= 30,  cex.lab = 1, xlab= "", ylab="", main = v)
}

mtext("Distributions of final items for the intensity across conflicts", outer = TRUE, cex = 1.5) 
dev.off()


# bivariate correlations among the items 
par(mfrow = c(1,1))
png(filename = here("Plots", "intensity_all_items_correlations.png"),  width = 1200, height = 1000)
intensity_all_items_cors <- cor(intensity_all_dat, use ="complete.obs")
corrplot(intensity_all_items_cors, type = "lower", method = "square", tl.cex= 2)
grid.text("Correlations between the final items for the intensity across conflicts",
          x = 0.5, y = 0.95, gp = gpar(fontsize = 20))
dev.off()


# check requirements 
check_factorstructure(intensity_all_dat)


# Identifying the number of factors
fa.parallel(intensity_all_dat, n.iter=10000) # 5 factors 
vss(intensity_all_dat) # suggest 3 factors 

# Test the different solutions using EFA
# Five factors oblimin rotation
EFA_intensity_all_obl_5<- fa(intensity_all_dat, nfactors = 5, rotate= "oblimin")
loadings_intensity_all_obl_5 <- data.frame(matrix(EFA_intensity_all_obl_5$loadings, ncol = 5)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Five factors promax rotation
EFA_intensity_all_pro_5<- fa(intensity_all_dat, nfactors = 5, rotate= "promax")
loadings_intensity_all_pro_5 <- data.frame(matrix(EFA_intensity_all_pro_5$loadings, ncol = 5)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Two factors oblimin rotation
EFA_intensity_all_obl_2<- fa(intensity_all_dat, nfactors = 2, rotate= "oblimin")
loadings_intensity_all_obl_2 <- data.frame(matrix(EFA_intensity_all_obl_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Two factors promax rotation
EFA_intensity_all_pro_2<- fa(intensity_all_dat, nfactors = 2, rotate= "promax")
loadings_intensity_all_pro_2 <- data.frame(matrix(EFA_intensity_all_pro_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Four factors oblimin rotation
EFA_intensity_all_obl_4<- fa(intensity_all_dat, nfactors = 4, rotate= "oblimin")
loadings_intensity_all_obl_4 <- data.frame(matrix(EFA_intensity_all_obl_4$loadings, ncol = 4)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Four factors promax rotation
EFA_intensity_all_pro_4<- fa(intensity_all_dat, nfactors = 4, rotate= "promax")
loadings_intensity_all_pro_4 <- data.frame(matrix(EFA_intensity_all_pro_4$loadings, ncol = 4)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

item_text_intensity_all <- codebook %>% 
  filter(item_name %in% final_items_intensity_all) %>% 
  dplyr::select(item_name, item_text_english)

# bring them in order 
item_text_intensity_all <- item_text_intensity_all[match(final_items_intensity_all,
                                                         item_text_intensity_all$item_name), ]


# bivariate correlations between items and predictor 
intensity_predictor_items_cors <- round(cor(intensity_all_dat, data$m_intensity_all_ESM, use= "complete.obs"),2)

#combine 
loadings_intensity_all_obl_5 <- cbind(item_text_intensity_all, loadings_intensity_all_obl_5, intensity_predictor_items_cors)
write_xlsx(loadings_intensity_all_obl_5, here("EFA results", "intensity_all_obl_5.xlsx"))

loadings_intensity_all_pro_5 <- cbind(item_text_intensity_all, loadings_intensity_all_pro_5, intensity_predictor_items_cors)
write_xlsx(loadings_intensity_all_pro_5, here("EFA results", "intensity_all_pro_5.xlsx"))

loadings_intensity_all_obl_2 <- cbind(item_text_intensity_all, loadings_intensity_all_obl_2, intensity_predictor_items_cors)
write_xlsx(loadings_intensity_all_obl_2, here("EFA results", "intensity_all_obl_2.xlsx"))

loadings_intensity_all_pro_2 <- cbind(item_text_intensity_all, loadings_intensity_all_pro_2, intensity_predictor_items_cors)
write_xlsx(loadings_intensity_all_pro_2, here("EFA results", "intensity_all_pro_2.xlsx"))

loadings_intensity_all_obl_4 <- cbind(item_text_intensity_all, loadings_intensity_all_obl_4, intensity_predictor_items_cors)
write_xlsx(loadings_intensity_all_obl_4, here("EFA results", "intensity_all_obl_4.xlsx"))

loadings_intensity_all_pro_4 <- cbind(item_text_intensity_all, loadings_intensity_all_pro_4, intensity_predictor_items_cors)
write_xlsx(loadings_intensity_all_pro_4, here("EFA results", "intensity_all_pro_4.xlsx"))


# Success across conflicts ------------------------------------------------

final_items_success_all <- read_csv(here("ML results", "final_var_success_all.csv")) %>% pull()
success_all_dat <- data %>% dplyr::select(final_items_success_all)


png(filename = here("Plots", "success_all_items_distributions.png"),  width = 1200, height = 1000)
par(mfrow = c(5, 4),         
    oma = c(0, 0, 6, 0),
    mar = c(4, 4, 2, 1),
    cex.main = 2,           
    cex.lab = 1.3,            
    cex.axis = 1.2)    

for(v in final_items_success_all){
  plot <- hist(success_all_dat[,v, drop = TRUE],  freq = TRUE, cex.axis = 1, breaks= 30,  cex.lab = 1, xlab= "", ylab="", main = v)
}

mtext("Distributions of final items for the success across conflicts", outer = TRUE, cex = 1.5) 
dev.off()


# bivariate correlations among the items 
par(mfrow = c(1,1))
png(filename = here("Plots", "success_all_items_correlations.png"),  width = 1200, height = 1000)
success_all_items_cors <- cor(success_all_dat, use ="complete.obs")
corrplot(success_all_items_cors, type = "lower", method = "square", tl.cex= 2)
grid.text("Correlations between the final items for the success across conflicts",
          x = 0.5, y = 0.95, gp = gpar(fontsize = 20))
dev.off()


check_factorstructure(success_all_dat)

#Identifying the number of factors
fa.parallel(success_all_dat, n.iter=10000) # 5 factors 
vss(success_all_dat) # suggest 1 factor 


# Five factors oblimin rotation
EFA_success_all_obl_5 <- fa(success_all_dat, nfactors = 5, rotate= "oblimin")
loadings_success_all_obl_5 <- data.frame(matrix(EFA_success_all_obl_5$loadings, ncol = 5)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.)< 0.1, NA, .))) %>% 
  round(2)

# Five factors promax rotation
EFA_success_all_pro_5 <- fa(success_all_dat, nfactors = 5, rotate= "promax")
loadings_success_all_pro_5 <- data.frame(matrix(EFA_success_all_pro_5$loadings, ncol = 5)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.)< 0.1, NA, .))) %>% 
  round(2)

# exploratory  three factors oblimin solution
EFA_success_all_obl_3 <- fa(success_all_dat, nfactors = 3, rotate= "oblimin")
loadings_success_all_obl_3<- data.frame(matrix(EFA_success_all_obl_3$loadings, ncol = 3)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.)< 0.1, NA, .))) %>% 
  round(2)

# exploratory  three factors promax solution
EFA_success_all_pro_3 <- fa(success_all_dat, nfactors = 3, rotate= "promax")
loadings_success_all_pro_3<- data.frame(matrix(EFA_success_all_pro_3$loadings, ncol = 3)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.)< 0.1, NA, .))) %>% 
  round(2)


item_text_success_all <- codebook %>% 
  filter(item_name %in% final_items_success_all) %>% 
  dplyr::select(item_name, item_text_english)

# bring them in order 
item_text_success_all <- item_text_success_all[match(final_items_success_all, item_text_success_all$item_name), ]

# bivariate correlations between items and predictor 
success_predictor_items_cors <- round(cor(success_all_dat, data$m_success_all_ESM, use = "complete.obs"),2)

# combine 
loadings_success_all_obl_5 <- cbind(item_text_success_all, loadings_success_all_obl_5, success_predictor_items_cors)
write_xlsx(loadings_success_all_obl_5, here("EFA results", "success_all_obl_5.xlsx"))

loadings_success_all_pro_5 <- cbind(item_text_success_all, loadings_success_all_pro_5, success_predictor_items_cors)
write_xlsx(loadings_success_all_pro_5, here("EFA results", "success_all_pro_5.xlsx"))

loadings_success_all_obl_3<- cbind(item_text_success_all, loadings_success_all_obl_3, success_predictor_items_cors)
write_xlsx(loadings_success_all_obl_3, here("EFA results", "success_all_obl_3.xlsx"))

loadings_success_all_pro_3<- cbind(item_text_success_all, loadings_success_all_pro_3, success_predictor_items_cors)
write_xlsx(loadings_success_all_pro_3, here("EFA results", "success_all_pro_3.xlsx"))


# Frequency initiation conflicts ------------------------------------------
final_items_freq_init <- read_csv(here("ML results", "final_var_frequency_init.csv")) %>% pull()
freq_init_dat <- data %>% dplyr::select(all_of(final_items_freq_init))


png(filename = here("Plots", "freq_init_items_distributions.png"),  width = 1200, height = 1000)
par(mfrow = c(5, 2),         
    oma = c(0, 0, 6, 0),
    mar = c(4, 4, 2, 1),
    cex.main = 2,           
    cex.lab = 1.3,            
    cex.axis = 1.2)    

for(v in final_items_freq_init ){
  plot <- hist(freq_init_dat[,v, drop = TRUE],  freq = TRUE, cex.axis = 1, breaks= 30,  cex.lab = 1, xlab= "", ylab="", main = v)
}

mtext("Distributions of final items for the frequency of initiation conflicts", outer = TRUE, cex = 1.5) 
dev.off()


# bivariate correlations among the items 
par(mfrow = c(1,1))
png(filename = here("Plots", "freq_init_items_correlations.png"),  width = 1200, height = 1000)
freq_init_items_cors <- cor(freq_init_dat, use ="complete.obs")
corrplot(freq_init_items_cors, type = "lower", method = "square", tl.cex= 2)
grid.text("Correlations between the final items for the frequency of initiation conflicts",
          x = 0.5, y = 0.95, gp = gpar(fontsize = 20))
dev.off()

# check requirements 
check_factorstructure(freq_init_dat)

# Identifying the number of factors
fa.parallel(freq_init_dat, n.iter=10000) # 2 factors 
vss(freq_init_dat) # suggest 1 factors 


# Two factors oblimin rotation
EFA_freq_init_obl_2<- fa(freq_init_dat, nfactors = 2, rotate= "oblimin")
loadings_freq_init_obl_2<- data.frame(matrix(EFA_freq_init_obl_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Two factors promax rotation
EFA_freq_init_pro_2<- fa(freq_init_dat, nfactors = 2, rotate= "promax")
loadings_freq_init_pro_2<- data.frame(matrix(EFA_freq_init_pro_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

item_text_freq_init <- codebook %>% 
  filter(item_name %in% final_items_freq_init) %>% 
  dplyr::select(item_name, item_text_english)

# bring them in order 
item_text_freq_init <- item_text_freq_init[match(final_items_freq_init,item_text_freq_init$item_name), ]

# bivariate correlations between items and predictor 
freq_init_predictor_items_cors <- round(cor(freq_init_dat, data$freq_con_init_ESM, use ="complete.obs"),2)

#combine 
loadings_freq_init_obl_2 <- cbind(item_text_freq_init, loadings_freq_init_obl_2, freq_init_predictor_items_cors)
write_xlsx(loadings_freq_init_obl_2, here("EFA results", "freq_init_obl_2.xlsx"))

loadings_freq_init_pro_2 <- cbind(item_text_freq_init, loadings_freq_init_pro_2, freq_init_predictor_items_cors)
write_xlsx(loadings_freq_init_pro_2, here("EFA results", "freq_init_pro_2.xlsx"))


# Frequency persistence conflicts -----------------------------------------
final_items_freq_pers <- read_csv(here("ML results", "final_var_frequency_pers.csv")) %>% pull()
freq_pers_dat <- data %>% dplyr::select(all_of(final_items_freq_pers))

png(filename = here("Plots", "freq_pers_items_distributions.png"),  width = 1200, height = 1000)
par(mfrow = c(4, 3),         
    oma = c(0, 0, 6, 0),
    mar = c(4, 4, 2, 1),
    cex.main = 2,           
    cex.lab = 1.3,            
    cex.axis = 1.2)    

for(v in final_items_freq_pers){
  plot <- hist(freq_pers_dat[,v, drop = TRUE],  freq = TRUE, cex.axis = 1, breaks= 30,  cex.lab = 1, xlab= "", ylab="", main = v)
}

mtext("Distributions of final items for the frequency of persistence conflicts", outer = TRUE, cex = 1.5) 
dev.off()

# bivariate correlations among the items 
par(mfrow = c(1,1))
png(filename = here("Plots", "freq_pers_items_correlations.png"),  width = 1200, height = 1000)
freq_pers_items_cors <- cor(freq_pers_dat, use ="complete.obs")
corrplot(freq_pers_items_cors, type = "lower", method = "square", tl.cex= 2)
grid.text("Correlations between the final items for the frequency of persistence conflicts",
          x = 0.5, y = 0.95, gp = gpar(fontsize = 20))
dev.off()

# check requirements 
check_factorstructure(freq_pers_dat)

# Identifying the number of factors
fa.parallel(freq_pers_dat, n.iter=10000) # 4 factors 
vss(freq_pers_dat) # suggest 1 factors 

# Four factors oblimin rotation
EFA_freq_pers_obl_4<- fa(freq_pers_dat, nfactors = 4, rotate= "oblimin")
loadings_freq_pers_obl_4<- data.frame(matrix(EFA_freq_pers_obl_4$loadings, ncol = 4)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Four factors promax rotation
EFA_freq_pers_pro_4<- fa(freq_pers_dat, nfactors = 4, rotate= "promax")
loadings_freq_pers_pro_4<- data.frame(matrix(EFA_freq_pers_pro_4$loadings, ncol = 4)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)


# Exploratory three factors oblimin rotation
EFA_freq_pers_obl_3<- fa(freq_pers_dat, nfactors = 3, rotate= "oblimin")
loadings_freq_pers_obl_3<- data.frame(matrix(EFA_freq_pers_obl_3$loadings, ncol = 3)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Exploratory three factors promax rotation
EFA_freq_pers_pro_3<- fa(freq_pers_dat, nfactors = 3, rotate= "promax")
loadings_freq_pers_pro_3<- data.frame(matrix(EFA_freq_pers_pro_3$loadings, ncol = 3)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

#  Exploratory two factor solution oblimin rotation
EFA_freq_pers_obl_2<- fa(freq_pers_dat, nfactors = 2, rotate= "oblimin")
loadings_freq_pers_obl_2<- data.frame(matrix(EFA_freq_pers_obl_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Exploratory two factor solution promax rotation
EFA_freq_pers_pro_2<- fa(freq_pers_dat, nfactors = 2, rotate= "promax")
loadings_freq_pers_pro_2<- data.frame(matrix(EFA_freq_pers_pro_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# add item text 
item_text_freq_pers <- codebook %>% 
  filter(item_name %in% final_items_freq_pers) %>% 
  dplyr::select(item_name, item_text_english)

# bring them in order 
item_text_freq_pers <- item_text_freq_pers[match(final_items_freq_pers,item_text_freq_pers$item_name), ]

# bivariate correlations between items and predictor 
freq_pers_predictor_items_cors <- round(cor(freq_pers_dat, data$freq_con_pers_ESM, use ="complete.obs"),2)

#combine 
loadings_freq_pers_obl_4 <- cbind(item_text_freq_pers, loadings_freq_pers_obl_4, freq_pers_predictor_items_cors)
write_xlsx(loadings_freq_pers_obl_4, here("EFA results", "freq_pers_obl_4.xlsx"))

loadings_freq_pers_pro_4 <- cbind(item_text_freq_pers, loadings_freq_pers_pro_4, freq_pers_predictor_items_cors)
write_xlsx(loadings_freq_pers_pro_4, here("EFA results", "freq_pers_pro_3.xlsx"))

loadings_freq_pers_obl_3 <- cbind(item_text_freq_pers, loadings_freq_pers_obl_3, freq_pers_predictor_items_cors)
write_xlsx(loadings_freq_pers_obl_3, here("EFA results", "freq_pers_obl_3.xlsx"))

loadings_freq_pers_pro_3 <- cbind(item_text_freq_pers, loadings_freq_pers_pro_3, freq_pers_predictor_items_cors)
write_xlsx(loadings_freq_pers_pro_3, here("EFA results", "freq_pers_pro_3.xlsx"))

loadings_freq_pers_obl_2 <- cbind(item_text_freq_pers, loadings_freq_pers_obl_2, freq_pers_predictor_items_cors)
write_xlsx(loadings_freq_pers_obl_2, here("EFA results", "freq_pers_obl_2.xlsx"))

loadings_freq_pers_pro_2 <- cbind(item_text_freq_pers, loadings_freq_pers_pro_2, freq_pers_predictor_items_cors)
write_xlsx(loadings_freq_pers_pro_2, here("EFA results", "freq_pers_pro_2.xlsx"))


# Intensity initiation conflicts ------------------------------------------
final_items_intensity_init <- read_csv(here("ML results", "final_var_intensity_init.csv")) %>% pull()
intensity_init_dat <- data %>% dplyr::select(all_of(final_items_intensity_init))

png(filename = here("Plots", "intensity_init_items_distributions.png"),  width = 1200, height = 1000)
par(mfrow = c(2, 1),         
    oma = c(0, 0, 6, 0),
    mar = c(4, 4, 2, 1),
    cex.main = 2,           
    cex.lab = 1.3,            
    cex.axis = 1.2)    

for(v in final_items_intensity_init){
  plot <- hist(intensity_init_dat[,v, drop = TRUE],  freq = TRUE, cex.axis = 1, breaks= 30,  cex.lab = 1, xlab= "", ylab="", main = v)
}

mtext("Distributions of final items for the intensity of initiation conflicts", outer = TRUE, cex = 1.5) 
dev.off()

# bivariate correlations among the items 
par(mfrow = c(1,1))
png(filename = here("Plots", "intensity_init_items_correlations.png"),  width = 1200, height = 1000)
intensity_init_items_cors <- cor(intensity_init_dat, use ="complete.obs")
corrplot(intensity_init_items_cors, type = "lower", method = "square", tl.cex= 2)
grid.text("Correlations between the final items for the intensity of initiation conflicts",
          x = 0.5, y = 0.95, gp = gpar(fontsize = 20))
dev.off()

### No EFA was conducted since only two items were selected 


# Intensity persistence conflicts -----------------------------------------
final_items_intensity_pers <- read_csv(here("ML results", "final_var_intensity_pers.csv")) %>% pull()
intensity_pers_dat <- data %>% dplyr::select(all_of(final_items_intensity_pers))

png(filename = here("Plots", "intensity_pers_items_distributions.png"),  width = 1200, height = 1000)
par(mfrow = c(5, 4),         
    oma = c(0, 0, 6, 0),
    mar = c(4, 4, 2, 1),
    cex.main = 2,           
    cex.lab = 1.3,            
    cex.axis = 1.2)    

for(v in final_items_intensity_pers){
  plot <- hist(intensity_pers_dat[,v, drop = TRUE],  freq = TRUE, cex.axis = 1, breaks= 30,  cex.lab = 1, xlab= "", ylab="", main = v)
}

mtext("Distributions of final items for the intensity of persistence conflicts", outer = TRUE, cex = 1.5) 
dev.off()

# bivariate correlations among the items 
par(mfrow = c(1,1))
png(filename = here("Plots", "intensity_pers_items_correlations.png"),  width = 1200, height = 1000)
intensity_pers_items_cors <- cor(intensity_pers_dat, use ="complete.obs")
corrplot(intensity_pers_items_cors, type = "lower", method = "square", tl.cex= 2)
grid.text("Correlations between the final items for the intensity of persistence conflicts",
          x = 0.5, y = 0.95, gp = gpar(fontsize = 20))
dev.off()


# check requirements 
check_factorstructure(intensity_pers_dat)

# Identifying the number of factors
fa.parallel(intensity_pers_dat, n.iter=10000) # 5 factors 
vss(intensity_pers_dat) # suggest 2 factors 

# Five factors oblimin rotation
EFA_intensity_pers_obl_5<- fa(intensity_pers_dat, nfactors = 5, rotate= "oblimin")
loadings_intensity_pers_obl_5<- data.frame(matrix(EFA_intensity_pers_obl_5$loadings, ncol = 5)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Five factors promax rotation
EFA_intensity_pers_pro_5<- fa(intensity_pers_dat, nfactors = 5, rotate= "promax")
loadings_intensity_pers_pro_5<- data.frame(matrix(EFA_intensity_pers_pro_5$loadings, ncol = 5)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Two factors oblimin rotation
EFA_intensity_pers_obl_2<- fa(intensity_pers_dat, nfactors = 2, rotate= "oblimin")
loadings_intensity_pers_obl_2<- data.frame(matrix(EFA_intensity_pers_obl_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Two factors promax rotation
EFA_intensity_pers_pro_2<- fa(intensity_pers_dat, nfactors = 2, rotate= "promax")
loadings_intensity_pers_pro_2<- data.frame(matrix(EFA_intensity_pers_pro_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Exploratory four factors oblimin rotation
EFA_intensity_pers_obl_4<- fa(intensity_pers_dat, nfactors = 4, rotate= "oblimin")
loadings_intensity_pers_obl_4<- data.frame(matrix(EFA_intensity_pers_obl_4$loadings, ncol = 4)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Exploratory four factors promax rotation
EFA_intensity_pers_pro_4<- fa(intensity_pers_dat, nfactors = 4, rotate= "promax")
loadings_intensity_pers_pro_4<- data.frame(matrix(EFA_intensity_pers_pro_4$loadings, ncol = 4)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)


item_text_intensity_pers <- codebook %>% 
  filter(item_name %in% final_items_intensity_pers) %>% 
  dplyr::select(item_name, item_text_english)

# bring them in order 
item_text_intensity_pers <- item_text_intensity_pers[match(final_items_intensity_pers,item_text_intensity_pers$item_name), ]

# bivariate correlations between items and predictor 
intensity_pers_predictor_items_cors <- round(cor(intensity_pers_dat, data$m_intensity_pers_ESM, use="complete.obs"),2)

#combine 
loadings_intensity_pers_obl_5 <- cbind(item_text_intensity_pers, loadings_intensity_pers_obl_5,
                                       intensity_pers_predictor_items_cors)
write_xlsx(loadings_intensity_pers_obl_5, here("EFA results", "intensity_pers_obl_5.xlsx"))

loadings_intensity_pers_pro_5 <- cbind(item_text_intensity_pers, loadings_intensity_pers_pro_5,
                                       intensity_pers_predictor_items_cors)
write_xlsx(loadings_intensity_pers_pro_5, here("EFA results", "intensity_pers_pro_5.xlsx"))

loadings_intensity_pers_obl_2 <- cbind(item_text_intensity_pers, loadings_intensity_pers_obl_2,
                                       intensity_pers_predictor_items_cors)
write_xlsx(loadings_intensity_pers_obl_2, here("EFA results", "intensity_pers_obl_2.xlsx"))

loadings_intensity_pers_pro_2 <- cbind(item_text_intensity_pers, loadings_intensity_pers_pro_2,
                                       intensity_pers_predictor_items_cors)
write_xlsx(loadings_intensity_pers_pro_2, here("EFA results", "intensity_pers_pro_2.xlsx"))

loadings_intensity_pers_obl_4<- cbind(item_text_intensity_pers, loadings_intensity_pers_obl_4,
                                      intensity_pers_predictor_items_cors)
write_xlsx(loadings_intensity_pers_obl_4, here("EFA results", "intensity_pers_obl_4.xlsx"))

loadings_intensity_pers_pro_4<- cbind(item_text_intensity_pers, loadings_intensity_pers_pro_4,
                                      intensity_pers_predictor_items_cors)
write_xlsx(loadings_intensity_pers_pro_4, here("EFA results", "intensity_pers_pro_4.xlsx"))



# Success initiation conflicts --------------------------------------------
final_items_success_init <- read_csv(here("ML results", "final_var_success_init.csv")) %>% pull()
success_init_dat <- data %>% dplyr::select(all_of(final_items_success_init))

png(filename = here("Plots", "success_init_items_distributions.png"),  width = 1200, height = 1000)
par(mfrow = c(5, 5),         
    oma = c(0, 0, 6, 0),
    mar = c(4, 4, 2, 1),
    cex.main = 2,           
    cex.lab = 1.3,            
    cex.axis = 1.2)    

for(v in final_items_success_init){
  plot <- hist(success_init_dat[,v, drop = TRUE],  freq = TRUE, cex.axis = 1, breaks= 30,  cex.lab = 1, xlab= "", ylab="", main = v)
}

mtext("Distributions of final items for the success of initiation conflicts", outer = TRUE, cex = 1.5) 
dev.off()

# bivariate correlations among the items 
par(mfrow = c(1,1))
png(filename = here("Plots", "success_init_items_correlations.png"),  width = 1200, height = 1000)
success_init_items_cors <- cor(success_init_dat, use ="complete.obs")
corrplot(success_init_items_cors, type = "lower", method = "square", tl.cex= 2)
grid.text("Correlations between the final items for the success of initiation conflicts",
          x = 0.5, y = 0.95, gp = gpar(fontsize = 20))
dev.off()



# check requirements 
check_factorstructure(success_init_dat)


# Identifying the number of factors
fa.parallel(success_init_dat, n.iter=10000) # 7 factors 
vss(success_init_dat) # suggest 1 factors 

# Seven factors oblimin rotation
EFA_success_init_obl_7 <- fa(success_init_dat, nfactors = 7,  rotate= "oblimin")
loadings_success_init_obl_7 <- data.frame(matrix(EFA_success_init_obl_7$loadings, ncol = 7)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Seven factors promax rotation
EFA_success_init_pro_7 <- fa(success_init_dat, nfactors = 7,  rotate= "promax")
loadings_success_init_pro_7 <- data.frame(matrix(EFA_success_init_pro_7$loadings, ncol = 7)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Exploratory five factors oblimin rotation
EFA_success_init_obl_5 <- fa(success_init_dat, nfactors = 5,  rotate= "oblimin")
loadings_success_init_obl_5 <- data.frame(matrix(EFA_success_init_obl_5$loadings, ncol = 5)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>%
  round(2)

# Exploratory five factors promax rotation
EFA_success_init_pro_5 <- fa(success_init_dat, nfactors = 5,  rotate= "promax")
loadings_success_init_pro_5 <- data.frame(matrix(EFA_success_init_pro_5$loadings, ncol = 5)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>%
  round(2)


item_text_success_init <- codebook %>% 
  filter(item_name %in% final_items_success_init) %>% 
  dplyr::select(item_name, item_text_english)


# bring them in order 
item_text_success_init <- item_text_success_init[match(final_items_success_init,
                                                       item_text_success_init$item_name), ]


# bivariate correlations between items and predictor 
success_init_predictor_items_cors <-round(cor(success_init_dat,data$m_success_init_ESM,use="complete.obs"),2)

#combine 
loadings_success_init_obl_7<- cbind(item_text_success_init, loadings_success_init_obl_7, success_init_predictor_items_cors)
write_xlsx(loadings_success_init_obl_7, here("EFA results", "success_init_obl_7.xlsx"))

loadings_success_init_pro_7<- cbind(item_text_success_init, loadings_success_init_pro_7, success_init_predictor_items_cors)
write_xlsx(loadings_success_init_pro_7, here("EFA results", "success_init_pro_7.xlsx"))

loadings_success_init_obl_5<- cbind(item_text_success_init, loadings_success_init_obl_5, success_init_predictor_items_cors)
write_xlsx(loadings_success_init_obl_5, here("EFA results", "success_init_obl_5.xlsx"))

loadings_success_init_pro_5<- cbind(item_text_success_init, loadings_success_init_pro_5, success_init_predictor_items_cors)
write_xlsx(loadings_success_init_pro_5, here("EFA results", "success_init_pro_5.xlsx"))


# Success persistence conflicts -------------------------------------------
final_items_success_pers <- read_csv(here("ML results", "final_var_success_pers.csv")) %>% pull()
success_pers_dat <- data %>% dplyr::select(all_of(final_items_success_pers))

png(filename = here("Plots", "success_pers_items_distributions.png"),  width = 1200, height = 1000)
par(mfrow = c(5, 3),         
    oma = c(0, 0, 6, 0),
    mar = c(4, 4, 2, 1),
    cex.main = 2,           
    cex.lab = 1.3,            
    cex.axis = 1.2)    

for(v in final_items_success_pers){
  plot <- hist(success_pers_dat[,v, drop = TRUE],  freq = TRUE, cex.axis = 1, breaks= 30,  cex.lab = 1, xlab= "", ylab="", main = v)
}

mtext("Distributions of final items for the success of persistence conflicts", outer = TRUE, cex = 1.5) 
dev.off()

# bivariate correlations among the items 
par(mfrow = c(1,1))
png(filename = here("Plots", "success_pers_items_correlations.png"),  width = 1200, height = 1000)
success_pers_items_cors <- cor(success_pers_dat, use ="complete.obs")
corrplot(success_pers_items_cors, type = "lower", method = "square", tl.cex= 2)
grid.text("Correlations between the final items for the success of persistence conflicts",
          x = 0.5, y = 0.95, gp = gpar(fontsize = 20))
dev.off()


# check requirements 
check_factorstructure(success_pers_dat)


# Identifying the number of factors
fa.parallel(success_pers_dat, n.iter=10000) # 4factors 
vss(success_pers_dat) # suggest 1 factors 


# Four factors oblimin rotation
EFA_success_pers_obl_4 <- fa(success_pers_dat, nfactors = 4,  rotate= "oblimin")
loadings_success_pers_obl_4<- data.frame(matrix(EFA_success_pers_obl_4$loadings, ncol = 4)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Four factors promax rotation
EFA_success_pers_pro_4 <- fa(success_pers_dat, nfactors = 4,  rotate= "promax")
loadings_success_pers_pro_4<- data.frame(matrix(EFA_success_pers_pro_4$loadings, ncol = 4)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Exploratory three factors oblimin rotation
EFA_success_pers_obl_3 <- fa(success_pers_dat, nfactors = 3,  rotate= "oblimin")
loadings_success_pers_obl_3<- data.frame(matrix(EFA_success_pers_obl_3$loadings, ncol = 3)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Exploratory three factors promax rotation
EFA_success_pers_pro_3 <- fa(success_pers_dat, nfactors = 3,  rotate= "promax")
loadings_success_pers_pro_3<- data.frame(matrix(EFA_success_pers_pro_3$loadings, ncol = 3)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Exploratory two factors oblimin rotation
EFA_success_pers_obl_2 <- fa(success_pers_dat, nfactors = 2,  rotate= "oblimin")
loadings_success_pers_obl_2<- data.frame(matrix(EFA_success_pers_obl_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

# Exploratory two factors promax rotation
EFA_success_pers_pro_2 <- fa(success_pers_dat, nfactors = 2,  rotate= "promax")
loadings_success_pers_pro_2<- data.frame(matrix(EFA_success_pers_pro_2$loadings, ncol = 2)) %>%
  mutate(across(where(is.numeric), ~ ifelse(abs(.) < 0.1, NA, .))) %>% 
  round(2)

item_text_success_pers <- codebook %>% 
  filter(item_name %in% final_items_success_pers) %>% 
  dplyr::select(item_name, item_text_english)


# bring them in order 
item_text_success_pers <- item_text_success_pers[match(final_items_success_pers,
                                                       item_text_success_pers$item_name), ]


# bivariate correlations between items and predictor 
success_pers_predictor_items_cors <-round(cor(success_pers_dat,data$m_success_pers_ESM,use="complete.obs"),2)

#combine 
loadings_success_pers_obl_4 <- cbind(item_text_success_pers, loadings_success_pers_obl_4, success_pers_predictor_items_cors)
write_xlsx(loadings_success_pers_obl_4, here("EFA results", "success_pers_obl_4.xlsx"))

loadings_success_pers_pro_4 <- cbind(item_text_success_pers, loadings_success_pers_pro_4, success_pers_predictor_items_cors)
write_xlsx(loadings_success_pers_pro_4, here("EFA results", "success_pers_pro_4.xlsx"))

loadings_success_pers_obl_3 <- cbind(item_text_success_pers, loadings_success_pers_obl_3, success_pers_predictor_items_cors)
write_xlsx(loadings_success_pers_obl_3, here("EFA results", "success_pers_obl_3.xlsx"))

loadings_success_pers_pro_3 <- cbind(item_text_success_pers, loadings_success_pers_pro_3, success_pers_predictor_items_cors)
write_xlsx(loadings_success_pers_pro_3, here("EFA results", "success_pers_pro_3.xlsx"))

loadings_success_pers_obl_2 <- cbind(item_text_success_pers, loadings_success_pers_obl_2, success_pers_predictor_items_cors)
write_xlsx(loadings_success_pers_obl_2, here("EFA results", "success_pers_obl_2.xlsx"))

loadings_success_pers_pro_2 <- cbind(item_text_success_pers, loadings_success_pers_pro_2, success_pers_predictor_items_cors)
write_xlsx(loadings_success_pers_pro_2, here("EFA results", "success_pers_pro_2.xlsx"))


