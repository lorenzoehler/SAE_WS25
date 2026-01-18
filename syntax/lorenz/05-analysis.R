library(tidyverse)

dat <- read.csv("results_batch/summary_final.csv")

# Data in long format bringen
df_long <- pivot_longer(dat, cols = c(
  # fh_null_mean_mse,
  fh_full_mean_mse,
  bhf_mean_mse),
  names_to = "Estimator", values_to = "MSE")

# Boxplot
ggplot(df_long, aes(x = Estimator, y = MSE, fill = Estimator)) +
  geom_boxplot(width = 0.5, alpha = 0.7) +
  theme_minimal() +
  labs(title = "MSE Distribution by Estimator",
       y = "MSE",
       x = "")


ggplot(df_long, aes(x = MSE, fill = Estimator)) +
  geom_histogram(width = 0.5, alpha = 0.7) +
  theme_minimal() +
  labs(title = "MSE Distribution by Estimator",
       y = "MSE",
       x = "")


ggplot(df_long, aes(x = Estimator, y = MSE, fill = Estimator)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.1, fill="white") +  # Boxplot in der Mitte
  theme_minimal() +
  labs(title = "MSE Distribution by Estimator",
       y = "MSE")

ggplot(df_long, aes(x = MSE, color = Estimator, fill = Estimator)) +
  geom_density(alpha = 0.3) +
  # geom_histogram(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Density of MSE by Estimator",
       x = "MSE", y = "Density")


sample_001_BHF <- readRDS("~/Documents/GitHub/SAE_WS25/syntax/lorenz/results_batch/models_rds/sample_001_BHF.rds")
sample_001_FH_full <- readRDS("~/Documents/GitHub/SAE_WS25/syntax/lorenz/results_batch/models_rds/sample_001_FH_full.rds")
sample_002_BHF <- readRDS("~/Documents/GitHub/SAE_WS25/syntax/lorenz/results_batch/models_rds/sample_002_BHF.rds")
sample_003_BHF <- readRDS("~/Documents/GitHub/SAE_WS25/syntax/lorenz/results_batch/models_rds/sample_003_BHF.rds")

sample_001_FH_full$MSE

lst_files <- list.files("results_batch/models_rds",full.names = T)
lst_files <- grep(pattern = "full",x = lst_files,value = T)

dat_MSE <- data.frame("ID_prov" = sample_001_FH_full$ind$Domain)
dat_estimate <- data.frame("ID_prov" = sample_001_FH_full$ind$Domain)

for(i in seq_along(lst_files)){
  tmp_path <- lst_files[i]
  tmp_model <- readRDS(tmp_path)
  sample_number <- sub(".*sample_([0-9]+)_FH_full.*", "\\1", tmp_path)
  
  MSE_selection <- tmp_model$MSE[,c("Domain","Direct","FH")]
  colnames(MSE_selection) <- c("ID_prov", paste0("s",sample_number,"Dir"),paste0("s",sample_number,"FH"))
  
  estimate_selection <- tmp_model$ind[,c("Domain","Direct","FH")]
  colnames(estimate_selection) <- c("ID_prov", paste0("s",sample_number,"Dir"),paste0("s",sample_number,"FH"))
  
  dat_MSE <- merge(dat_MSE,MSE_selection,by = "ID_prov")
  dat_estimate <- merge(dat_estimate,estimate_selection,by = "ID_prov")
}

lst_files <- list.files("results_batch/models_rds",full.names = T)
lst_files <- grep(pattern = "BHF",x = lst_files,value = T)


# dat_MSE <- data.frame("ID_prov" = sample_001_FH_full$ind$Domain)

for(i in seq_along(lst_files)){
  tmp_path <- lst_files[i]
  tmp_model <- readRDS(tmp_path)
  sample_number <- sub(".*sample_([0-9]+)_BHF.*", "\\1", tmp_path)
  MSE_selection <- tmp_model$MSE[,c("Domain","Mean")]
  colnames(MSE_selection) <- c("ID_prov", paste0("s",sample_number,"BHF"))
  dat_MSE <- merge(dat_MSE,MSE_selection,by = "ID_prov")
}


#####  create long df for MSEs
lst_colnames_MSEs <- colnames(dat_MSE[,-1])

df_MSE_long <- pivot_longer(dat_MSE, cols = lst_colnames_MSEs,
  names_to = "Estimator", values_to = "MSE")

# df_long$Estimator

df_MSE_long <- df_MSE_long %>%
  mutate(
    sample = str_extract(Estimator, "(?<=^s)\\d+"),
    method = str_extract(Estimator, "(Dir|FH|BHF)$")
  )

saveRDS(df_MSE_long,"../../data_raw/simulation/processed/df_MSE_long.RDS")

###### creat long df for estimates
lst_colnames_estimates <- colnames(dat_estimate[,-1])
df_estimates_long <- pivot_longer(dat_estimate, cols = lst_colnames_estimates,
                            names_to = "Estimator", values_to = "estimate")

# df_long$Estimator

df_estimates_long <- df_estimates_long %>%
  mutate(
    sample = str_extract(Estimator, "(?<=^s)\\d+"),
    method = str_extract(Estimator, "(Dir|FH|BHF)$")
  )

saveRDS(df_estimates_long,"../../data_raw/simulation/processed/df_estimates_long.RDS")



# Boxplot
df_long %>% filter(sample == "001") %>% 
ggplot(., aes(x = Estimator, y = MSE, fill = Estimator)) +
  geom_boxplot(width = 0.5, alpha = 0.7) +
  theme_minimal() +
  labs(title = "MSE Distribution by Estimator",
       y = "MSE",
       x = "")

df_long %>% 
  # filter(method == "FH") %>% 
ggplot(., aes(x = MSE, fill = method)) +
  geom_histogram(width = 0.1, alpha = 0.7,binwidth = .02) +
  theme_minimal() +
  labs(title = "MSE Distribution by Estimator",
       y = "MSE",
       x = "")



