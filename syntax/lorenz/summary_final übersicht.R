res <- read.csv("results_batch/summary_final.csv")
head(res)


table(res$fh_full_ok)
table(res$bhf_ok)

colSums(is.na(res[, c("fh_full_mean_mse","bhf_mean_mse")]))


#Alle 200 FH-full-Modelle haben erfolgreich gefittet

#191 von 200 BHF-Modellen erfolgreich

#9 BHF-Modelle sind fehlgeschlagen
#Die Warnungen, wärend dem batch, stammen genau von diesen 9 Fällen, nicht von den 191 erfolgreichen.

# Das BHF-Modell ist ein Unit-Level-Modell und reagiert empfindlich auf sehr kleine oder strukturell
# ungünstige Domains. In 9 von 200 Stichproben konnte das Modell nicht stabil geschätzt werden,
# weshalb diese Iterationen ausgeschlossen wurden. Die Area-Level FH-Modelle waren dagegen in allen Fällen stabil.



############################################################################
#ÜBERSICHT  summary_final.csv
############################################################################
library(tidyverse)

# 1) Laden ----------------------------------------------------
res <- read.csv("results_batch/summary_final.csv")

# Kurzcheck
res %>% summarise(
  n = n(),
  bhf_failed = sum(!bhf_ok),
  fh_na = sum(is.na(fh_full_mean_mse)),
  bhf_na = sum(is.na(bhf_mean_mse))
)

# 2) Daten in langes Format für Vergleichsplots ---------------
res_long <- res %>%
  transmute(
    sample_id,
    FH_full = fh_full_mean_mse,
    BHF = ifelse(bhf_ok, bhf_mean_mse, NA_real_)
  ) %>%
  pivot_longer(cols = c(FH_full, BHF), names_to = "model", values_to = "mean_mse")

# Optional: Ordner für Plots
dir.create("results_batch/plots", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# Plot A: Histogramm FH full mean MSE
# ============================================================
p1 <- ggplot(res, aes(x = fh_full_mean_mse)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Verteilung: mean MSE (FH full) über 200 Samples",
    x = "mean MSE (FH full)",
    y = "Anzahl Samples"
  )

ggsave("results_batch/plots/A_hist_FH_full_meanMSE.png", p1, width = 8, height = 5, dpi = 200)
p1


# ============================================================
# Plot B: Histogramm BHF mean MSE (nur erfolgreiche Runs)
# ============================================================
p2 <- ggplot(res %>% filter(bhf_ok), aes(x = bhf_mean_mse)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Verteilung: mean MSE (BHF) – nur erfolgreiche Runs",
    subtitle = paste0("Erfolgreich: ", sum(res$bhf_ok), " / ", nrow(res)),
    x = "mean MSE (BHF)",
    y = "Anzahl Samples"
  )

ggsave("results_batch/plots/B_hist_BHF_meanMSE.png", p2, width = 8, height = 5, dpi = 200)
p2


# ============================================================
# Plot C: Boxplot Vergleich FH vs BHF
# ============================================================
p3 <- ggplot(res_long, aes(x = model, y = mean_mse)) +
  geom_boxplot() +
  labs(
    title = "Vergleich der mean MSE-Verteilungen: FH full vs BHF",
    x = NULL,
    y = "mean MSE"
  )

ggsave("results_batch/plots/C_box_FH_vs_BHF.png", p3, width = 7, height = 5, dpi = 200)
p3


# ============================================================
# Plot D: Dichtekurven FH vs BHF (nur nicht-NA)
# ============================================================
p4 <- ggplot(res_long %>% filter(!is.na(mean_mse)), aes(x = mean_mse, fill = model)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Dichtevergleich: mean MSE (FH full vs BHF)",
    x = "mean MSE",
    y = "Dichte"
  )

ggsave("results_batch/plots/D_density_FH_vs_BHF.png", p4, width = 8, height = 5, dpi = 200)
p4


# ============================================================
# Plot E: Scatter FH vs BHF pro Sample (nur wo BHF ok)
# ============================================================
p5 <- ggplot(res %>% filter(bhf_ok), aes(x = fh_full_mean_mse, y = bhf_mean_mse)) +
  geom_point() +
  geom_abline() +
  labs(
    title = "FH full vs BHF: mean MSE pro Sample",
    subtitle = "Nur Samples mit erfolgreichem BHF",
    x = "mean MSE (FH full)",
    y = "mean MSE (BHF)"
  )

ggsave("results_batch/plots/E_scatter_FH_vs_BHF.png", p5, width = 7, height = 6, dpi = 200)
p5


# ============================================================
# Plot F: Welche Samples sind bei BHF fehlgeschlagen?
# ============================================================
p6 <- res %>%
  mutate(sample_num = readr::parse_number(sample_id)) %>%
  arrange(sample_num) %>%
  ggplot(aes(x = sample_num, y = as.integer(bhf_ok))) +
  geom_col() +
  scale_y_continuous(breaks = c(0, 1), labels = c("fail", "ok")) +
  labs(
    title = "BHF-Erfolg pro Sample (1=ok, 0=fail)",
    x = "Sample Nummer",
    y = "BHF Status"
  )

ggsave("results_batch/plots/F_bhf_success_by_sample.png", p6, width = 9, height = 4.5, dpi = 200)
p6

