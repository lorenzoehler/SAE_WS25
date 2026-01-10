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