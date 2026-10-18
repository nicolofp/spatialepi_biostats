library(data.table)

X1 <- fread("data/specialistica_per_erogatore_geo.csv")
X2 <- fread("data/specialistica_per_erogatore_geo_v1.csv")
Y <- fread("data/Ricoveri_per_Erogatore_20261014.csv")

# Pulmonary services: FARE_LIV_* are generic service types (no organ/diagnosis),
# so the pulmonary filter is the branch PNEUMOLOGIA (COD_BRANCA 022)
pulm        <- X1[COD_BRANCA == "022"]
pulm_visits <- pulm[COD_FARE_LIV_1 == "V"]   # V10 prima visita, V20 visita di controllo

# Pollution-sensitive outcome: first visits (new demand; follow-ups are chronic care
# and ~86% lack a priority class). Urgent = U (<=3 days) + B (<=10 days)
pulm_first <- pulm_visits[COD_FARE_LIV_3 == "V10"]
pulm_provider_year <- pulm_first[, .(
  N_tot = sum(QTA_TOT),
  U     = sum(QTA_TOT[COD_CLASSE_PRIORITA == "U"]),
  B     = sum(QTA_TOT[COD_CLASSE_PRIORITA == "B"]),
  D     = sum(QTA_TOT[COD_CLASSE_PRIORITA == "D"]),
  P     = sum(QTA_TOT[COD_CLASSE_PRIORITA == "P"])
), by = .(ANNO, COD_STRUTTURA_EROGAZIONE, DESC_STRUTTURA_EROGAZIONE,
          DESC_ATS_EROGAZIONE, PUBB_PRIV, lat, lon, geo_source)]
pulm_provider_year[, u_ratio := (U + B) / (N_tot)]

pulm_2025 <- pulm_provider_year[ANNO == 2025]

# Hospital admissions: respiratory DRGs plausibly linked to air pollution
# (excluded: neoplasms, trauma, pneumothorax, embolism, surgical DRGs)
drg_resp <- c(
  "088"                     = "COPD",
  "096" = "asthma_bronchitis", "097" = "asthma_bronchitis", "098" = "asthma_bronchitis",
  "089" = "pneumonia", "090" = "pneumonia", "091" = "pneumonia",
  "079" = "resp_infection", "080" = "resp_infection", "081" = "resp_infection",
  "068" = "upper_resp", "069" = "upper_resp", "070" = "upper_resp", "071" = "upper_resp",
  "087"                     = "resp_failure"
)
Y_resp <- Y[CODICE_DRG %in% names(drg_resp)]
Y_resp[, resp_group := drg_resp[CODICE_DRG]]
# RICOVERI_DO includes rehab and long-term stays: keep acute admissions only
acute <- function(d) d[, RICOVERI_DO - RICOVERI_RIABILITAZIONE - RICOVERI_LUNGODEGENZA + RICOVERI_DH]
Y_resp[, ricoveri := acute(Y_resp)]

Y_resp_2025 <- Y_resp[ANNO == 2025]

# Proportional ratio: respiratory / all acute admissions in the same hospital
Y_2025 <- Y[ANNO == 2025]
Y_2025[, ricoveri := acute(Y_2025)]
hosp_2025 <- merge(
  Y_2025[, .(tot_acute = sum(ricoveri)),
         by = .(CODICE_STRUTTURA, DESCRIZIONE_STRUTTURA, LATITUDINE_NORM, LONGITUDINE_NORM)],
  Y_resp_2025[, .(resp = sum(ricoveri)), by = CODICE_STRUTTURA],
  by = "CODICE_STRUTTURA", all.x = TRUE)
hosp_2025[is.na(resp), resp := 0]
hosp_2025 <- hosp_2025[tot_acute > 0]
hosp_2025[, resp_ratio := resp / tot_acute]

names(X2)
