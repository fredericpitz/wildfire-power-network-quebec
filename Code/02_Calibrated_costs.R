### ======================================================
### 02_Calibrated_costs.R
### PART A — Raw economic replacement costs of assets
### PART B — Historical analysis 1981–2010 (real fires vs Boulanger Baseline)
### PART C — Delta calibration (correction of Boulanger bias)
###
### PART A :
###   Expected cost = esperance_perte × unit_cost(type)
###   where esperance_perte = P_ajustee × nb_actifs (from 01_Prob_ff.R)
###
###   Inputs  :
###     Sortie/01_prob_ff/redistribution/resultats_feux_actifs_{MODELE}_{INVENTAIRE}.csv
###
###   Outputs (per model × inventory combination) :
###     Sortie/02_calibrated_costs/couts/   (wd_sortie05)
###       couts_detail_boulanger_{MODELE}_{INVENTAIRE}.csv
###       couts_quebec_annee_boulanger_{MODELE}_{INVENTAIRE}.csv
###       couts_par_horizon_boulanger_{MODELE}_{INVENTAIRE}.csv
### ======================================================

if (!exists("wd")) source(here::here("Code", "00_Main.R"))

TEST_MODE <- TRUE  # Set to FALSE for full run

library(dplyr)
library(readr)
library(tidyr)
library(tibble)

t_debut <- Sys.time()
cat(sprintf("[%s] ══ Starting economic cost calculation ══\n",
            format(t_debut, "%H:%M:%S")))


## ══════════════════════════════════════════════════════
## PARAMETERS — modify here to choose what to process
## ══════════════════════════════════════════════════════

## Available models: "CANESM2", "HadGEM2-ES", "MIROC-ESM-CHEM"
MODELES_A_TRAITER     <- c("CANESM2", "HadGEM2-ES", "MIROC-ESM-CHEM")

## Available inventories: "stable", "BAU_couple", "EE_couple"
## "BAU_couple" = Baseline→stable, RCP45→BAU_SSP2, RCP85→BAU_SSP3
## "EE_couple"  = Baseline→stable, RCP45→EE_SSP2,  RCP85→EE_SSP3
## Note: this script always re-reads and recalculates (no skip).
## To recalculate only BAU/EE after a fix to 01_Prob_ff.R, set:
##   INVENTAIRES_A_TRAITER <- c("BAU_couple", "EE_couple")
INVENTAIRES_A_TRAITER <- c("stable", "BAU_couple", "EE_couple")

if (TEST_MODE) {
  cat("RUNNING IN TEST MODE - limited data only\n")
  MODELES_A_TRAITER     <- MODELES_A_TRAITER[1]
  INVENTAIRES_A_TRAITER <- "stable"
}


## ══════════════════════════════════════════════════════
## UNIT REPLACEMENT COSTS (CAD $)
## ══════════════════════════════════════════════════════

couts_unitaires <- c(
  `Poteaux de bois (h-frame)`              =  36684,
  `Poteaux de bois distribution-transport` = 6598,
  `Poteaux bois distribution (réel)`       = 6598,
  `Postes de transformation (nombre)`      = 7368084,
  `Transfo 10 kVA`                         = 2490,
  `Transfo 25 kVA`                         = 3390,
  `Transfo 50 kVA`                         = 5970,
  `Transfo 75 kVA`                         = 10800,
  `Transfo 100 kVA`                        = 11400,
  `Transfo 167 kVA`                        = 11400,
  `Transfo 150-300 kVA`                    = 18500,
  `Transfo 500+ kVA`                       = 31000,
  `Lignes distribution MT`                 = 178.711  ## 178 711 $/km → converted to $/m (nb_actifs is in metres)
)

table_couts <- tibble::tibble(
  type_actif    = names(couts_unitaires),
  cout_unitaire = as.numeric(couts_unitaires)
)

horizon_de_annee <- function(yr) {
  dplyr::case_when(
    yr >= 2011 & yr <= 2040 ~ "2011-2040",
    yr >= 2041 & yr <= 2070 ~ "2041-2070",
    yr >= 2071 & yr <= 2100 ~ "2071-2100",
    TRUE                    ~ NA_character_
  )
}


## ══════════════════════════════════════════════════════
## LOOP OVER MODELS × INVENTORIES
## ══════════════════════════════════════════════════════

combos <- expand.grid(
  modele     = MODELES_A_TRAITER,
  inventaire = INVENTAIRES_A_TRAITER,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(combos))) {
  
  modele     <- combos$modele[i]
  inventaire <- combos$inventaire[i]
  
  cat(sprintf("\n[%s] ── Model: %s | Inventory: %s ──\n",
              format(Sys.time(), "%H:%M:%S"), modele, inventaire))

  ## ── Loading ─────────────────────────────────────
  chemin_in <- file.path(wd_sortie04,
                         sprintf("resultats_feux_actifs_%s_%s.csv", modele, inventaire))
  
  if (!file.exists(chemin_in)) {
    warning("File not found, combination skipped: ", basename(chemin_in),
            "\n  → Source Code/01_Prob_ff.R first")
    next
  }
  
  resultats <- read_csv(chemin_in, show_col_types = FALSE)
  
  types_sans_cout <- setdiff(unique(resultats$type_actif), names(couts_unitaires))
  if (length(types_sans_cout) > 0)
    warning("Asset types without unit cost (excluded): ",
            paste(types_sans_cout, collapse = ", "))
  
  ## ── Expected cost calculation ────────────────────────
  base <- resultats |>
    dplyr::inner_join(table_couts, by = "type_actif") |>
    dplyr::mutate(
      cout_espere = esperance_perte * cout_unitaire,
      horizon     = horizon_de_annee(YR)
    )
  
  ## Internal rename: "h-frame" = wood poles on transmission network (HQ_TYPE POTEAU)
  base <- base |>
    dplyr::mutate(type_actif = dplyr::recode(type_actif,
                                             "Poteaux de bois (h-frame)" = "Poteaux de bois de transport"))
  
  ## ── Output 1: full detail ───────────────────────
  couts_detail <- base |>
    dplyr::select(SCEN, CELL_ID, YR, horizon, score_raster,
                  type_actif, cout_unitaire,
                  nb_actifs, P_ajustee, esperance_perte, cout_espere) |>
    dplyr::arrange(SCEN, CELL_ID, YR, score_raster, type_actif)
  
  chemin_detail <- file.path(wd_sortie05,
                             sprintf("couts_detail_boulanger_%s_%s.csv", modele, inventaire))
  write_csv(couts_detail, chemin_detail)
  cat(sprintf("[%s]   → %s (%d lignes)\n",
              format(Sys.time(), "%H:%M:%S"),
              basename(chemin_detail), nrow(couts_detail)))
  
  ## ── Output 2: Quebec total by scenario × year ───
  couts_qc_annee <- base |>
    dplyr::group_by(SCEN, YR, horizon) |>
    dplyr::summarise(cout_total_qc = sum(cout_espere, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(SCEN, YR)
  
  chemin_qc <- file.path(wd_sortie05,
                         sprintf("couts_quebec_annee_boulanger_%s_%s.csv", modele, inventaire))
  write_csv(couts_qc_annee, chemin_qc)
  cat(sprintf("[%s]   → %s (%d lignes)\n",
              format(Sys.time(), "%H:%M:%S"),
              basename(chemin_qc), nrow(couts_qc_annee)))
  
  ## ── Output 3: by horizon × cell × asset type ─
  couts_horizon <- base |>
    dplyr::filter(!is.na(horizon)) |>
    dplyr::group_by(SCEN, horizon, CELL_ID, type_actif) |>
    dplyr::summarise(
      n_annees          = dplyr::n_distinct(YR),
      cout_moyen_annuel = sum(cout_espere, na.rm = TRUE) / dplyr::n_distinct(YR),
      cout_total        = sum(cout_espere, na.rm = TRUE),
      .groups           = "drop"
    ) |>
    dplyr::arrange(SCEN, horizon, CELL_ID, type_actif)
  
  chemin_horizon <- file.path(wd_sortie05,
                              sprintf("couts_par_horizon_boulanger_%s_%s.csv", modele, inventaire))
  write_csv(couts_horizon, chemin_horizon)
  cat(sprintf("[%s]   → %s (%d lignes)\n",
              format(Sys.time(), "%H:%M:%S"),
              basename(chemin_horizon), nrow(couts_horizon)))
  
  ## ── Console summary ─────────────────────────────────
  cat(sprintf("\n── Annual cost QC | %s × %s ──\n", modele, inventaire))
  couts_qc_annee |>
    dplyr::group_by(SCEN) |>
    dplyr::summarise(
      Years  = paste(min(YR), max(YR), sep = "–"),
      Moy    = sprintf("%.3f M$", mean(cout_total_qc) / 1e6),
      Min    = sprintf("%.3f M$", min(cout_total_qc)  / 1e6),
      Max    = sprintf("%.3f M$", max(cout_total_qc)  / 1e6),
      .groups = "drop"
    ) |>
    print()
}

duree <- round(as.numeric(difftime(Sys.time(), t_debut, units = "secs")))
cat(sprintf("\n[%s] ══ Done — elapsed: %ds ══\n",
            format(Sys.time(), "%H:%M:%S"), duree))






### ======================================================
### PART B — Historical analysis (1981–2010) of fire risk
###
### Section A: Probabilistic approach — Boulanger Baseline
###   P_feu = 1/FireCycle × MRNF redistribution
###   → constant annual expected cost (uniform over 1981–2010)
###
### Section B: Real fires — FEUX_PROV.gpkg
###   Spatial intersection of assets × real fire polygons
###   → actual replacement cost by (year, type_actif)
###
### Section C: Comparison A vs B
###   Tables + charts
###
### Prerequisites (must have run beforehand):
###   01_Prob_ff.R → Sortie/01_prob_ff/zones_propagation.gpkg         (wd_sortie01)
###                  Sortie/01_prob_ff/probabilites/                   (wd_sortie03)
###                  Sortie/01_prob_ff/redistribution/                 (wd_sortie04)
###
### Outputs in Sortie/02_calibrated_costs/historique/   (wd_sortie08) :
###   section_A_boulanger_baseline.csv
###   section_B_feux_reels_par_annee.csv
###   section_B_resume.csv
###   section_C_comparaison.csv
###   graphique_evolution_annuelle.png
###   graphique_par_type_actif.png
###   graphique_calibration.png
### ======================================================

if (!exists("wd")) source(here::here("Code", "00_Main.R"))

library(terra)
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(arrow)
library(ggplot2)
library(scales)

sf_use_s2(FALSE)
terra::tmpFiles(orphan = TRUE, remove = TRUE)
terraOptions(memfrac = 0.5)

t_debut <- Sys.time()
cat(sprintf("[%s] ══ Starting historical analysis 1981–2010 ══\n",
            format(t_debut, "%H:%M:%S")))


## ══════════════════════════════════════════════════════
## PARAMETERS AND PATHS
## ══════════════════════════════════════════════════════

ANNEES_HIST       <- 1981:2010
N_ANNEES          <- length(ANNEES_HIST)   # 30
TAUX_BOIS_ORL_SEI <- 0.7978

## Input data
CHEMIN_BASELINE    <- file.path(wd_data, "Boulanger_RCP",
                                "FireCycle_Baseline_WGS1984_0.25.tif")
CHEMIN_RASTER_MRNF <- file.path(wd_data, "Potentiel_Intensite_Propagation_Feux",
                                "Couche globale", "Potentiel_IP_25-26.tif")
CHEMIN_ZONES       <- file.path(wd_sortie01, "zones_propagation.gpkg")
CHEMIN_FEUX        <- file.path(wd_data, "FEUX_HISTORIQUES", "FEUX_PROV.gpkg")

## Stable assets (shapefiles)
CHEMIN_SUPPORTS    <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_LAV <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_MAT <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_NOR <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_IDM <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_ORL <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_SEI <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POSTES      <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_TRANSFOS    <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_LIGNES_STAB <- file.path(wd_inventaires,
                                "INSERT ASSET DATA HERE.parquet")

## Checkpoints from 01_Prob_ff.R (avoids re-extracting from rasters)
CHEMIN_FACTEURS    <- file.path(wd_sortie04, "facteurs_redistribution_boulanger.csv")
CHEMIN_CELLULES    <- file.path(wd_sortie03, "cellules_reference_boulanger.csv")
CHEMIN_ACTIFS_04   <- file.path(wd_sortie04, "resultats_feux_actifs_CANESM2_stable.csv")

## Preliminary checks for critical files
fichiers_requis <- c(CHEMIN_BASELINE, CHEMIN_RASTER_MRNF, CHEMIN_ZONES,
                     CHEMIN_FEUX, CHEMIN_FACTEURS, CHEMIN_CELLULES, CHEMIN_ACTIFS_04)
manquants <- fichiers_requis[!file.exists(fichiers_requis)]
if (length(manquants) > 0)
  stop("Files not found:\n  ", paste(manquants, collapse = "\n  "),
       "\n  → Source Code/01_Prob_ff.R first")


## ══════════════════════════════════════════════════════
## UNIT COSTS (identical to Part A)
## ══════════════════════════════════════════════════════

couts_unitaires <- c(
  `Poteaux de bois de transport`           =  36684,
  `Poteaux de bois distribution-transport` =   6598,
  `Poteaux bois distribution (réel)`       =   6598,
  `Postes de transformation (nombre)`      = 7368084,
  `Transfo 10 kVA`                         =   2490,
  `Transfo 25 kVA`                         =   3390,
  `Transfo 50 kVA`                         =   5970,
  `Transfo 75 kVA`                         =  10800,
  `Transfo 100 kVA`                        =  11400,
  `Transfo 167 kVA`                        =  11400,
  `Transfo 150-300 kVA`                    =  18500,
  `Transfo 500+ kVA`                       =  31000,
  `Lignes distribution MT`                 =  178.711  ## $/m (nb_brules is in metres)
)

table_couts <- tibble::tibble(
  type_actif    = names(couts_unitaires),
  cout_unitaire = as.numeric(couts_unitaires)
)

classifier_kva <- function(x) {
  x_num <- suppressWarnings(as.numeric(sub("T$", "", trimws(as.character(x)))))
  dplyr::case_when(
    x_num == 10                      ~ "Transfo 10 kVA",
    x_num == 25                      ~ "Transfo 25 kVA",
    x_num %in% c(40, 50)            ~ "Transfo 50 kVA",
    x_num %in% c(65, 75)            ~ "Transfo 75 kVA",
    x_num == 100                     ~ "Transfo 100 kVA",
    x_num == 167                     ~ "Transfo 167 kVA",
    x_num %in% c(150, 200, 300)      ~ "Transfo 150-300 kVA",
    x_num %in% c(500, 501, 600, 900) ~ "Transfo 500+ kVA",
    TRUE                             ~ NA_character_
  )
}


## ══════════════════════════════════════════════════════
## SECTION A: PROBABILISTIC BOULANGER BASELINE
## ══════════════════════════════════════════════════════

cat(sprintf("\n[%s] ══ SECTION A: Boulanger Baseline probabilistic ══\n",
            format(Sys.time(), "%H:%M:%S")))

## ── A1. Spatial mask + P_feu from the Baseline raster ─────────────

cat(sprintf("[%s] Loading spatial mask and Baseline raster...\n",
            format(Sys.time(), "%H:%M:%S")))

masque_vect <- sf::st_read(CHEMIN_ZONES, quiet = TRUE) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_transform(4326) |>
  terra::vect()

r_baseline  <- terra::rast(CHEMIN_BASELINE)
r_bl_masked <- terra::mask(terra::crop(r_baseline, masque_vect), masque_vect)
rm(r_baseline); gc()

cellules_ref <- readr::read_csv(CHEMIN_CELLULES, show_col_types = FALSE)

## Extract fire cycle at each reference cell
vpts_ref      <- terra::vect(cbind(cellules_ref$lon, cellules_ref$lat),
                             crs = "EPSG:4326")
fire_cycle    <- terra::extract(r_bl_masked, vpts_ref)[[2]]
rm(r_bl_masked, vpts_ref); gc()

p_feu_cell <- cellules_ref |>
  dplyr::mutate(
    P_feu = dplyr::if_else(!is.na(fire_cycle) & fire_cycle > 0,
                           1 / fire_cycle, 0)
  ) |>
  dplyr::select(CELL_ID, P_feu)

cat(sprintf("[%s] %d cells — P_feu: mean=%.4f  max=%.4f\n",
            format(Sys.time(), "%H:%M:%S"),
            nrow(p_feu_cell), mean(p_feu_cell$P_feu), max(p_feu_cell$P_feu)))


## ── A2. MRNF redistribution factors (checkpoint 04) ──────────────

cat(sprintf("[%s] Loading MRNF redistribution factors...\n",
            format(Sys.time(), "%H:%M:%S")))

facteurs <- readr::read_csv(CHEMIN_FACTEURS, show_col_types = FALSE) |>
  dplyr::select(CELL_ID, score_raster, facteur)


## ── A3. Aggregated assets (retrieved from 01_Prob_ff.R output) ──────────
##
##   nb_actifs is invariant across models/scenarios for a stable inventory
##   → read CANESM2_stable and take distinct combinations.
##   This avoids re-extracting from rasters (heavy operation).

cat(sprintf("[%s] Loading aggregated assets from 01_Prob_ff.R...\n",
            format(Sys.time(), "%H:%M:%S")))

actifs_agregat <- readr::read_csv(CHEMIN_ACTIFS_04, show_col_types = FALSE) |>
  dplyr::mutate(
    type_actif = dplyr::recode(type_actif,
                               "Poteaux de bois (h-frame)" = "Poteaux de bois de transport")
  ) |>
  dplyr::distinct(CELL_ID, score_raster, type_actif, nb_actifs)

cat(sprintf("[%s] %d combinations (CELL_ID × score × type)\n",
            format(Sys.time(), "%H:%M:%S"), nrow(actifs_agregat)))


## ── A4. Compute adjusted P and expected cost ─────────────────────────────
##
##   score >= 0 → MRNF coverage → P_ajustee = P_feu × facteur(score, cell)
##   score == -1 → outside coverage → P_ajustee = P_feu (no redistribution)

cat(sprintf("[%s] Computing adjusted P and expected costs...\n",
            format(Sys.time(), "%H:%M:%S")))

section_A_detail <- actifs_agregat |>
  dplyr::left_join(p_feu_cell, by = "CELL_ID") |>
  dplyr::left_join(facteurs,   by = c("CELL_ID", "score_raster")) |>
  dplyr::mutate(
    P_ajustee = dplyr::case_when(
      score_raster >= 0 & !is.na(facteur) ~ P_feu * facteur,
      TRUE                                 ~ dplyr::coalesce(P_feu, 0)
    ),
    esperance_perte = nb_actifs * P_ajustee
  ) |>
  dplyr::inner_join(table_couts, by = "type_actif") |>
  dplyr::mutate(cout_espere = esperance_perte * cout_unitaire)


## ── A5. Aggregation by type_actif ────────────────────────────────────

section_A_par_type <- section_A_detail |>
  dplyr::group_by(type_actif, cout_unitaire) |>
  dplyr::summarise(
    nb_actifs_total      = sum(nb_actifs,       na.rm = TRUE),
    esperance_perte_tot  = sum(esperance_perte,  na.rm = TRUE),
    cout_espere_annuel   = sum(cout_espere,      na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(cout_espere_30ans = cout_espere_annuel * N_ANNEES) |>
  dplyr::arrange(dplyr::desc(cout_espere_annuel))

cout_A_total_annuel <- sum(section_A_par_type$cout_espere_annuel)

section_A_total <- tibble::tibble(
  type_actif           = "TOTAL QUÉBEC",
  cout_unitaire        = NA_real_,
  nb_actifs_total      = sum(section_A_par_type$nb_actifs_total),
  esperance_perte_tot  = sum(section_A_par_type$esperance_perte_tot),
  cout_espere_annuel   = cout_A_total_annuel,
  cout_espere_30ans    = cout_A_total_annuel * N_ANNEES
)

section_A <- dplyr::bind_rows(section_A_par_type, section_A_total)
readr::write_csv(section_A,
                 file.path(wd_sortie08, "section_A_boulanger_baseline.csv"))

cat(sprintf("\n── Section A Summary ──\n"))
cat(sprintf("  Expected annual cost (Baseline): %.3f M$\n",
            cout_A_total_annuel / 1e6))
cat(sprintf("  Expected total cost 30 years   : %.3f M$\n",
            cout_A_total_annuel * N_ANNEES / 1e6))

rm(actifs_agregat, section_A_detail, facteurs, p_feu_cell); gc()


## ══════════════════════════════════════════════════════
## SECTION B: REAL FIRES 1981–2010
## ══════════════════════════════════════════════════════

cat(sprintf("\n[%s] ══ SECTION B: Real fires 1981–2010 ══\n",
            format(Sys.time(), "%H:%M:%S")))

## ── B1. Load historical fires ──────────────────────────────────

cat(sprintf("[%s] Loading FEUX_PROV.gpkg...\n",
            format(Sys.time(), "%H:%M:%S")))

feux_hist <- sf::st_read(CHEMIN_FEUX, layer = "feux_prov", quiet = TRUE) |>
  sf::st_make_valid() |>
  dplyr::filter(!is.na(an_origine),
                as.integer(an_origine) %in% ANNEES_HIST) |>
  dplyr::mutate(annee = as.integer(an_origine))

CRS_FEUX <- sf::st_crs(feux_hist)  ## 32198 (Quebec Lambert)

cat(sprintf("[%s] %d fire polygons (1981–2010), CRS: %s\n",
            format(Sys.time(), "%H:%M:%S"),
            nrow(feux_hist), CRS_FEUX$input))


## ── B2. Prepare point assets (sf in 32198) ───────────────────

cat(sprintf("[%s] Loading point assets...\n",
            format(Sys.time(), "%H:%M:%S")))

## Reads a shapefile, applies optional filter, returns sf with type_actif + weight
lire_pts <- function(chemin, type_actif, filtre_fn = NULL, poids = 1.0) {
  if (!file.exists(chemin)) {
    warning("Shapefile not found: ", chemin)
    return(NULL)
  }
  pts <- sf::st_read(chemin, quiet = TRUE) |>
    sf::st_transform(CRS_FEUX)
  if (!is.null(filtre_fn)) pts <- filtre_fn(pts)
  if (nrow(pts) == 0) return(NULL)
  sf::st_sf(
    type_actif = type_actif,
    poids      = poids,
    geometry   = sf::st_geometry(pts)
  )
}

pts_ht  <- lire_pts(CHEMIN_SUPPORTS, "Poteaux de bois de transport",
                    filtre_fn = function(x) dplyr::filter(x, HQ_TYPE == "POTEAU"))
pts_dt  <- lire_pts(CHEMIN_SUPPORTS, "Poteaux de bois distribution-transport",
                    filtre_fn = function(x) dplyr::filter(x, HQ_TYPE == "POTEAU_DIS"))
pts_lav <- lire_pts(CHEMIN_POTEAUX_LAV, "Poteaux bois distribution (réel)",
                    filtre_fn = function(x) dplyr::filter(x, materiel == "Bois"))
pts_mat <- lire_pts(CHEMIN_POTEAUX_MAT, "Poteaux bois distribution (réel)",
                    filtre_fn = function(x) dplyr::filter(x, materiel == "Bois"))
pts_nor <- lire_pts(CHEMIN_POTEAUX_NOR, "Poteaux bois distribution (réel)",
                    filtre_fn = function(x) dplyr::filter(x, materiel == "Bois"))
pts_idm <- lire_pts(CHEMIN_POTEAUX_IDM, "Poteaux bois distribution (réel)",
                    filtre_fn = function(x) dplyr::filter(x, materiel == "Bois"))
## ORL and SEI: no material filter → weight = estimated wood fraction
pts_orl <- lire_pts(CHEMIN_POTEAUX_ORL, "Poteaux bois distribution (réel)",
                    poids = TAUX_BOIS_ORL_SEI)
pts_sei <- lire_pts(CHEMIN_POTEAUX_SEI, "Poteaux bois distribution (réel)",
                    poids = TAUX_BOIS_ORL_SEI)
pts_pos <- lire_pts(CHEMIN_POSTES, "Postes de transformation (nombre)")

## Transformers (classification by kVA)
pts_tr <- NULL
if (file.exists(CHEMIN_TRANSFOS)) {
  tr_raw <- sf::st_read(CHEMIN_TRANSFOS, quiet = TRUE) |>
    dplyr::filter(classe_app == "Transformateur") |>
    dplyr::mutate(type_actif = classifier_kva(valeur_nom)) |>
    dplyr::filter(!is.na(type_actif)) |>
    sf::st_transform(CRS_FEUX)
  pts_tr <- sf::st_sf(
    type_actif = tr_raw$type_actif,
    poids      = 1.0,
    geometry   = sf::st_geometry(tr_raw)
  )
  rm(tr_raw)
} else {
  warning("Transformer shapefile not found: ", CHEMIN_TRANSFOS)
}

actifs_pts <- dplyr::bind_rows(
  pts_ht, pts_dt,
  pts_lav, pts_mat, pts_nor, pts_idm,
  pts_orl, pts_sei,
  pts_pos, pts_tr
)

if (TEST_MODE) {
  actifs_pts     <- dplyr::filter(actifs_pts, type_actif == "Postes de transformation (nombre)")
  pts_lignes_25m <- NULL
}

cat(sprintf("[%s] %d point assets loaded\n",
            format(Sys.time(), "%H:%M:%S"), nrow(actifs_pts)))

rm(pts_ht, pts_dt, pts_lav, pts_mat, pts_nor, pts_idm,
   pts_orl, pts_sei, pts_pos, pts_tr); gc()


## ── B3. Prepare MT lines (vectorised — 25m points in 32198) ──────
##
##   Same method as Section A / 01_Prob_ff.R (Part B), but VECTORISED:
##   - st_as_sfc() on the entire WKT vector in one pass (no purrr::map)
##   - st_segmentize() on the whole sfc at once
##   - st_coordinates() extracts all points into a matrix
##   - st_as_sf() builds the final sf from the coordinate matrix
##   → much faster than the row-by-row version

cat(sprintf("[%s] Loading MT lines — 25m interpolation (vectorised)...\n",
            format(Sys.time(), "%H:%M:%S")))

pts_lignes_25m <- NULL

if (file.exists(CHEMIN_LIGNES_STAB)) {
  
  raw_lig <- arrow::open_dataset(CHEMIN_LIGNES_STAB) |>
    dplyr::filter(reseau == "moyenne tension", year == 2022L) |>
    dplyr::select(exemple_segment, longueur) |>
    dplyr::collect() |>
    dplyr::filter(!is.na(exemple_segment))
  
  cat(sprintf("[%s] %d MT segments — vectorised parsing...\n",
              format(Sys.time(), "%H:%M:%S"), nrow(raw_lig)))
  
  ## 1. Parse all WKTs in a single vectorised operation
  geoms_wgs <- tryCatch(
    sf::st_as_sfc(raw_lig$exemple_segment, crs = 4326),
    error = function(e) {
      warning("Batch parsing failed — individual fallback")
      purrr::map(raw_lig$exemple_segment, function(wkt)
        tryCatch(sf::st_as_sfc(wkt, crs = 4326)[[1]],
                 error = function(e) sf::st_geometrycollection())
      ) |> sf::st_sfc(crs = 4326)
    }
  )
  geoms_m   <- sf::st_transform(geoms_wgs, CRS_FEUX)
  long_segs <- as.numeric(sf::st_length(geoms_m))
  valide    <- !sf::st_is_empty(geoms_m) & long_segs > 0
  rm(geoms_wgs); gc()
  
  geoms_valid    <- geoms_m[valide]
  longueur_valid <- as.numeric(raw_lig$longueur)[valide]
  rm(geoms_m, raw_lig); gc()
  
  cat(sprintf("[%s] %d valid segments — 25m segmentation...\n",
              format(Sys.time(), "%H:%M:%S"), sum(valide)))
  
  ## 2. Segmentise the whole sfc in one pass then extract coordinates
  geoms_seg  <- sf::st_segmentize(geoms_valid, dfMaxLength = 25)
  rm(geoms_valid); gc()
  
  coords_mat <- sf::st_coordinates(geoms_seg)
  rm(geoms_seg); gc()
  
  ## Last column = index of parent geometry (L1 for LINESTRING,
  ## L2/L3 for MULTI) — always valid regardless of geometry type
  geom_idx      <- as.integer(coords_mat[, ncol(coords_mat)])
  n_pts_per_seg <- tabulate(geom_idx, nbins = sum(valide))
  poids_per_pt  <- longueur_valid[geom_idx] / pmax(n_pts_per_seg[geom_idx], 1L)
  
  ## 3. Build the final sf from the coordinate matrix (vectorised, fast)
  pts_lignes_25m <- sf::st_as_sf(
    data.frame(
      poids = poids_per_pt,
      X     = coords_mat[, "X"],
      Y     = coords_mat[, "Y"]
    ),
    coords = c("X", "Y"),
    crs    = CRS_FEUX
  )
  rm(coords_mat, geom_idx, n_pts_per_seg, poids_per_pt); gc()
  
  cat(sprintf("[%s] %d 25m points generated\n",
              format(Sys.time(), "%H:%M:%S"), nrow(pts_lignes_25m)))
  
} else {
  warning("MT lines parquet not found: ", CHEMIN_LIGNES_STAB,
          "\n  → MT lines excluded from Section B.")
}


## ── Pre-filter: restrict to assets within the extent of 1981–2010 fires ──
##
##   The vast majority of assets (southern Quebec) will never fall inside a
##   historical fire polygon. Excluding them ONCE here avoids testing them
##   needlessly at each loop iteration → ×10–×50 speedup.

cat(sprintf("[%s] Pre-filtering assets within historical fire extent...\n",
            format(Sys.time(), "%H:%M:%S")))

zone_feux_hist <- sf::st_union(feux_hist)  ## computed once, outside the loop

idx_pts_zone    <- lengths(sf::st_intersects(actifs_pts, zone_feux_hist)) > 0
actifs_pts_zone <- actifs_pts[idx_pts_zone, ]
cat(sprintf("[%s]   Point assets: %d / %d within zone\n",
            format(Sys.time(), "%H:%M:%S"),
            nrow(actifs_pts_zone), nrow(actifs_pts)))
rm(actifs_pts); gc()

pts_lig_zone <- NULL
if (!is.null(pts_lignes_25m) && nrow(pts_lignes_25m) > 0) {
  idx_lig_zone <- lengths(sf::st_intersects(pts_lignes_25m, zone_feux_hist)) > 0
  pts_lig_zone <- pts_lignes_25m[idx_lig_zone, ]
  cat(sprintf("[%s]   MT line points: %d / %d within zone\n",
              format(Sys.time(), "%H:%M:%S"),
              nrow(pts_lig_zone), nrow(pts_lignes_25m)))
  rm(pts_lignes_25m)
}
rm(zone_feux_hist); gc()


## ── B4. Per-year loop: intersection of assets × fires ────────────────
##
##   Point assets: st_intersects → asset burned if inside ≥ 1 polygon
##   MT lines    : st_intersects on 25m points → sum(weight) = metres burned
##                 (same method as Section A / 01_Prob_ff.R Part B)

cat(sprintf("[%s] Starting intersection loop (%d years)...\n",
            format(Sys.time(), "%H:%M:%S"), N_ANNEES))

resultats_B_liste <- purrr::map(ANNEES_HIST, function(yr) {
  
  cat(sprintf("[%s] Year %d...\r", format(Sys.time(), "%H:%M:%S"), yr))
  flush.console()
  
  feux_yr <- dplyr::filter(feux_hist, annee == yr)
  if (nrow(feux_yr) == 0) return(NULL)
  
  res_yr <- list()
  
  ## ── Point assets (pre-filtered subset) ──────────────────────
  idx_pts <- sf::st_intersects(actifs_pts_zone, feux_yr, sparse = TRUE)
  brule   <- lengths(idx_pts) > 0
  
  if (any(brule)) {
    res_yr[["ponctuels"]] <- actifs_pts_zone[brule, ] |>
      sf::st_drop_geometry() |>
      dplyr::group_by(type_actif) |>
      dplyr::summarise(nb_brules = sum(poids), .groups = "drop") |>
      dplyr::mutate(annee = yr)
  }
  
  ## ── MT lines — 25m points (pre-filtered subset) ────────────────
  if (!is.null(pts_lig_zone) && nrow(pts_lig_zone) > 0) {
    
    idx_lig   <- sf::st_intersects(pts_lig_zone, feux_yr, sparse = TRUE)
    brule_lig <- lengths(idx_lig) > 0
    
    total_m_brules <- sum(pts_lig_zone$poids[brule_lig], na.rm = TRUE)
    
    if (total_m_brules > 0) {
      res_yr[["lignes"]] <- tibble::tibble(
        type_actif = "Lignes distribution MT",
        nb_brules  = total_m_brules,
        annee      = yr
      )
    }
  }
  
  dplyr::bind_rows(res_yr)
})

cat(sprintf("\n[%s] Loop complete\n", format(Sys.time(), "%H:%M:%S")))


## ── B5. Aggregation and save ─────────────────────────────────────

section_B_annee <- dplyr::bind_rows(resultats_B_liste) |>
  dplyr::left_join(table_couts, by = "type_actif") |>
  dplyr::mutate(cout_reel = nb_brules * cout_unitaire) |>
  dplyr::arrange(annee, type_actif)

## 30-year summary — annual average over 30 years = total / 30 (includes zero-fire years)
section_B_resume <- section_B_annee |>
  dplyr::group_by(type_actif, cout_unitaire) |>
  dplyr::summarise(
    n_annees_avec_feux  = dplyr::n_distinct(annee),
    total_nb_brules     = sum(nb_brules,  na.rm = TRUE),
    total_cout_reel     = sum(cout_reel,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    moy_nb_brules_30ans = total_nb_brules / N_ANNEES,
    moy_cout_30ans      = total_cout_reel / N_ANNEES
  )

cout_B_total_annuel <- sum(section_B_resume$moy_cout_30ans)

readr::write_csv(section_B_annee,
                 file.path(wd_sortie08, "section_B_feux_reels_par_annee.csv"))
readr::write_csv(section_B_resume,
                 file.path(wd_sortie08, "section_B_resume.csv"))

cat(sprintf("\n── Section B Summary ──\n"))
cat(sprintf("  Real mean annual cost: %.3f M$\n",  cout_B_total_annuel / 1e6))
cat(sprintf("  Real total cost 30 years: %.3f M$\n",
            sum(section_B_resume$total_cout_reel) / 1e6))

rm(actifs_pts_zone, pts_lig_zone, feux_hist, resultats_B_liste); gc()


## ══════════════════════════════════════════════════════
## SECTION C: COMPARISON A vs B
## ══════════════════════════════════════════════════════

cat(sprintf("\n[%s] ══ SECTION C: Comparison ══\n",
            format(Sys.time(), "%H:%M:%S")))

## ── C1. Comparative table by type_actif ────────────────────────────

comparaison <- section_A_par_type |>
  dplyr::select(type_actif, cout_espere_annuel) |>
  dplyr::full_join(
    section_B_resume |>
      dplyr::select(type_actif, moy_cout_30ans, total_cout_reel,
                    moy_nb_brules_30ans, n_annees_avec_feux),
    by = "type_actif"
  ) |>
  dplyr::mutate(
    diff_annuel    = moy_cout_30ans - cout_espere_annuel,
    ratio_B_sur_A  = dplyr::if_else(
      !is.na(cout_espere_annuel) & cout_espere_annuel > 0,
      moy_cout_30ans / cout_espere_annuel,
      NA_real_
    )
  ) |>
  dplyr::arrange(dplyr::desc(dplyr::coalesce(cout_espere_annuel, 0)))

## Quebec total row
total_C <- tibble::tibble(
  type_actif          = "TOTAL QUÉBEC",
  cout_espere_annuel  = cout_A_total_annuel,
  moy_cout_30ans      = cout_B_total_annuel,
  total_cout_reel     = sum(section_B_resume$total_cout_reel),
  moy_nb_brules_30ans = NA_real_,
  n_annees_avec_feux  = NA_integer_,
  diff_annuel         = cout_B_total_annuel - cout_A_total_annuel,
  ratio_B_sur_A       = cout_B_total_annuel / cout_A_total_annuel
)

section_C <- dplyr::bind_rows(comparaison, total_C)
readr::write_csv(section_C, file.path(wd_sortie08, "section_C_comparaison.csv"))


## ══════════════════════════════════════════════════════
## FINAL SUMMARY
## ══════════════════════════════════════════════════════

duree <- round(as.numeric(difftime(Sys.time(), t_debut, units = "secs")))

cat(sprintf("\n╔══════════════════════════════════════════════════╗\n"))
cat(sprintf("║  FINAL SUMMARY — Historical analysis 1981–2010  ║\n"))
cat(sprintf("╠══════════════════════════════════════════════════╣\n"))
cat(sprintf("║  Section A — Boulanger Baseline (probabilistic) ║\n"))
cat(sprintf("║    Expected annual cost: %8.3f M$             ║\n",
            cout_A_total_annuel / 1e6))
cat(sprintf("║    Expected cost 30 yr : %8.3f M$             ║\n",
            cout_A_total_annuel * N_ANNEES / 1e6))
cat(sprintf("╠══════════════════════════════════════════════════╣\n"))
cat(sprintf("║  Section B — Real fires (spatial intersection)  ║\n"))
cat(sprintf("║    Mean annual cost    : %8.3f M$             ║\n",
            cout_B_total_annuel / 1e6))
cat(sprintf("║    Total cost 30 yr    : %8.3f M$             ║\n",
            cout_B_total_annuel * N_ANNEES / 1e6))
cat(sprintf("╠══════════════════════════════════════════════════╣\n"))
cat(sprintf("║  B/A ratio (global calibration): %6.2fx        ║\n",
            total_C$ratio_B_sur_A))
cat(sprintf("║  Total elapsed: %ds                             ║\n", duree))
cat(sprintf("╚══════════════════════════════════════════════════╝\n"))

cat(sprintf("\nOutputs in: %s\n", wd_sortie08))


### ======================================================
### PART C — Delta calibration — correction of Boulanger bias
###
### Method:
###   r_type = historical_real_cost_avg(type) / expected_Boulanger_Baseline_cost(type)
###   Calibrated_cost(type, scen, yr) = Boulanger_cost(type, scen, yr) × r_type
###
###   → For types without sufficient historical data:
###     fallback to the global factor r_global.
###
### Produces charts and tables with calibrated costs.
### Adds a Section 5 calibration synthesis
### (factors, before/after, by type).
###
### Prerequisites:
###   02_Calibrated_costs.R (Part A) → Sortie/02_calibrated_costs/couts/   (wd_sortie05)
###   02_Calibrated_costs.R (Part B) → Sortie/02_calibrated_costs/historique/ (wd_sortie08)
###
### Outputs in Sortie/02_calibrated_costs/calibration/   (wd_sortie09) :
###   graphiques/   → PDF
###   tableaux/     → CSV
###   tableaux/s5_facteurs_calibration.csv
### ======================================================

if (!exists("wd")) source(here::here("Code", "00_Main.R"))

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(scales)
library(sf)

select <- dplyr::select
filter <- dplyr::filter

t_debut <- Sys.time()
cat(sprintf("[%s] ══ Starting delta calibration ══\n\n",
            format(t_debut, "%H:%M:%S")))


## ══════════════════════════════════════════════════════
## CONSTANTES
## ══════════════════════════════════════════════════════

MODELES_BOULANGER <- c("CANESM2", "HadGEM2-ES", "MIROC-ESM-CHEM")
INVENTAIRES       <- c("stable", "EE_couple", "BAU_couple")
HORIZONS_FUTURS   <- c("2011-2040", "2041-2070", "2071-2100")

if (TEST_MODE) {
  MODELES_BOULANGER <- MODELES_BOULANGER[1]
  INVENTAIRES       <- "stable"
  HORIZONS_FUTURS   <- HORIZONS_FUTURS[1]
}

LBL_INVENTAIRE <- c(stable = "Stable", EE_couple = "EE", BAU_couple = "BAU")
LBL_SCEN       <- c(RCP45 = "RCP4.5 — Moderate emissions",
                    RCP85 = "RCP8.5 — High emissions")

PAL_SCEN <- c(RCP45 = "#2E7D32", RCP85 = "#C62828")
PAL_INVENTAIRE <- c("Stable" = "#1565C0", "EE" = "#2E7D32", "BAU" = "#E65100")
PAL_MODELE <- c(CANESM2 = "#1565C0", `HadGEM2-ES` = "#E65100",
                `MIROC-ESM-CHEM` = "#6A1B9A")

## Internal processing uses French names; English translations applied at plot layer
PAL_ACTIF <- c(
  "Postes de transformation"         = "#B71C1C",
  "Poteaux de bois de transport"     = "#E65100",
  "Poteaux de bois de distribution"  = "#F9A825",
  "Transformateurs"                  = "#01579B",
  "Lignes de distribution"           = "#00695C"
)

ACTIF_ENG <- c(
  "Postes de transformation"         = "Substations",
  "Poteaux de bois de transport"     = "Transmission Wood Poles",
  "Poteaux de bois de distribution"  = "Distribution Wood Poles",
  "Transformateurs"                  = "Transformers",
  "Lignes de distribution"           = "Distribution Lines"
)

PAL_ACTIF_ENG <- setNames(unname(PAL_ACTIF), unname(ACTIF_ENG))


## ── Output directories ────────────────────────────────
wd_sortie09_graph <- file.path(wd_sortie09, "graphiques")
wd_sortie09_tab   <- file.path(wd_sortie09, "tableaux")

sauv_g <- function(nom, g, w = 14, h = 7) {
  ggplot2::ggsave(file.path(wd_sortie09_graph, nom), g,
                  width = w, height = h, dpi = 300, bg = "white")
  cat(sprintf("    → %s\n", nom))
}
sauv_t <- function(nom, df) {
  readr::write_csv(df, file.path(wd_sortie09_tab, nom))
  cat(sprintf("    → %s\n", nom))
}

theme_ff <- theme_bw(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "#ECEFF1"),
    strip.text       = element_text(face = "bold", size = 10)
  )


## ══════════════════════════════════════════════════════
## SECTION 0 — CALIBRATION FACTORS (from Part B)
## ══════════════════════════════════════════════════════

cat(sprintf("[%s] Computing delta calibration factors...\n",
            format(Sys.time(), "%H:%M:%S")))

## Check Part B files
for (f in c(file.path(wd_sortie08, "section_A_boulanger_baseline.csv"),
            file.path(wd_sortie08, "section_B_resume.csv"))) {
  if (!file.exists(f)) stop("File not found: ", f,
                            "\n  → Source Code/02_Calibrated_costs.R (Part B) first")
}

section_A <- readr::read_csv(
  file.path(wd_sortie08, "section_A_boulanger_baseline.csv"),
  show_col_types = FALSE
) |> filter(type_actif != "TOTAL QUÉBEC")

section_B <- readr::read_csv(
  file.path(wd_sortie08, "section_B_resume.csv"),
  show_col_types = FALSE
)

## Recode to the same groups as Part A (identical to recode_actifs())
recode_actifs <- function(df) {
  df |> mutate(type_actif = case_when(
    type_actif %in% c("Poteaux de bois distribution-transport",
                      "Poteaux bois distribution (réel)")    ~ "Poteaux de bois de distribution",
    type_actif %in% c("Poteaux de bois (h-frame)",
                      "Poteaux de bois de transport")        ~ "Poteaux de bois de transport",
    grepl("^Transfo", type_actif)                           ~ "Transformateurs",
    type_actif == "Postes de transformation (nombre)"       ~ "Postes de transformation",
    type_actif == "Lignes distribution MT"                  ~ "Lignes de distribution",
    TRUE                                                     ~ type_actif
  ))
}

## Aggregate A and B at the asset group level
A_groupe <- section_A |>
  recode_actifs() |>
  group_by(type_actif) |>
  summarise(cout_A_annuel = sum(cout_espere_annuel, na.rm = TRUE), .groups = "drop")

B_groupe <- section_B |>
  recode_actifs() |>
  group_by(type_actif) |>
  summarise(cout_B_annuel = sum(moy_cout_30ans, na.rm = TRUE), .groups = "drop")

## Global factor (fallback for types without historical data)
r_global <- sum(B_groupe$cout_B_annuel, na.rm = TRUE) /
  sum(A_groupe$cout_A_annuel, na.rm = TRUE)

cat(sprintf("[%s] Global calibration factor: %.4f (-%.1f%%)\n",
            format(Sys.time(), "%H:%M:%S"), r_global, (1 - r_global) * 100))

## Factors by group — global fallback if no historical data
facteurs_cal <- A_groupe |>
  left_join(B_groupe, by = "type_actif") |>
  mutate(
    cout_B_annuel   = coalesce(cout_B_annuel, 0),
    r_type          = if_else(cout_A_annuel > 0 & cout_B_annuel > 0,
                              cout_B_annuel / cout_A_annuel,
                              NA_real_),
    ## Global fallback for types never observed in real fires
    r_effectif      = coalesce(r_type, r_global),
    source_facteur  = if_else(!is.na(r_type), "type-specific", "global (fallback)")
  )

cat(sprintf("[%s] Factors by asset group:\n", format(Sys.time(), "%H:%M:%S")))
facteurs_cal |>
  mutate(across(c(cout_A_annuel, cout_B_annuel), ~ round(. / 1e6, 3))) |>
  mutate(r_effectif = round(r_effectif, 4)) |>
  select(type_actif, cout_A_annuel, cout_B_annuel, r_effectif, source_facteur) |>
  print(n = Inf)

## Fast lookup type → r_effectif
lookup_r <- setNames(facteurs_cal$r_effectif, facteurs_cal$type_actif)


## ══════════════════════════════════════════════════════
## SECTION 1 — LOADING AND CALIBRATION OF PART A DATA
## ══════════════════════════════════════════════════════

cat(sprintf("\n[%s] Loading and calibrating Part A data...\n",
            format(Sys.time(), "%H:%M:%S")))

combinaisons <- expand.grid(
  modele     = MODELES_BOULANGER,
  inventaire = INVENTAIRES,
  stringsAsFactors = FALSE
)

lire_csv_combo <- function(prefixe, modele, inventaire) {
  nom    <- paste0(prefixe, "_boulanger_", modele, "_", inventaire, ".csv")
  chemin <- file.path(wd_sortie05, nom)
  if (!file.exists(chemin)) { warning("File not found: ", chemin); return(NULL) }
  readr::read_csv(chemin, show_col_types = FALSE) |>
    mutate(modele = modele, inventaire = inventaire,
           lbl_inventaire = LBL_INVENTAIRE[inventaire])
}

detail_raw <- purrr::pmap_dfr(combinaisons, function(modele, inventaire)
  lire_csv_combo("couts_detail", modele, inventaire))

## Recode + aggregation of sub-types
detail <- detail_raw |>
  recode_actifs() |>
  group_by(modele, inventaire, lbl_inventaire, SCEN, CELL_ID, YR, horizon,
           score_raster, type_actif, cout_unitaire) |>
  summarise(cout_espere = sum(cout_espere, na.rm = TRUE), .groups = "drop")

## Apply the calibration factor
detail <- detail |>
  mutate(
    r              = coalesce(lookup_r[type_actif], r_global),
    cout_calibre   = cout_espere * r
  )

## Recompute qc (QC total by scenario × year × model × inventory)
qc <- detail |>
  group_by(modele, inventaire, lbl_inventaire, SCEN, YR) |>
  summarise(
    cout_total_qc     = sum(cout_espere,  na.rm = TRUE),
    cout_total_qc_cal = sum(cout_calibre, na.rm = TRUE),
    .groups = "drop"
  )

## Factorise for consistent ordering
factoriser <- function(df) df |>
  mutate(
    inventaire     = factor(inventaire,     levels = INVENTAIRES),
    lbl_inventaire = factor(lbl_inventaire, levels = unname(LBL_INVENTAIRE)),
    SCEN           = factor(SCEN,           levels = c("Baseline", "RCP45", "RCP85"))
  )

qc     <- factoriser(qc)
detail <- factoriser(detail)

if (TEST_MODE) {
  qc     <- dplyr::filter(qc,     SCEN %in% c("Baseline", "RCP45"))
  detail <- dplyr::filter(detail, SCEN %in% c("Baseline", "RCP45"))
}

cat(sprintf("[%s] %d rows (calibrated detail)\n",
            format(Sys.time(), "%H:%M:%S"), nrow(detail)))


## ══════════════════════════════════════════════════════
## SECTION 2 — Inter-model × inventory (calibrated)
## ══════════════════════════════════════════════════════

cat(strrep("═", 60), "\n")
cat("SECTION 2: Inter-model × inventory [calibrated]\n")
cat(strrep("═", 60), "\n\n")


## ── 2.1  Time series: mean + min–max ribbon ──

cat(sprintf("[%s] 2.1 Time series...\n", format(Sys.time(), "%H:%M:%S")))

moy_qc <- qc |>
  filter(SCEN %in% c("RCP45", "RCP85")) |>
  group_by(SCEN, inventaire, lbl_inventaire, YR) |>
  summarise(
    cout_moy     = mean(cout_total_qc_cal),
    cout_min     = min(cout_total_qc_cal),
    cout_max     = max(cout_total_qc_cal),
    cout_moy_brut = mean(cout_total_qc),   ## raw for comparison
    .groups = "drop"
  )

g2_1 <- ggplot(moy_qc,
               aes(x = YR, y = cout_moy / 1e6, colour = SCEN, fill = SCEN)) +
  geom_ribbon(aes(ymin = cout_min / 1e6, ymax = cout_max / 1e6),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ lbl_inventaire, nrow = 1) +
  scale_colour_manual(values = PAL_SCEN, labels = LBL_SCEN, name = "Scenario") +
  scale_fill_manual(values   = PAL_SCEN, labels = LBL_SCEN, name = "Scenario") +
  scale_x_continuous(breaks = seq(2011, 2100, 15)) +
  scale_y_continuous(labels = label_number(suffix = " M CAD", big.mark = ",")) +
  coord_cartesian(xlim = c(2011, 2100)) +
  labs(title = "Annual Expected Replacement Cost by Asset Portfolio",
       x = NULL, y = "Expected cost (M CAD)") +
  theme_ff

sauv_g("s2_1_series_par_inventaire_calibre_ENG.pdf", g2_1)


## ── 2.3  Bar chart: asset composition × inventory × horizon ─

cat(sprintf("\n[%s] 2.3 Bar chart assets x inventory x horizon...\n",
            format(Sys.time(), "%H:%M:%S")))

area_actif <- detail |>
  filter(SCEN %in% c("RCP45", "RCP85")) |>
  group_by(modele, SCEN, inventaire, lbl_inventaire, YR, type_actif) |>
  summarise(cout_cal = sum(cout_calibre, na.rm = TRUE), .groups = "drop") |>
  group_by(SCEN, inventaire, lbl_inventaire, YR, type_actif) |>
  summarise(cout_moy = mean(cout_cal), .groups = "drop")

area_actif_horiz <- area_actif |>
  mutate(horizon = case_when(
    YR >= 2011 & YR <= 2040 ~ "2011-2040",
    YR >= 2041 & YR <= 2070 ~ "2041-2070",
    YR >= 2071 & YR <= 2100 ~ "2071-2100"
  )) |>
  filter(!is.na(horizon)) |>
  group_by(SCEN, inventaire, lbl_inventaire, horizon, type_actif) |>
  summarise(cout_moy = mean(cout_moy), .groups = "drop") |>
  mutate(
    horizon        = factor(horizon, levels = HORIZONS_FUTURS),
    type_actif_eng = factor(ACTIF_ENG[as.character(type_actif)],
                            levels = unname(ACTIF_ENG))
  )

g2_3 <- ggplot(filter(area_actif_horiz, SCEN == "RCP85"),
               aes(x = horizon, y = cout_moy / 1e6, fill = type_actif_eng)) +
  geom_col(width = 0.7, alpha = 0.9) +
  facet_wrap(~ lbl_inventaire, nrow = 1) +
  scale_fill_manual(values = PAL_ACTIF_ENG, name = "Asset type") +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40),
                     labels = label_number(suffix = " M CAD", big.mark = ","),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Average Annual Replacement Cost by Asset Type — RCP8.5",
       x = NULL, y = "Avg. annual cost (M CAD)") +
  guides(fill = guide_legend(ncol = 3)) +
  theme_ff

g2_3b <- g2_3 %+% filter(area_actif_horiz, SCEN == "RCP45") +
  labs(title = "Average Annual Replacement Cost by Asset Type — RCP4.5")

g2_3_combined <- (g2_3b / g2_3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
sauv_g("s2_3_bar_actifs_calibre_ENG.pdf", g2_3_combined, w = 15, h = 12)


## ══════════════════════════════════════════════════════
## SECTION 3 — Regional decomposition (calibrated)
## ══════════════════════════════════════════════════════

cat(strrep("═", 60), "\n")
cat("SECTION 3: Regional decomposition [calibrated]\n")
cat(strrep("═", 60), "\n\n")

sf_use_s2(FALSE)

cat(sprintf("[%s] Building CELL_ID to region lookup...\n",
            format(Sys.time(), "%H:%M:%S")))

chemin_regions <- file.path(wd_data, "Zones", "regio_s.shp")
if (!file.exists(chemin_regions)) {
  cat("  [SKIP] regio_s.shp not found — Section 3 skipped\n")
  lookup_region <- tibble::tibble(CELL_ID = integer(), region = character())
} else {
  cellules_ref_sf <- readr::read_csv(
    file.path(wd_sortie03, "cellules_reference_boulanger.csv"),
    show_col_types = FALSE
  ) |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  regions_sf <- sf::st_read(chemin_regions, quiet = TRUE) |>
    sf::st_transform(4326) |>
    select(RES_NM_REG)
  
  lookup_region <- sf::st_join(cellules_ref_sf, regions_sf, join = sf::st_within) |>
    sf::st_drop_geometry() |>
    select(CELL_ID, region = RES_NM_REG)
  
  cat(sprintf("[%s] %d / %d cells assigned to a region\n",
              format(Sys.time(), "%H:%M:%S"),
              sum(!is.na(lookup_region$region)), nrow(lookup_region)))
}

if (nrow(lookup_region) > 0) {
  
  top5_regions <- detail |>
    filter(SCEN == "RCP85", YR >= 2011, inventaire == "stable") |>
    left_join(lookup_region, by = "CELL_ID") |>
    filter(!is.na(region)) |>
    group_by(modele, region) |>
    summarise(cout = sum(cout_calibre, na.rm = TRUE), .groups = "drop") |>
    group_by(region) |>
    summarise(cout_moy = mean(cout), .groups = "drop") |>
    slice_max(cout_moy, n = 5) |>
    pull(region)
  
  couts_region <- detail |>
    filter(SCEN %in% c("RCP45", "RCP85")) |>
    left_join(lookup_region, by = "CELL_ID") |>
    mutate(region_label = if_else(
      !is.na(region) & region %in% top5_regions, region, "Rest of Quebec"
    )) |>
    group_by(modele, SCEN, inventaire, lbl_inventaire, YR, region_label) |>
    summarise(cout_cal = sum(cout_calibre, na.rm = TRUE), .groups = "drop") |>
    group_by(SCEN, inventaire, lbl_inventaire, YR, region_label) |>
    summarise(cout_moy = mean(cout_cal), .groups = "drop") |>
    mutate(region_label = factor(region_label,
                                 levels = c(top5_regions, "Rest of Quebec")))
  
  PAL_REGION <- setNames(
    c("#1565C0", "#C62828", "#2E7D32", "#E65100", "#6A1B9A", "#9E9E9E"),
    c(top5_regions, "Rest of Quebec")
  )
  
  couts_region_horiz <- couts_region |>
    mutate(horizon = case_when(
      YR >= 2011 & YR <= 2040 ~ "2011-2040",
      YR >= 2041 & YR <= 2070 ~ "2041-2070",
      YR >= 2071 & YR <= 2100 ~ "2071-2100"
    )) |>
    filter(!is.na(horizon)) |>
    group_by(SCEN, inventaire, lbl_inventaire, horizon, region_label) |>
    summarise(cout_moy = mean(cout_moy), .groups = "drop") |>
    mutate(horizon = factor(horizon, levels = HORIZONS_FUTURS))
  
  g3_1_rcp85 <- ggplot(
    filter(couts_region_horiz, SCEN == "RCP85"),
    aes(x = horizon, y = cout_moy / 1e6, fill = region_label)
  ) +
    geom_col(width = 0.7, alpha = 0.9) +
    facet_wrap(~ lbl_inventaire, nrow = 1) +
    scale_fill_manual(values = PAL_REGION, name = "Region") +
    scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 40),
                       labels = label_number(suffix = " M CAD", big.mark = ","),
                       expand = expansion(mult = c(0, 0))) +
    labs(title = "Average Annual Replacement Cost by Administrative Region — RCP8.5",
         x = NULL, y = "Avg. annual cost (M CAD)") +
    guides(fill = guide_legend(ncol = 3)) +
    theme_ff
  
  g3_1_rcp45 <- g3_1_rcp85 %+%
    filter(couts_region_horiz, SCEN == "RCP45") +
    labs(title = "Average Annual Replacement Cost by Administrative Region — RCP4.5")
  
  g3_1_combined <- (g3_1_rcp45 / g3_1_rcp85) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  sauv_g("s3_1_regions_bar_calibre_ENG.pdf", g3_1_combined, w = 15, h = 12)
}


## ══════════════════════════════════════════════════════
## FINAL CONSOLE SUMMARY
## ══════════════════════════════════════════════════════

duree <- round(as.numeric(difftime(Sys.time(), t_debut, units = "secs")))

cat(sprintf("\n%s\n", strrep("═", 60)))
cat("  DELTA CALIBRATION SUMMARY\n")
cat(sprintf("%s\n\n", strrep("═", 60)))

cat(sprintf("  Global factor r = %.4f  (correction: -%.1f%%)\n\n",
            r_global, (1 - r_global) * 100))

cat("  Factors by asset type:\n")
cat(sprintf("  %-38s  %6s  %8s  %8s  %s\n",
            "Type", "r", "Raw M$", "Cal. M$", "Source"))
cat(sprintf("  %s\n", strrep("─", 80)))
for (i in seq_len(nrow(facteurs_cal))) {
  cat(sprintf("  %-38s  %6.4f  %8.3f  %8.3f  %s\n",
              facteurs_cal$type_actif[i],
              facteurs_cal$r_effectif[i],
              facteurs_cal$cout_A_annuel[i] / 1e6,
              facteurs_cal$cout_B_annuel[i] / 1e6,
              facteurs_cal$source_facteur[i]))
}
cat(sprintf("  %s\n", strrep("─", 80)))
cat(sprintf("  %-38s  %6.4f  %8.3f  %8.3f\n",
            "TOTAL",
            r_global,
            sum(facteurs_cal$cout_A_annuel) / 1e6,
            sum(facteurs_cal$cout_B_annuel) / 1e6))

cat(sprintf("\n  Elapsed: %ds\n", duree))
cat(sprintf("  Charts  -> %s\n", wd_sortie09_graph))
cat(sprintf("  Tables  -> %s\n", wd_sortie09_tab))
