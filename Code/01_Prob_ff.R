### ======================================================
### 01_Prob_ff.R
### PART A — Fire probabilities per Boulanger cell × year × scenario
### PART B — Redistribution by MRNF raster score × join with assets
###
### PART A :
###   - Creates zones_propagation.gpkg from regio_s.shp (dissolved)
###   - P_feu = 1 / FireCycle per cell ~0.25°
###   Outputs → Sortie/01_prob_ff/probabilites/   (wd_sortie03)
###     cellules_reference_boulanger.csv
###     prob_feux_boulanger_{MODELE}_{SCEN}.csv
###
### PART B :
###   facteur(S, C) = [S × N_total(C)] / [Σ_s s × N(s,C)]
###   P_ajustée(S, C, YR, SCEN) = P_zone(C, YR, SCEN) × facteur(S, C)
###   Outputs → Sortie/01_prob_ff/redistribution/   (wd_sortie04)
###     facteurs_redistribution_boulanger.csv
###     resultats_feux_actifs_{MODELE}_{INVENTAIRE}.csv
### ======================================================

if (!exists("wd")) source(here::here("Code", "00_Main.R"))

TEST_MODE <- TRUE  # Set to FALSE for full run

library(terra)
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(arrow)

sf::sf_use_s2(FALSE)

terra::tmpFiles(orphan = TRUE, remove = TRUE)
terraOptions(memfrac = 0.5)

t_debut <- Sys.time()
cat(sprintf("[%s] ══ Starting 01_Prob_ff ══\n", format(t_debut, "%H:%M:%S")))


## ══════════════════════════════════════════════════════
## PARAMETERS
## ══════════════════════════════════════════════════════

MODELES_A_TRAITER <- c("CANESM2", "HadGEM2-ES", "MIROC-ESM-CHEM")

RASTER_PREFIX <- c(
  CANESM2          = "CanESM2",
  `HadGEM2-ES`     = "Hadley",
  `MIROC-ESM-CHEM` = "MIROC"
)

INVENTAIRES_A_TRAITER <- c("stable", "BAU_couple", "EE_couple")

FORCER_RECALCUL    <- TRUE
INVENTAIRES_FORCER <- c("stable")

COUPLAGE_BAU_PARQUET <- c(RCP45 = "BAU_SSP2", RCP85 = "BAU_SSP3")
COUPLAGE_EE_PARQUET  <- c(RCP45 = "EE_SSP2",  RCP85 = "EE_SSP3")

ANNEES_REP <- c("2011-2040" = 2025L, "2041-2070" = 2055L, "2071-2100" = 2085L)

TENSION_KV_TYPE <- c(
  "0.6"  = "Transfo 10 kVA",
  "4.2"  = "Transfo 25 kVA",
  "12.4" = "Transfo 25 kVA",
  "13.8" = "Transfo 25 kVA",
  "24.9" = "Transfo 25 kVA",
  "34.5" = "Transfo 150-300 kVA"
)

wd_boulanger <- file.path(wd_data, "Boulanger_RCP")

CHEMIN_RASTER      <- file.path(wd_data, "Potentiel_Intensite_Propagation_Feux",
                                "Couche globale", "Potentiel_IP_25-26.tif")
CHEMIN_SUPPORTS    <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_LAV <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_MAT <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_NOR <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_IDM <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_ORL <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POTEAUX_SEI <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_POSTES      <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")
CHEMIN_TRANSFOS    <- file.path(wd_data, "INSERT ASSET DATA HERE.shp")

DOSSIER_PARQUET <- wd_inventaires

p <- function(f) file.path(DOSSIER_PARQUET, f)
CHEMINS_PARQUET <- list(
  Poteaux = list(
    BAU_SSP2 = p("INSERT ASSET DATA HERE.parquet"),
    BAU_SSP3 = p("INSERT ASSET DATA HERE.parquet"),
    EE_SSP2  = p("INSERT ASSET DATA HERE.parquet"),
    EE_SSP3  = p("INSERT ASSET DATA HERE.parquet")
  ),
  Supports = list(
    BAU_SSP2 = p("INSERT ASSET DATA HERE.parquet"),
    BAU_SSP3 = p("INSERT ASSET DATA HERE.parquet"),
    EE_SSP2  = p("INSERT ASSET DATA HERE.parquet"),
    EE_SSP3  = p("INSERT ASSET DATA HERE.parquet")
  ),
  Transformateurs = list(
    BAU_SSP2 = p("INSERT ASSET DATA HERE.parquet"),
    BAU_SSP3 = p("INSERT ASSET DATA HERE.parquet"),
    EE_SSP2  = p("INSERT ASSET DATA HERE.parquet"),
    EE_SSP3  = p("INSERT ASSET DATA HERE.parquet")
  ),
  Postes = list(
    BAU_SSP2 = p("INSERT ASSET DATA HERE.parquet"),
    BAU_SSP3 = p("INSERT ASSET DATA HERE.parquet"),
    EE_SSP2  = p("INSERT ASSET DATA HERE.parquet"),
    EE_SSP3  = p("INSERT ASSET DATA HERE.parquet")
  ),
  Lignes_distribution = list(
    stable   = p("INSERT ASSET DATA HERE.parquet"),
    BAU_SSP2 = p("INSERT ASSET DATA HERE.parquet"),
    BAU_SSP3 = p("INSERT ASSET DATA HERE.parquet"),
    EE_SSP2  = p("INSERT ASSET DATA HERE.parquet"),
    EE_SSP3  = p("INSERT ASSET DATA HERE.parquet")
  )
)
rm(p)

TAUX_BOIS_ORL_SEI <- 0.7978

if (TEST_MODE) {
  cat("RUNNING IN TEST MODE - limited data only\n")
  MODELES_A_TRAITER     <- MODELES_A_TRAITER[1]
  INVENTAIRES_A_TRAITER <- "stable"
  ANNEES_REP            <- ANNEES_REP["2011-2040"]
}


## ══════════════════════════════════════════════════════
## PART A — FIRE PROBABILITIES
## ══════════════════════════════════════════════════════

cat(sprintf("\n[%s] ── PART A: fire probabilities ──\n",
            format(Sys.time(), "%H:%M:%S")))

## ── A1. Spatial mask — dissolve regio_s.shp ────────

cat(sprintf("[%s] Creating zones_propagation from regio_s.shp...\n",
            format(Sys.time(), "%H:%M:%S")))

chemin_regions <- file.path(wd_data, "Zones", "regio_s.shp")
if (!file.exists(chemin_regions))
  stop("File not found: ", chemin_regions)

zones_propagation <- sf::st_read(chemin_regions, quiet = TRUE) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_sf()

chemin_zones <- file.path(wd_sortie01, "zones_propagation.gpkg")
sf::st_write(zones_propagation, chemin_zones, delete_dsn = TRUE, quiet = TRUE)
cat(sprintf("[%s]   → %s\n", format(Sys.time(), "%H:%M:%S"), chemin_zones))

masque_vect <- zones_propagation |>
  sf::st_transform(4326) |>
  terra::vect()

## ── A2. Reference grid CELL_ID ───────────────────

cat(sprintf("[%s] Building reference grid (CELL_ID)...\n",
            format(Sys.time(), "%H:%M:%S")))

r_ref <- terra::mask(
  terra::crop(
    terra::rast(file.path(wd_boulanger, "FireCycle_Baseline_WGS1984_0.25.tif")),
    masque_vect
  ),
  masque_vect
)

cellules_ref <- terra::as.data.frame(r_ref, xy = TRUE, na.rm = TRUE) |>
  dplyr::select(lon = x, lat = y) |>
  dplyr::arrange(lat, lon) |>
  dplyr::mutate(CELL_ID = seq(0L, dplyr::n() - 1L))

cat(sprintf("[%s] %d active cells\n",
            format(Sys.time(), "%H:%M:%S"), nrow(cellules_ref)))

chemin_ref <- file.path(wd_sortie03, "cellules_reference_boulanger.csv")
write_csv(cellules_ref, chemin_ref)
cat(sprintf("[%s]   → %s\n", format(Sys.time(), "%H:%M:%S"), basename(chemin_ref)))

pts_ref <- terra::vect(
  as.matrix(cellules_ref[, c("lon", "lat")]),
  crs = "EPSG:4326"
)

## ── A3. Model loop — compute P_feu ─────────────────

for (modele in MODELES_A_TRAITER) {

  prefix <- RASTER_PREFIX[[modele]]
  cat(sprintf("\n[%s] ── Model: %s ──\n", format(Sys.time(), "%H:%M:%S"), modele))

  rasters_meta <- tibble::tribble(
    ~fichier,                                                          ~SCEN,      ~yr_debut, ~yr_fin,
    "FireCycle_Baseline_WGS1984_0.25.tif",                            "Baseline",  1981L,     2010L,
    sprintf("FireCycle_%s_RCP45_2011-2040_WGS1984_0.25.tif", prefix), "RCP45",     2011L,     2040L,
    sprintf("FireCycle_%s_RCP45_2041-2070_WGS1984_0.25.tif", prefix), "RCP45",     2041L,     2070L,
    sprintf("FireCycle_%s_RCP45_2071-2100_WGS1984_0.25.tif", prefix), "RCP45",     2071L,     2100L,
    sprintf("FireCycle_%s_RCP85_2011-2040_WGS1984_0.25.tif", prefix), "RCP85",     2011L,     2040L,
    sprintf("FireCycle_%s_RCP85_2041-2070_WGS1984_0.25.tif", prefix), "RCP85",     2041L,     2070L,
    sprintf("FireCycle_%s_RCP85_2071-2100_WGS1984_0.25.tif", prefix), "RCP85",     2071L,     2100L
  ) |>
    dplyr::mutate(chemin = file.path(wd_boulanger, fichier))

  if (TEST_MODE)
    rasters_meta <- dplyr::filter(rasters_meta,
                                  SCEN %in% c("Baseline", "RCP45") & yr_fin <= 2040L)

  manquants <- rasters_meta$chemin[!file.exists(rasters_meta$chemin)]
  if (length(manquants) > 0) {
    warning("Model ", modele, " — rasters not found, skipped:\n  ",
            paste(manquants, collapse = "\n  "))
    next
  }

  resultats_bruts <- purrr::map_dfr(seq_len(nrow(rasters_meta)), function(i) {
    meta <- rasters_meta[i, ]
    cat(sprintf("[%s]   %s (%s, %d–%d)...\n",
                format(Sys.time(), "%H:%M:%S"),
                meta$fichier, meta$SCEN, meta$yr_debut, meta$yr_fin))
    r      <- terra::mask(terra::crop(terra::rast(meta$chemin), masque_vect), masque_vect)
    valeurs <- terra::extract(r, pts_ref)[[2]]
    data.frame(
      CELL_ID    = cellules_ref$CELL_ID,
      SCEN       = meta$SCEN,
      yr_debut   = meta$yr_debut,
      yr_fin     = meta$yr_fin,
      fire_cycle = as.numeric(valeurs)
    )
  })

  probabilites <- resultats_bruts |>
    dplyr::left_join(cellules_ref, by = "CELL_ID") |>
    dplyr::mutate(
      P_feu = pmin(1 / fire_cycle, 1.0),
      P_feu = round(P_feu, 6)
    ) |>
    tidyr::uncount(yr_fin - yr_debut + 1L, .id = "offset") |>
    dplyr::mutate(YR = yr_debut + offset - 1L) |>
    dplyr::select(CELL_ID, lon, lat, SCEN, YR, P_feu) |>
    dplyr::arrange(SCEN, CELL_ID, YR)

  for (scen in sort(unique(probabilites$SCEN))) {
    df_scen    <- dplyr::filter(probabilites, SCEN == scen)
    chemin_csv <- file.path(wd_sortie03,
                            sprintf("prob_feux_boulanger_%s_%s.csv", modele, scen))
    write_csv(df_scen, chemin_csv)
    cat(sprintf("[%s]   → %s  (%d rows, P_mean = %.4f%%)\n",
                format(Sys.time(), "%H:%M:%S"),
                basename(chemin_csv), nrow(df_scen),
                mean(df_scen$P_feu, na.rm = TRUE) * 100))
  }
}

cat(sprintf("\n[%s] ── PART A complete ──\n", format(Sys.time(), "%H:%M:%S")))


## ══════════════════════════════════════════════════════
## PART B — REDISTRIBUTION BY MRNF RASTER SCORE
## ══════════════════════════════════════════════════════

cat(sprintf("\n[%s] ── PART B: MRNF raster redistribution ──\n",
            format(Sys.time(), "%H:%M:%S")))

## ── B1. MRNF raster ───────────────────────────────────

cat(sprintf("[%s] Loading MRNF raster...\n", format(Sys.time(), "%H:%M:%S")))

if (!file.exists(CHEMIN_RASTER))
  stop("Raster not found: ", CHEMIN_RASTER)

raster_mrnf <- terra::rast(CHEMIN_RASTER)
crs_raster  <- terra::crs(raster_mrnf)
names(raster_mrnf) <- "score_raster"

cat(sprintf("[%s] MRNF raster: resolution %s m\n",
            format(Sys.time(), "%H:%M:%S"),
            paste(round(terra::res(raster_mrnf)), collapse = "×")))

## ── B2. CELL_ID raster reprojected onto MRNF grid ──────

chemin_ckpt_cell_id <- file.path(wd_sortie04, "ckpt_r_cell_id_mrnf.tif")

if (file.exists(chemin_ckpt_cell_id)) {

  cat(sprintf("[%s] [CHECKPOINT] r_cell_id_mrnf loaded from %s\n",
              format(Sys.time(), "%H:%M:%S"), basename(chemin_ckpt_cell_id)))
  r_cell_id_mrnf <- terra::rast(chemin_ckpt_cell_id)

} else {

  cat(sprintf("[%s] Building CELL_ID raster...\n", format(Sys.time(), "%H:%M:%S")))

  ## cellules_ref and masque_vect already available from Part A
  r_ref_masked <- terra::mask(terra::crop(r_ref, masque_vect), masque_vect)

  r_cell_id <- terra::setValues(r_ref_masked, NA_real_)
  idx_cellules <- terra::cellFromXY(
    r_cell_id, as.matrix(cellules_ref[, c("lon", "lat")])
  )
  r_cell_id[idx_cellules] <- cellules_ref$CELL_ID
  names(r_cell_id) <- "CELL_ID"

  cat(sprintf("[%s] Reprojecting CELL_ID → MRNF grid...\n", format(Sys.time(), "%H:%M:%S")))
  r_cell_id_mrnf <- terra::project(r_cell_id, raster_mrnf, method = "near")

  terra::writeRaster(r_cell_id_mrnf, chemin_ckpt_cell_id, overwrite = TRUE)
  cat(sprintf("[%s] [CHECKPOINT] r_cell_id_mrnf saved → %s\n",
              format(Sys.time(), "%H:%M:%S"), basename(chemin_ckpt_cell_id)))
}

## ── B3. MRNF factors ─────────────────────────────────

chemin_facteurs  <- file.path(wd_sortie04, "facteurs_redistribution_boulanger.csv")
raster_mrnf_zone <- terra::mask(terra::crop(raster_mrnf, r_cell_id_mrnf), r_cell_id_mrnf)

if (file.exists(chemin_facteurs)) {

  cat(sprintf("[%s] [CHECKPOINT] factors loaded from %s\n",
              format(Sys.time(), "%H:%M:%S"), basename(chemin_facteurs)))
  facteurs <- readr::read_csv(chemin_facteurs, show_col_types = FALSE)

} else {

  cat(sprintf("[%s] Crosstab CELL_ID × MRNF score...\n", format(Sys.time(), "%H:%M:%S")))

  ct <- terra::crosstab(c(r_cell_id_mrnf, raster_mrnf_zone), long = TRUE)

  distribution_scores <- ct |>
    dplyr::rename(CELL_ID = CELL_ID, score_raster = score_raster, nb_cellules = n) |>
    dplyr::mutate(
      CELL_ID      = as.integer(as.character(CELL_ID)),
      score_raster = as.integer(as.character(score_raster)),
      nb_cellules  = as.integer(nb_cellules)
    ) |>
    dplyr::filter(!is.na(CELL_ID)) |>
    tidyr::complete(CELL_ID = unique(CELL_ID), score_raster = 0:5,
                    fill = list(nb_cellules = 0L)) |>
    dplyr::arrange(CELL_ID, score_raster)

  facteurs <- distribution_scores |>
    dplyr::group_by(CELL_ID) |>
    dplyr::mutate(
      N_total      = sum(nb_cellules),
      denominateur = sum(score_raster * nb_cellules),
      facteur      = dplyr::case_when(
        score_raster == 0 ~ 0,
        denominateur == 0 ~ 1,
        TRUE              ~ (score_raster * N_total) / denominateur
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(CELL_ID, score_raster, nb_cellules, N_total, denominateur, facteur)

  write_csv(facteurs, chemin_facteurs)
  cat(sprintf("[%s] [CHECKPOINT] Factors → %s (%d rows)\n",
              format(Sys.time(), "%H:%M:%S"), basename(chemin_facteurs), nrow(facteurs)))
}

cells_avec_raster <- facteurs |>
  dplyr::group_by(CELL_ID) |>
  dplyr::summarise(a_couverture = sum(nb_cellules) > 0, .groups = "drop")

r_extraction_cell  <- r_cell_id_mrnf
r_extraction_score <- raster_mrnf_zone


## ── B4. Utility functions ──────────────────────────

vers_crs_raster <- function(sf_obj) {
  if (sf::st_crs(sf_obj) != sf::st_crs(crs_raster))
    sf_obj <- sf::st_transform(sf_obj, sf::st_crs(crs_raster))
  sf_obj
}

extraire_cellule_score <- function(chemin, nom, filtre_fn = NULL) {
  pts <- sf::st_read(chemin, quiet = TRUE) |> vers_crs_raster()
  if (!is.null(filtre_fn)) pts <- filtre_fn(pts)
  if (nrow(pts) == 0) return(NULL)
  vpts         <- terra::vect(pts[, "geometry"])
  cell_id_vals <- terra::extract(r_extraction_cell,  vpts)[["CELL_ID"]]
  score_vals   <- terra::extract(r_extraction_score, vpts)[["score_raster"]]
  rm(vpts); gc()
  df <- data.frame(
    type_actif   = nom,
    CELL_ID      = as.integer(cell_id_vals),
    score_raster = as.integer(score_vals)
  ) |> dplyr::filter(!is.na(CELL_ID))
  if (nrow(df) == 0) return(NULL)
  df
}

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

add_poids <- function(df, p = 1.0) if (is.null(df)) NULL else dplyr::mutate(df, poids = p)

construire_cache_lignes <- function(df_ref) {
  purrr::map_dfr(seq_len(nrow(df_ref)), function(i) {
    wkt <- df_ref$exemple_segment[i]
    if (is.na(wkt)) return(NULL)
    geom_wgs <- tryCatch(sf::st_as_sfc(wkt, crs = 4326), error = function(e) NULL)
    if (is.null(geom_wgs) || length(geom_wgs) == 0) return(NULL)
    geom_m   <- sf::st_transform(geom_wgs, 32198)
    long_seg <- as.numeric(sf::st_length(geom_m))
    if (long_seg < 1) return(NULL)
    pts_wgs <- tryCatch(
      sf::st_transform(sf::st_cast(sf::st_segmentize(geom_m, dfMaxLength = 25), "POINT"), 4326),
      error = function(e) sf::st_transform(sf::st_centroid(geom_m), 4326)
    )
    n <- length(pts_wgs)
    sf::st_sf(exemple_segment = rep(wkt, n), fraction = rep(1 / n, n), geometry = pts_wgs)
  })
}

trouver_parquet <- function(type, variante) {
  ch <- CHEMINS_PARQUET[[type]][[variante]]
  if (is.null(ch) || (length(ch) == 1 && is.na(ch))) return(NULL)
  if (!file.exists(ch)) { warning("Parquet file not found: ", ch); return(NULL) }
  ch
}

charger_actifs_stable <- function() {
  cat(sprintf("[%s]   Loading stable assets (shapefiles)...\n",
              format(Sys.time(), "%H:%M:%S")))

  df_ht  <- extraire_cellule_score(CHEMIN_SUPPORTS, "Poteaux de bois (h-frame)",
               filtre_fn = function(x) dplyr::filter(x, HQ_TYPE == "POTEAU"))
  gc()
  df_dt  <- extraire_cellule_score(CHEMIN_SUPPORTS, "Poteaux de bois distribution-transport",
               filtre_fn = function(x) dplyr::filter(x, HQ_TYPE == "POTEAU_DIS"))
  gc()
  df_lav <- extraire_cellule_score(CHEMIN_POTEAUX_LAV, "Poteaux bois distribution (réel)",
               filtre_fn = function(x) dplyr::filter(x, materiel == "Bois"))
  gc()
  df_mat <- extraire_cellule_score(CHEMIN_POTEAUX_MAT, "Poteaux bois distribution (réel)",
               filtre_fn = function(x) dplyr::filter(x, materiel == "Bois"))
  gc()
  df_nor <- extraire_cellule_score(CHEMIN_POTEAUX_NOR, "Poteaux bois distribution (réel)",
               filtre_fn = function(x) dplyr::filter(x, materiel == "Bois"))
  gc()
  df_idm <- extraire_cellule_score(CHEMIN_POTEAUX_IDM, "Poteaux bois distribution (réel)",
               filtre_fn = function(x) dplyr::filter(x, materiel == "Bois"))
  gc()
  df_orl <- extraire_cellule_score(CHEMIN_POTEAUX_ORL, "_orl_tous")
  gc()
  df_sei <- extraire_cellule_score(CHEMIN_POTEAUX_SEI, "_sei_tous")
  gc()
  df_postes <- extraire_cellule_score(CHEMIN_POSTES, "Postes de transformation (nombre)")
  gc()

  transfos_raw <- sf::st_read(CHEMIN_TRANSFOS, quiet = TRUE) |>
    dplyr::filter(classe_app == "Transformateur") |>
    dplyr::mutate(categorie_kva = classifier_kva(valeur_nom)) |>
    dplyr::filter(!is.na(categorie_kva)) |>
    vers_crs_raster()
  vpts_tr    <- terra::vect(transfos_raw[, "geometry"])
  cell_id_tr <- terra::extract(r_extraction_cell,  vpts_tr)[["CELL_ID"]]
  score_tr   <- terra::extract(r_extraction_score, vpts_tr)[["score_raster"]]
  rm(vpts_tr); gc()
  df_tr <- data.frame(
    type_actif   = transfos_raw$categorie_kva,
    CELL_ID      = as.integer(cell_id_tr),
    score_raster = as.integer(score_tr)
  ) |> dplyr::filter(!is.na(CELL_ID))
  rm(transfos_raw, cell_id_tr, score_tr); gc()

  f_lig_s  <- trouver_parquet("Lignes_distribution", "stable")
  df_lig_s <- NULL
  if (!is.null(f_lig_s)) {
    raw_lig_s <- arrow::open_dataset(f_lig_s) |>
      dplyr::filter(reseau == "moyenne tension", year == 2022L) |>
      dplyr::select(dplyr::any_of(c("exemple_segment", "longueur"))) |>
      dplyr::collect()
    if (nrow(raw_lig_s) > 0) {
      df_lig_s <- raw_lig_s |>
        dplyr::left_join(cache_geom_lignes, by = "exemple_segment",
                         relationship = "many-to-many") |>
        dplyr::filter(!is.na(CELL_ID)) |>
        dplyr::mutate(type_actif = "Lignes distribution MT",
                      poids      = as.numeric(longueur) * fraction) |>
        dplyr::select(type_actif, CELL_ID, score_raster, poids)
      cat(sprintf("[%s]     MT lines stable: %d cells\n",
                  format(Sys.time(), "%H:%M:%S"), nrow(df_lig_s)))
    }
    gc()
  }

  df_orl_p <- if (!is.null(df_orl))
    dplyr::mutate(df_orl, type_actif = "Poteaux bois distribution (réel)", poids = TAUX_BOIS_ORL_SEI)
  df_sei_p <- if (!is.null(df_sei))
    dplyr::mutate(df_sei, type_actif = "Poteaux bois distribution (réel)", poids = TAUX_BOIS_ORL_SEI)

  dplyr::bind_rows(
    add_poids(df_ht), add_poids(df_dt),
    add_poids(dplyr::bind_rows(df_lav, df_mat, df_nor, df_idm)),
    df_orl_p, df_sei_p,
    add_poids(df_postes),
    add_poids(df_tr),
    df_lig_s
  ) |> dplyr::mutate(score_raster = tidyr::replace_na(score_raster, -1L))
}

appliquer_cache_ponc <- function(raw_avec_type) {
  if (!"count" %in% names(raw_avec_type))
    raw_avec_type <- dplyr::mutate(raw_avec_type, count = 1L)
  raw_avec_type |>
    dplyr::left_join(geoms_lookup, by = c("geometry" = "geometry_key")) |>
    dplyr::left_join(cache_geom_ponctuels, by = "geom_idx",
                     relationship = "many-to-many") |>
    dplyr::filter(!is.na(CELL_ID)) |>
    dplyr::mutate(poids = count * fraction) |>
    dplyr::select(type_actif, CELL_ID, score_raster, poids)
}

charger_actifs_parquet_annee <- function(variante, annee) {
  cat(sprintf("[%s]     → %s / year %d\n", format(Sys.time(), "%H:%M:%S"), variante, annee))
  chemin    <- function(type) trouver_parquet(type, variante)
  resultats <- list()

  f_pot <- chemin("Poteaux")
  if (!is.null(f_pot)) {
    raw <- arrow::open_dataset(f_pot) |> dplyr::filter(year == annee) |>
      dplyr::select(dplyr::any_of(c("geometry", "count"))) |> dplyr::collect()
    if (nrow(raw) > 0) {
      raw <- dplyr::mutate(raw, type_actif = "Poteaux bois distribution (réel)")
      resultats[["poteaux"]] <- appliquer_cache_ponc(raw)
    }
  } else warning("Poteaux parquet not found for variant: ", variante)

  f_sup <- chemin("Supports")
  if (!is.null(f_sup)) {
    raw_sup <- arrow::open_dataset(f_sup) |>
      dplyr::filter(year == annee, HQ_TYPE %in% c("POTEAU", "POTEAU_DIS")) |>
      dplyr::select(dplyr::any_of(c("geometry", "count", "HQ_TYPE"))) |> dplyr::collect()
    if (nrow(raw_sup) > 0) {
      type_map <- c(POTEAU = "Poteaux de bois (h-frame)", POTEAU_DIS = "Poteaux de bois distribution-transport")
      raw_sup <- dplyr::mutate(raw_sup, type_actif = type_map[HQ_TYPE])
      resultats[["supports"]] <- appliquer_cache_ponc(raw_sup)
    }
  } else message("Supports parquet absent for variant: ", variante, " (skipped)")

  f_tr <- chemin("Transformateurs")
  if (!is.null(f_tr)) {
    raw_tr <- arrow::open_dataset(f_tr) |>
      dplyr::filter(year == annee, tension %in% names(TENSION_KV_TYPE)) |>
      dplyr::select(dplyr::any_of(c("geometry", "count", "tension"))) |> dplyr::collect()
    if (nrow(raw_tr) > 0) {
      raw_tr <- dplyr::mutate(raw_tr, type_actif = TENSION_KV_TYPE[tension])
      resultats[["transfos"]] <- appliquer_cache_ponc(raw_tr)
    }
  } else warning("Transformateurs parquet not found for variant: ", variante)

  f_pos <- chemin("Postes")
  if (!is.null(f_pos)) {
    raw_pos <- arrow::open_dataset(f_pos) |> dplyr::filter(year == annee) |>
      dplyr::select(dplyr::any_of(c("geometry", "count"))) |> dplyr::collect()
    if (nrow(raw_pos) > 0) {
      raw_pos <- dplyr::mutate(raw_pos, type_actif = "Postes de transformation (nombre)")
      resultats[["postes"]] <- appliquer_cache_ponc(raw_pos)
    }
  } else warning("Postes parquet not found for variant: ", variante)

  f_lig <- chemin("Lignes_distribution")
  if (!is.null(f_lig)) {
    raw_lig <- arrow::open_dataset(f_lig) |>
      dplyr::filter(reseau == "moyenne tension", year == annee) |>
      dplyr::select(dplyr::any_of(c("exemple_segment", "longueur"))) |> dplyr::collect()
    if (nrow(raw_lig) > 0) {
      resultats[["lignes"]] <- raw_lig |>
        dplyr::left_join(cache_geom_lignes, by = "exemple_segment", relationship = "many-to-many") |>
        dplyr::filter(!is.na(CELL_ID)) |>
        dplyr::mutate(type_actif = "Lignes distribution MT", poids = as.numeric(longueur) * fraction) |>
        dplyr::select(type_actif, CELL_ID, score_raster, poids)
    }
  } else warning("Lignes_distribution parquet not found for variant: ", variante)

  dplyr::bind_rows(resultats) |>
    dplyr::mutate(score_raster = tidyr::replace_na(score_raster, -1L))
}

agreger_actifs <- function(tous_actifs) {
  tous_actifs |>
    dplyr::group_by(CELL_ID, score_raster, type_actif) |>
    dplyr::summarise(nb_actifs = round(sum(poids)), .groups = "drop")
}

charger_actifs_parquet <- function(variante) {
  purrr::imap(ANNEES_REP, function(annee, horizon) {
    cat(sprintf("[%s]   Horizon %s (rep. year %d) — %s\n",
                format(Sys.time(), "%H:%M:%S"), horizon, annee, variante))
    agreger_actifs(charger_actifs_parquet_annee(variante, annee))
  })
}

joindre_actifs_df <- function(prob_scen, actifs_df) {
  prob_scen |>
    dplyr::select(SCEN, CELL_ID, YR, score_raster, P_feu_original, facteur, P_ajustee) |>
    dplyr::left_join(actifs_df, by = c("CELL_ID", "score_raster"), relationship = "many-to-many") |>
    dplyr::filter(!is.na(type_actif), !is.na(nb_actifs)) |>
    dplyr::mutate(esperance_perte = round(nb_actifs * P_ajustee, 6))
}

joindre_actifs_horizons <- function(prob_scen, actifs_horizons) {
  purrr::map_dfr(names(actifs_horizons), function(h) {
    bornes <- as.integer(strsplit(h, "-")[[1]])
    prob_h <- dplyr::filter(prob_scen, YR >= bornes[1], YR <= bornes[2])
    if (nrow(prob_h) == 0) return(NULL)
    joindre_actifs_df(prob_h, actifs_horizons[[h]])
  })
}


## ── B5. Geometry cache ─────────────────────────────

chemin_ckpt_lignes    <- file.path(wd_sortie04, "ckpt_cache_geom_lignes.rds")
chemin_ckpt_ponctuels <- file.path(wd_sortie04, "ckpt_cache_geom_ponctuels.parquet")
chemin_ckpt_lookup    <- file.path(wd_sortie04, "ckpt_cache_geom_lookup.parquet")

if (file.exists(chemin_ckpt_lignes)) {
  cat(sprintf("[%s] [CHECKPOINT] cache_geom_lignes loaded\n", format(Sys.time(), "%H:%M:%S")))
  cache_geom_lignes <- readRDS(chemin_ckpt_lignes)
} else {
  cat(sprintf("[%s] Building geometry cache — MT lines...\n", format(Sys.time(), "%H:%M:%S")))
  f_lig_ref <- trouver_parquet("Lignes_distribution", "BAU_SSP2")
  if (is.null(f_lig_ref)) stop("Line cache: Lignes_distribution BAU_SSP2 not found")
  raw_ref  <- arrow::open_dataset(f_lig_ref) |>
    dplyr::filter(reseau == "moyenne tension", year == 2022L) |>
    dplyr::select(exemple_segment) |> dplyr::collect()
  pts_ref2 <- construire_cache_lignes(raw_ref) |> vers_crs_raster()
  vpts_ref <- terra::vect(pts_ref2)
  cell_ref <- terra::extract(r_extraction_cell,  vpts_ref)[["CELL_ID"]]
  scor_ref <- terra::extract(r_extraction_score, vpts_ref)[["score_raster"]]
  rm(vpts_ref); gc()
  cache_geom_lignes <- data.frame(
    exemple_segment = pts_ref2$exemple_segment,
    fraction        = pts_ref2$fraction,
    CELL_ID         = as.integer(cell_ref),
    score_raster    = as.integer(scor_ref)
  ) |> dplyr::filter(!is.na(CELL_ID))
  saveRDS(cache_geom_lignes, chemin_ckpt_lignes)
  cat(sprintf("[%s] [CHECKPOINT] cache_geom_lignes → %s (%d rows)\n",
              format(Sys.time(), "%H:%M:%S"), basename(chemin_ckpt_lignes), nrow(cache_geom_lignes)))
  rm(raw_ref, pts_ref2, cell_ref, scor_ref); gc()
}

if (file.exists(chemin_ckpt_ponctuels) && file.exists(chemin_ckpt_lookup)) {
  cat(sprintf("[%s] [CHECKPOINT] cache_geom_ponctuels loaded\n", format(Sys.time(), "%H:%M:%S")))
  cache_geom_ponctuels <- arrow::read_parquet(chemin_ckpt_ponctuels)
  geoms_lookup         <- arrow::read_parquet(chemin_ckpt_lookup)
} else {
  cat(sprintf("[%s] Building geometry cache — point assets...\n", format(Sys.time(), "%H:%M:%S")))

  lire_ponc_ref <- function(type, filtre_fn = NULL) {
    f <- trouver_parquet(type, "BAU_SSP2")
    if (is.null(f)) return(NULL)
    raw <- arrow::open_dataset(f) |> dplyr::filter(year == 2022L) |>
      dplyr::select(dplyr::any_of(c("geometry", "count", "HQ_TYPE", "tension"))) |> dplyr::collect()
    if (!is.null(filtre_fn)) raw <- filtre_fn(raw)
    if (nrow(raw) == 0) return(NULL)
    raw
  }
  raw_ponc_all <- dplyr::bind_rows(purrr::compact(list(
    lire_ponc_ref("Poteaux"),
    lire_ponc_ref("Supports",        function(x) dplyr::filter(x, HQ_TYPE %in% c("POTEAU", "POTEAU_DIS"))),
    lire_ponc_ref("Transformateurs", function(x) dplyr::filter(x, tension %in% names(TENSION_KV_TYPE))),
    lire_ponc_ref("Postes")
  ))) |> dplyr::distinct(geometry)

  geoms    <- raw_ponc_all$geometry
  pts_list <- strsplit(geoms, "_", fixed = TRUE)
  n_pts    <- vapply(pts_list, function(v) sum(nzchar(trimws(v))), integer(1))
  tous_pts <- unlist(pts_list, use.names = FALSE)
  tous_pts <- tous_pts[nzchar(trimws(tous_pts))]
  coord_m  <- regmatches(tous_pts,
    regexpr("\\((-?[0-9]+\\.?[0-9]*)\\s+(-?[0-9]+\\.?[0-9]*)", tous_pts))
  parts    <- strsplit(sub("^\\(", "", coord_m), "\\s+", fixed = FALSE)
  lon      <- as.numeric(vapply(parts, function(x) x[[1]], character(1)))
  lat      <- as.numeric(vapply(parts, function(x) x[[2]], character(1)))
  geom_idx <- rep(seq_along(geoms), n_pts)
  fraction <- rep(1 / pmax(n_pts, 1L), n_pts)
  valide     <- !is.na(lon) & !is.na(lat)
  lon_v      <- lon[valide]; lat_v <- lat[valide]
  geom_idx_v <- geom_idx[valide]; fraction_v <- fraction[valide]
  n_total    <- length(lon_v)
  rm(lon, lat, geom_idx, fraction, tous_pts, parts, coord_m, pts_list); gc()

  crs_raster_wkt <- terra::crs(r_extraction_cell)
  chunk_size <- 30000L
  cell_ponc  <- integer(n_total)
  score_ponc <- integer(n_total)
  for (i_start in seq(1L, n_total, by = chunk_size)) {
    i_end   <- min(i_start + chunk_size - 1L, n_total)
    v_chunk <- terra::project(
      terra::vect(cbind(lon_v[i_start:i_end], lat_v[i_start:i_end]), crs = "EPSG:4326"),
      crs_raster_wkt
    )
    cell_ponc[i_start:i_end]  <- terra::extract(r_extraction_cell,  v_chunk)[[2]]
    score_ponc[i_start:i_end] <- terra::extract(r_extraction_score, v_chunk)[[2]]
    rm(v_chunk); gc()
  }

  cache_geom_ponctuels <- data.frame(
    geom_idx = geom_idx_v, fraction = fraction_v,
    CELL_ID = as.integer(cell_ponc), score_raster = as.integer(score_ponc)
  ) |> dplyr::filter(!is.na(CELL_ID))
  geoms_lookup <- data.frame(geom_idx = seq_along(geoms), geometry_key = geoms)
  arrow::write_parquet(cache_geom_ponctuels, chemin_ckpt_ponctuels)
  arrow::write_parquet(geoms_lookup, chemin_ckpt_lookup)
  cat(sprintf("[%s] [CHECKPOINT] point cache → %d rows | lookup → %d geometries\n",
              format(Sys.time(), "%H:%M:%S"), nrow(cache_geom_ponctuels), nrow(geoms_lookup)))
  rm(raw_ponc_all, lon_v, lat_v, geom_idx_v, fraction_v, cell_ponc, score_ponc); gc()
}


## ── B6. Pre-compute assets by inventory ──────────────

actifs_agregat_list <- list()

BESOIN_STABLE <- "stable" %in% INVENTAIRES_A_TRAITER ||
                 any(c("BAU_couple", "EE_couple") %in% INVENTAIRES_A_TRAITER)

df_stable_agregat <- NULL
if (BESOIN_STABLE) {
  cat(sprintf("\n[%s] Preparing assets — stable\n", format(Sys.time(), "%H:%M:%S")))
  df_stable_agregat <- agreger_actifs(charger_actifs_stable())
  if (TEST_MODE && !is.null(df_stable_agregat))
    df_stable_agregat <- dplyr::filter(df_stable_agregat,
                                        type_actif == "Postes de transformation (nombre)")
}

if ("stable" %in% INVENTAIRES_A_TRAITER)
  actifs_agregat_list[["stable"]] <- df_stable_agregat

for (inv in setdiff(INVENTAIRES_A_TRAITER, "stable")) {
  cat(sprintf("\n[%s] Preparing assets — %s\n", format(Sys.time(), "%H:%M:%S"), inv))
  if (!inv %in% c("BAU_couple", "EE_couple")) stop("Unknown inventory: ", inv)

  prefixe          <- sub("_couple$", "", inv)
  couplage_parquet <- get(paste0("COUPLAGE_", prefixe, "_PARQUET"))
  actifs_par_scen  <- list(Baseline = df_stable_agregat)

  for (scen in names(couplage_parquet))
    actifs_par_scen[[scen]] <- charger_actifs_parquet(couplage_parquet[[scen]])

  actifs_agregat_list[[inv]] <- actifs_par_scen
}


## ── B7. Model × inventory loop ──────────────────

for (modele in MODELES_A_TRAITER) {

  cat(sprintf("\n[%s] ── Model: %s ──\n", format(Sys.time(), "%H:%M:%S"), modele))

  fichiers_prob <- list.files(
    wd_sortie03,
    pattern    = sprintf("^prob_feux_boulanger_%s_.*\\.csv$", modele),
    full.names = TRUE
  )
  if (length(fichiers_prob) == 0) {
    warning("No probability file found for model ", modele, " — skipped.")
    next
  }

  probabilites <- purrr::map_dfr(fichiers_prob, read_csv, show_col_types = FALSE) |>
    dplyr::select(CELL_ID, SCEN, YR, P_feu)

  if (TEST_MODE)
    probabilites <- dplyr::filter(probabilites, SCEN %in% c("Baseline", "RCP45"))

  prob_ajustees <- probabilites |>
    dplyr::left_join(cells_avec_raster, by = "CELL_ID") |>
    dplyr::mutate(a_couverture = tidyr::replace_na(a_couverture, FALSE)) |>
    dplyr::left_join(
      facteurs |> dplyr::select(CELL_ID, score_raster, nb_cellules, facteur),
      by = "CELL_ID", relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      P_ajustee = dplyr::case_when(a_couverture ~ P_feu * facteur, TRUE ~ P_feu),
      P_ajustee = round(P_ajustee, 8)
    ) |>
    dplyr::select(SCEN, CELL_ID, YR, score_raster, nb_cellules,
                  P_feu_original = P_feu, facteur, P_ajustee, a_couverture) |>
    dplyr::arrange(SCEN, CELL_ID, YR, score_raster)

  for (inv in INVENTAIRES_A_TRAITER) {

    chemin_out <- file.path(wd_sortie04,
                            sprintf("resultats_feux_actifs_%s_%s.csv", modele, inv))
    forcer_ce_inv <- FORCER_RECALCUL &&
                     (length(INVENTAIRES_FORCER) == 0 || inv %in% INVENTAIRES_FORCER)

    if (file.exists(chemin_out) && !forcer_ce_inv) {
      cat(sprintf("[%s]   [SKIP] %s\n", format(Sys.time(), "%H:%M:%S"), basename(chemin_out)))
      next
    }

    cat(sprintf("[%s]   Inventory: %s\n", format(Sys.time(), "%H:%M:%S"), inv))

    if (inv == "stable") {
      resultats <- joindre_actifs_df(prob_ajustees, actifs_agregat_list[["stable"]])
    } else {
      actifs_scen <- actifs_agregat_list[[inv]]
      resultats <- purrr::map_dfr(unique(prob_ajustees$SCEN), function(scen) {
        prob_s <- dplyr::filter(prob_ajustees, SCEN == scen)
        actifs <- actifs_scen[[scen]]
        if (is.data.frame(actifs)) joindre_actifs_df(prob_s, actifs)
        else joindre_actifs_horizons(prob_s, actifs)
      })
    }

    resultats <- dplyr::arrange(resultats, SCEN, CELL_ID, YR, score_raster, type_actif)
    write_csv(resultats, chemin_out)
    cat(sprintf("[%s]   → %s (%d rows)\n",
                format(Sys.time(), "%H:%M:%S"), basename(chemin_out), nrow(resultats)))
    rm(resultats); gc()
  }

  rm(probabilites, prob_ajustees); gc()
}

duree <- round(as.numeric(difftime(Sys.time(), t_debut, units = "secs")))
cat(sprintf("\n[%s] ══ Done — total duration: %ds ══\n",
            format(Sys.time(), "%H:%M:%S"), duree))
cat(sprintf("    Prob     → %s\n", wd_sortie03))
cat(sprintf("    Redistrib → %s\n", wd_sortie04))
