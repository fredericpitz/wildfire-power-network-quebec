### ======================================================
### 03_Stats.R
### Descriptive statistics for FireCycle Boulanger-RCP rasters
###
### Outputs in Sortie/03_stats/   (wd_sortie10) :
###   table1_stats_globales.csv
###     → by GCM × scenario × horizon : mean, median, min, max, %<100yrs, %<500yrs
###   table2_stats_par_region.csv
###     → same stats broken down by administrative region (regio_s.shp)
### ======================================================

if (!exists("wd")) source(here::here("Code", "00_Main.R"))

library(terra)
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)

sf::sf_use_s2(FALSE)

wd_sortie10 <- file.path(wd_sortie, "03_stats")
dir.create(wd_sortie10, recursive = TRUE, showWarnings = FALSE)

wd_boulanger <- file.path(wd_data, "Boulanger_RCP")

## ── Quebec spatial mask ────────────────────────────────────────────────────

cat("[03_Stats] Loading Quebec spatial mask...\n")

chemin_zones <- file.path(wd_sortie01, "zones_propagation.gpkg")
if (!file.exists(chemin_zones))
  stop("Not found: ", chemin_zones, "\n  → Source Code/01_Prob_ff.R first")

masque_vect <- sf::st_read(chemin_zones, quiet = TRUE) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_transform(4326) |>
  terra::vect()

## ── Reference grid → pixel → region lookup ────────────────────────────────

cat("[03_Stats] Building pixel → administrative region lookup...\n")

r_baseline <- terra::rast(
  file.path(wd_boulanger, "FireCycle_Baseline_WGS1984_0.25.tif")
)
r_baseline <- terra::mask(terra::crop(r_baseline, masque_vect), masque_vect)

pixels_ref <- terra::as.data.frame(r_baseline, xy = TRUE, na.rm = TRUE) |>
  dplyr::select(lon = x, lat = y) |>
  dplyr::mutate(pixel_id = seq_len(dplyr::n())) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

regions_sf <- sf::st_read(
  file.path(wd_data, "Zones", "regio_s.shp"), quiet = TRUE
) |>
  sf::st_transform(4326) |>
  dplyr::select(region = RES_NM_REG)

lookup_region <- sf::st_join(pixels_ref, regions_sf, join = sf::st_within) |>
  sf::st_drop_geometry() |>
  dplyr::select(pixel_id, lon, lat, region)

n_ass <- sum(!is.na(lookup_region$region))
cat(sprintf("[03_Stats] %d / %d pixels assigned to a region\n",
            n_ass, nrow(lookup_region)))

## ── Metadata for the 19 rasters ───────────────────────────────────────────

rasters_meta <- dplyr::bind_rows(
  tibble::tibble(
    gcm      = "Baseline",
    scenario = "Baseline",
    horizon  = "1981-2010",
    fichier  = "FireCycle_Baseline_WGS1984_0.25.tif"
  ),
  tidyr::expand_grid(
    gcm      = c("CanESM2", "Hadley", "MIROC"),
    scenario = c("RCP45", "RCP85"),
    horizon  = c("2011-2040", "2041-2070", "2071-2100")
  ) |>
    dplyr::mutate(
      fichier = sprintf("FireCycle_%s_%s_%s_WGS1984_0.25.tif",
                        gcm, scenario, horizon)
    )
) |>
  dplyr::mutate(
    chemin = file.path(wd_boulanger, fichier),
    existe = file.exists(chemin)
  )

manquants <- dplyr::filter(rasters_meta, !existe)
if (nrow(manquants) > 0) {
  warning(nrow(manquants), " raster(s) not found:\n",
          paste(" -", manquants$fichier, collapse = "\n"))
}
rasters_meta <- dplyr::filter(rasters_meta, existe)
cat(sprintf("[03_Stats] %d rasters to process\n", nrow(rasters_meta)))

## ── Value extraction per raster ───────────────────────────────────────────

cat("[03_Stats] Extracting values (crop + mask per raster)...\n")

donnees_brutes <- purrr::map_dfr(seq_len(nrow(rasters_meta)), function(i) {
  m <- rasters_meta[i, ]
  cat(sprintf("  [%d/%d] %s\n", i, nrow(rasters_meta), m$fichier))

  r <- terra::rast(m$chemin)
  r <- terra::mask(terra::crop(r, masque_vect), masque_vect)

  df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df) <- c("lon", "lat", "fire_cycle")

  df |>
    dplyr::filter(!is.na(fire_cycle), fire_cycle > 0) |>
    dplyr::mutate(gcm = m$gcm, scenario = m$scenario, horizon = m$horizon)
})

## Join with region lookup
donnees_brutes <- donnees_brutes |>
  dplyr::left_join(
    dplyr::select(lookup_region, lon, lat, region),
    by = c("lon", "lat")
  )

cat(sprintf("[03_Stats] Total: %d pixel-rasters extracted\n", nrow(donnees_brutes)))

## ── Statistics function ────────────────────────────────────────────────────

stats_firecycle <- function(df) {
  df |>
    dplyr::summarise(
      n_pixels      = dplyr::n(),
      moy_cycle_ans = round(mean(fire_cycle,                        na.rm = TRUE), 1),
      med_cycle_ans = round(median(fire_cycle,                      na.rm = TRUE), 1),
      sd_cycle_ans  = round(sd(fire_cycle,                          na.rm = TRUE), 1),
      min_cycle_ans = round(min(fire_cycle,                         na.rm = TRUE), 1),
      p10           = round(unname(quantile(fire_cycle, 0.10,       na.rm = TRUE)), 1),
      p25           = round(unname(quantile(fire_cycle, 0.25,       na.rm = TRUE)), 1),
      p75           = round(unname(quantile(fire_cycle, 0.75,       na.rm = TRUE)), 1),
      p90           = round(unname(quantile(fire_cycle, 0.90,       na.rm = TRUE)), 1),
      max_cycle_ans = round(max(fire_cycle,                         na.rm = TRUE), 1),
      pct_inf100    = round(mean(fire_cycle < 100,                  na.rm = TRUE) * 100, 2),
      pct_inf500    = round(mean(fire_cycle < 500,                  na.rm = TRUE) * 100, 2),
      pct_sup5000   = round(mean(fire_cycle > 5000,                 na.rm = TRUE) * 100, 2),
      .groups = "drop"
    )
}

## ── TABLE 1 : global stats by GCM × scenario × horizon ────────────────────

cat("[03_Stats] Building Table 1 (global stats)...\n")

table1 <- donnees_brutes |>
  dplyr::group_by(gcm, scenario, horizon) |>
  stats_firecycle() |>
  dplyr::arrange(scenario, gcm, horizon)

chemin_t1 <- file.path(wd_sortie10, "table1_stats_globales.csv")
readr::write_csv(table1, chemin_t1)
cat(sprintf("[03_Stats]   → %s  (%d lignes)\n", basename(chemin_t1), nrow(table1)))

## ── TABLE 2 : stats by administrative region ──────────────────────────────

cat("[03_Stats] Building Table 2 (by administrative region)...\n")

table2 <- donnees_brutes |>
  dplyr::filter(!is.na(region)) |>
  dplyr::group_by(gcm, scenario, horizon, region) |>
  stats_firecycle() |>
  dplyr::arrange(scenario, gcm, horizon, region)

chemin_t2 <- file.path(wd_sortie10, "table2_stats_par_region.csv")
readr::write_csv(table2, chemin_t2)
cat(sprintf("[03_Stats]   → %s  (%d lignes)\n", basename(chemin_t2), nrow(table2)))

## ── Console preview ────────────────────────────────────────────────────────


## ── BASELINE SECTION : detailed Quebec stats ──────────────────────────────

cat("\n[03_Stats] Baseline section — detailed Quebec statistics...\n")

baseline_px <- donnees_brutes |> dplyr::filter(gcm == "Baseline")

stats_baseline_qc <- baseline_px |>
  stats_firecycle() |>
  dplyr::mutate(gcm = "Baseline", scenario = "Baseline", horizon = "1981-2010",
                .before = 1)

chemin_t3 <- file.path(wd_sortie10, "table3_baseline_stats_QC.csv")
readr::write_csv(stats_baseline_qc, chemin_t3)
cat(sprintf("[03_Stats]   → %s\n", basename(chemin_t3)))


## ── EXTREMES SECTION : pixels > 5000 years ────────────────────────────────

cat("\n[03_Stats] Extremes section (fire_cycle > 5000 years)...\n")

n_total    <- nrow(baseline_px)
extremes   <- baseline_px |> dplyr::filter(fire_cycle > 5000)
n_extremes <- nrow(extremes)
cat(sprintf("[03_Stats]   Pixels > 5000 years: %d / %d (%.2f%%)\n",
            n_extremes, n_total, n_extremes / n_total * 100))

extremes_par_region <- extremes |>
  dplyr::group_by(region) |>
  dplyr::summarise(
    n_pixels_extremes  = dplyr::n(),
    pct_des_extremes   = round(dplyr::n() / n_extremes * 100, 1),
    lat_min            = round(min(lat),          2),
    lat_max            = round(max(lat),          2),
    lon_min            = round(min(lon),          2),
    lon_max            = round(max(lon),          2),
    fire_cycle_moy     = round(mean(fire_cycle),  0),
    fire_cycle_max     = round(max(fire_cycle),   0),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(n_pixels_extremes))

chemin_t4 <- file.path(wd_sortie10, "table4_extremes_par_region.csv")
readr::write_csv(extremes_par_region, chemin_t4)
cat(sprintf("[03_Stats]   → %s\n", basename(chemin_t4)))



## ── HISTOGRAM SECTION ─────────────────────────────────────────────────────

cat("\n[03_Stats] Histogram section...\n")

theme_ff <- theme_bw(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "#ECEFF1"),
    strip.text       = element_text(face = "bold", size = 10)
  )

bins_breaks <- c(0, 100, 200, 500, 1000, 2000, 5000, Inf)
bins_labels <- c("0–100", "100–200", "200–500", "500–1 000",
                 "1 000–2 000", "2 000–5 000", "5 000+")

## ── Function: % per bin for a subset of pixels ───────────────────────────
pct_par_bin <- function(df) {
  df |>
    dplyr::mutate(bin = cut(fire_cycle, breaks = bins_breaks,
                            labels = bins_labels,
                            right = TRUE, include.lowest = TRUE)) |>
    dplyr::count(bin) |>
    tidyr::complete(bin = factor(bins_labels, levels = bins_labels),
                    fill = list(n = 0L)) |>
    dplyr::mutate(pct = n / sum(n) * 100)
}

## ── Histogram 1 : Quebec Baseline ────────────────────────────────────────

hist_baseline <- pct_par_bin(baseline_px) |>
  dplyr::mutate(scenario_lbl = "Baseline (1981–2010)")

## ── Histogram 2 : Comparison Baseline vs. RCP8.5 2071–2100 ──────────────

## RCP8.5 2071-2100 : inter-model average of % per bin
hist_rcp85 <- donnees_brutes |>
  dplyr::filter(scenario == "RCP85", horizon == "2071-2100") |>
  dplyr::group_by(gcm) |>
  dplyr::group_modify(~ pct_par_bin(.x)) |>
  dplyr::ungroup() |>
  dplyr::group_by(bin) |>
  dplyr::summarise(pct = mean(pct), n = round(mean(n)), .groups = "drop") |>
  dplyr::mutate(scenario_lbl = "RCP8.5 (2071–2100)")

comp_data <- dplyr::bind_rows(
  dplyr::select(hist_baseline, bin, pct, scenario_lbl),
  dplyr::select(hist_rcp85,    bin, pct, scenario_lbl)
) |>
  dplyr::mutate(
    scenario_lbl = factor(scenario_lbl,
                          levels = c("Baseline (1981–2010)", "RCP8.5 (2071–2100)"))
  )

PAL_COMP <- c("Baseline (1981–2010)" = "#1565C0", "RCP8.5 (2071–2100)" = "#C62828")

g_comp <- ggplot(comp_data, aes(x = bin, y = pct, fill = scenario_lbl)) +
  geom_col(position = position_dodge(0.82), width = 0.75, alpha = 0.85) +
  scale_fill_manual(values = PAL_COMP, name = "Scenario") +
  scale_y_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Fire Cycle Duration Distribution — Quebec: Baseline vs. RCP8.5 (2071–2100)",
    x     = "Fire cycle duration (years)",
    y     = "Share of pixels (%)"
  ) +
  theme_ff

chemin_comp <- file.path(wd_sortie10, "histogram_comparison_RCP85_2071_QC.pdf")
ggsave(chemin_comp, g_comp, width = 12, height = 6, dpi = 300, bg = "white")
cat(sprintf("[03_Stats]   → %s\n", basename(chemin_comp)))


cat(sprintf("\n[03_Stats] ══ Done. Files saved in: %s ══\n", wd_sortie10))

