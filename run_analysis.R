library(knitr)

dir.create("results", showWarnings = FALSE)
dir.create("cache",   showWarnings = FALSE)

knit_hooks$set(gc = function(before, options, envir) {
  if (!before) {
    gc(full = TRUE)
    Sys.sleep(0.5)
  }
})

opts_chunk$set(
  gc         = TRUE,
  cache      = TRUE,
  cache.path = "cache/"
)

knit("group32_part1.Rmd", output = "group32_part1.md")

renames <- c(
  "appendix-conditions-1.png"  = "appendix_condition1_fitted.png",
  "appendix-pairs-1.png"       = "appendix_condition2_pairs.png",
  "hist-predictors-1.png"      = "s02-1_num_pred_histograms.png",
  "box-predictors-1.png"       = "s02-1_num_pred_boxplots.png",
  "bar-predictors-1.png"       = "s02-2_cat_pred_barplots.png",
  "sbp-age-lm-1.png"           = "s03_sbp_vs_age_gender_linear.png",
  "sbp-age-loess-1.png"        = "s03_sbp_vs_age_gender_loess.png",
  "fig-response-1.png"         = "s04-2_cleaned_sbp.png",
  "fig-boxplots-1.png"         = "s04-3_cleaned_pred_boxplots.png",
  "fig-age-gender-1.png"       = "s04-4_cleaned_age_gender.png",
  "conditions-1.png"           = "s07-1_conditions.png",
  "pairs-plot-1.png"           = "s07-1_pairs_plot.png",
  "resid-fitted-qq-1.png"      = "s07-2_resid_fitted_qq.png",
  "resid-predictors-num-1.png" = "s07-2_resid_num_predictors.png",
  "resid-predictors-cat-1.png" = "s07-2_resid_cat_predictors.png",
  "resid-quad-1.png"           = "s08_resid_quad_age.png",
  "boxcox-response-1.png"      = "s09-1_boxcox_profile.png",
  "bc-resid-qq-1.png"          = "s09-1_transformed_resid_qq.png",
  "bc-resid-num-1.png"         = "s09-1_transformed_resid_num.png",
  "bc-resid-cat-1.png"         = "s09-1_transformed_resid_cat.png",
  "boxcox-compare-1.png"       = "s09-3_model_comparison_resid.png",
  "boxcox-qq-1.png"            = "s09-3_model_comparison_qq.png",
  "leverage-1.png"             = "s10-1_leverage.png",
  "outliers-1.png"             = "s10-2_outliers.png",
  "influence-1.png"            = "s10-3_influence_cooks_dfbetas.png",
  "refit-plots-1.png"          = "s10-4_refit_sensitivity.png",
  "interaction-plot-1.png"     = "s11-2_interaction_age_gender.png",
  "age2-curves-1.png"          = "s11-4_age2_fitted_curves.png",
  "age2-gap-1.png"             = "s11-4_age2_gender_gap.png",
  "final-resid-main-1.png"     = "s11-6_final_resid_qq.png",
  "final-resid-pred-1.png"     = "s11-6_final_resid_predictors.png",
  "final-compare-1.png"        = "s11-6_model_comparison_resid.png"
)

for (old in names(renames)) {
  src <- file.path("results", old)
  dst <- file.path("results", renames[[old]])
  if (file.exists(src)) file.rename(src, dst)
}

md <- readLines("group32_part1.md", warn = FALSE)
for (old in names(renames)) {
  md <- gsub(old, renames[[old]], md, fixed = TRUE)
}
writeLines(md, "group32_part1.md")

cat("\nDone. Outputs saved to results/\n")
