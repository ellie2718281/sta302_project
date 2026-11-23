library(knitr)

# free memory after every chunk
knit_hooks$set(gc = function(before, options, envir) {
  if (!before) gc()
})
opts_chunk$set(gc = TRUE)

# knit to markdown only — runs all R code, saves all plots/CSVs to results/, skips LaTeX
knit("group32_part1.Rmd", output = "group32_part1.md")

# rename plots from chunk-based names to section-based descriptive names
renames <- c(
  # Section 2 – Response Variable
  "sbp-plots-1.png"            = "s02_sbp_distribution.png",
  # Section 3.1 – Numeric Predictors
  "hist-predictors-1.png"      = "s03-1_num_pred_histograms.png",
  "box-predictors-1.png"       = "s03-1_num_pred_boxplots.png",
  # Section 3.2 – Categorical Predictors
  "bar-predictors-1.png"       = "s03-2_cat_pred_barplots.png",
  # Section 4 – Research Question
  "sbp-age-lm-1.png"           = "s04_sbp_vs_age_gender_linear.png",
  "sbp-age-loess-1.png"        = "s04_sbp_vs_age_gender_loess.png",
  # Section 5.2 – Cleaned response
  "fig-response-1.png"         = "s05-2_cleaned_sbp.png",
  # Section 5.3 – Cleaned predictors
  "fig-boxplots-1.png"         = "s05-3_cleaned_pred_boxplots.png",
  # Section 5.4 – Cleaned age x gender
  "fig-age-gender-1.png"       = "s05-4_cleaned_age_gender.png",
  # Section 8.1 – Conditions
  "conditions-1.png"           = "s08-1_conditions.png",
  "pairs-plot-1.png"           = "s08-1_pairs_plot.png",
  # Section 8.2 – Residual plots (preliminary model)
  "resid-fitted-qq-1.png"      = "s08-2_resid_fitted_qq.png",
  "resid-predictors-num-1.png" = "s08-2_resid_num_predictors.png",
  "resid-predictors-cat-1.png" = "s08-2_resid_cat_predictors.png",
  # Section 9 – Non-linearity in Age
  "resid-quad-1.png"           = "s09_resid_quad_age.png",
  # Section 10.1 – Response Transformation
  "boxcox-response-1.png"      = "s10-1_boxcox_profile.png",
  "bc-resid-qq-1.png"          = "s10-1_transformed_resid_qq.png",
  "bc-resid-num-1.png"         = "s10-1_transformed_resid_num.png",
  "bc-resid-cat-1.png"         = "s10-1_transformed_resid_cat.png",
  # Section 10.3 – Model Comparison
  "boxcox-compare-1.png"       = "s10-3_model_comparison_resid.png",
  "boxcox-qq-1.png"            = "s10-3_model_comparison_qq.png",
  # Section 11.1 – Leverage
  "leverage-1.png"             = "s11-1_leverage.png",
  # Section 11.2 – Outliers
  "outliers-1.png"             = "s11-2_outliers.png",
  # Section 11.3 – Influence
  "influence-1.png"            = "s11-3_influence_cooks_dfbetas.png",
  # Section 11.4 – Refit sensitivity
  "refit-plots-1.png"          = "s11-4_refit_sensitivity.png",
  # Section 12.2 – Interaction retest
  "interaction-plot-1.png"     = "s12-2_interaction_age_gender.png",
  # Section 12.4 – Age^2 curvature
  "age2-curves-1.png"          = "s12-4_age2_fitted_curves.png",
  "age2-gap-1.png"             = "s12-4_age2_gender_gap.png",
  # Section 12.6 – Final residual plots
  "final-resid-main-1.png"     = "s12-6_final_resid_qq.png",
  "final-resid-pred-1.png"     = "s12-6_final_resid_predictors.png",
  "final-compare-1.png"        = "s12-6_model_comparison_resid.png",
  # Appendix
  "appendix-conditions-1.png"  = "appendix_condition1_fitted.png",
  "appendix-pairs-1.png"       = "appendix_condition2_pairs.png"
)

for (old in names(renames)) {
  src <- file.path("results", old)
  dst <- file.path("results", renames[[old]])
  if (file.exists(src)) file.rename(src, dst)
}

cat("\nDone. Outputs saved to results/\n")
