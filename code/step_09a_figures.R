library(tidyverse)
library(gtsummary)
library(gt)
library(tidymodels)
library(censored)
library(paletteer)
library(survey)
library(patchwork)
save = FALSE # change to true if we want to save figs to manuscript/figures
col1 = "#CC5151"; col2 = "#422CB2" # from paletteer_d("colorBlindness::SteppedSequential5Steps")
# paletteer_d("ggthemes::colorblind") # for colorblind friendly palette
col_vec = c("#000000FF", "#009E73FF", "#CC79A7FF", "#E69F00FF", "#D55E00FF", "#56B4E9FF", "#0072B2FF")

# change to  % diff after smoothing
wt_single = readRDS(here::here("results", "metrics_wtd_100_singlevar.rds")) %>%
  group_by(variable) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(variable, rep) %>%
  summarize(concordance = mean(concordance))


# results from multivariable
wt_all = readRDS(here::here("results", "metrics_wtd_100.rds")) %>%
  group_by(model) %>%
  mutate(ind = row_number(),
         rep = floor((row_number()-1)/10)) %>%
  group_by(model, rep) %>%
  summarize(concordance = mean(concordance))

# covariate/pa df
df_all = readRDS(here::here("data", "covariates_accel_mortality_df.rds"))

df_mortality =
  df_all %>%
  filter(num_valid_days >= 3) %>%
  filter(age_in_years_at_screening >= 50 &
           age_in_years_at_screening < 80)

df_accel =
  df_all %>%
  filter(num_valid_days >= 3 & age_in_years_at_screening >= 18)

df_mortality_win =
  df_mortality %>%
  filter(if_all(.cols = c(age_in_years_at_screening, gender,
                          race_hispanic_origin, cat_education,
                          cat_bmi, chd, chf, heartattack, stroke, cancer,
                          bin_diabetes, cat_alcohol, cat_smoke, bin_mobilityproblem,
                          general_health_condition, mortstat, permth_exm, total_PAXMTSM),
                ~!is.na(.x))) %>%
  mutate(event_time = permth_exm / 12) %>%
  ungroup() %>%
  mutate(across(c(contains("total"), contains("peak")), ~DescTools::Winsorize(.x, quantile(.x, probs = c(0, 0.99)))))

df_accel_win =
  df_accel %>%
  mutate(across(c(contains("total"), contains("peak")), ~DescTools::Winsorize(.x, quantile(.x, probs = c(0, 0.99)))))


# supplemental figure: distributions
p1 = df_accel %>%
  select(contains("steps") & contains("total"), SEQN) %>%
  pivot_longer(cols = -c(SEQN)) %>%
  mutate(type = "Raw") %>%
  bind_rows(df_accel_win %>%
              select(contains("steps") & contains("total"), SEQN) %>%
              pivot_longer(cols = -c(SEQN)) %>%
              mutate(type = "Winsorized")) %>%
  mutate(name = factor(name, labels = c("Actilife steps", "ADEPT", "Oak", "Stepcount RF", "Stepcount SSL", "Verisense", "Verisense rev."))) %>%
  ggplot(aes(x = value / 1000, fill = type, col = type))+
  geom_density() +
  facet_wrap(type~name, scales = "free", nrow = 2)+
  theme_bw() +
  scale_fill_manual(values = c(col1, col2))+
  scale_color_manual(values = c(col1, col2))+
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(size = 15),
        title = element_text(size = 16)) +
  labs(x = "Mean Daily Steps x 1000", y = "Density", title = "Distribution of Mean Daily Step Counts")
p1
if(save) {
  ggsave(
    here::here("manuscript", "figures", "step_distributions.png"),
    dpi = 400,
    width = 10,
    height = 8
  )
}

p2 = df_accel %>%
  select(!contains("steps") & contains("total"), SEQN) %>%
  pivot_longer(cols = -c(SEQN)) %>%
  mutate(type = "Raw") %>%
  bind_rows(df_accel_win %>%
              select(!contains("steps") & contains("total"), SEQN) %>%
              pivot_longer(cols = -c(SEQN)) %>%
              mutate(type = "Winsorized")) %>%
  mutate(name = factor(name, labels = c("AC", "log10 AC", "MIMS", "log10 MIMS"))) %>%
  ggplot(aes(x = value / 1000, fill = type, col = type))+
  geom_density() +
  facet_wrap(type~name, scales = "free", nrow = 2)+
  theme_bw() +
  scale_fill_manual(values = c(col1, col2))+
  scale_color_manual(values = c(col1, col2))+
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(size = 15),
        title = element_text(size = 16)) +
  labs(x = "Mean Daily Value x 1000", y = "Density", title = "Distribution of Mean PA")

p2
if(save) {
  ggsave(
    here::here("manuscript", "figures", "pa_distributions.png"),
    dpi = 400,
    width = 10,
    height = 8
  )
}

p1 / p2 + plot_annotation(tag_levels = 'A')

if(save) {
  ggsave(
    here::here("manuscript", "figures", "distributions_all.png"),
    dpi = 400,
    width = 10,
    height = 8
  )
}

##### figure 1
survey_design =
  df_accel %>%
  filter(age_in_years_at_screening >= 18 & age_in_years_at_screening < 80) %>%
  mutate(weight = full_sample_2_year_mec_exam_weight / 2,
         weight_norm = weight / mean(weight)) %>%
  ungroup()  %>%
  survey::svydesign(
  id = ~ masked_variance_pseudo_psu,
  strata = ~ masked_variance_pseudo_stratum,
  weights = ~ weight_norm,
  data = .,
  nest = TRUE
)

# function to calculate mean estimates by age
calc_by_age =
  function(var, df) {
    # var = "total_oaksteps"
    formula = as.formula(paste("~", var))
    total_by_age_gender = svyby(formula,
                                ~ age_in_years_at_screening,
                                df,
                                svymean) %>%
      rename(mean = contains(var)) %>%
      mutate(metric = var)
  }

means_df =
  map_dfr(.x = df_accel %>% select(contains("total") | contains("peak")) %>% colnames(),
          .f = calc_by_age, df = survey_design)

# get smooths for means and confidence intervals
models = means_df %>%
  mutate(lb = mean - 1.96 * se,
         ub = mean + 1.96 * se) %>%
  tidyr::nest(data = -c(metric)) %>%
  dplyr::mutate(
    # Perform loess calculation on each group
    m = purrr::map(data, loess,
                   formula = mean ~ age_in_years_at_screening, span = .75),
    # Retrieve the fitted values from each model
    fitted_mean = purrr::map(m, `[[`, "fitted"),
    l = purrr::map(data, loess,
                   formula = lb ~ age_in_years_at_screening, span = .75),
    # Retrieve the fitted values from each model
    fitted_lb = purrr::map(l, `[[`, "fitted"),
    u = purrr::map(data, loess,
                   formula = ub ~ age_in_years_at_screening, span = .75),
    # Retrieve the fitted values from each model
    fitted_ub = purrr::map(u, `[[`, "fitted")
  )

# Apply fitted y's as a new column
results = models %>%
  dplyr::select(-m, -l, -u) %>%
  tidyr::unnest(cols = c(data, contains("fitted")))


# get some means for the text of the manuscript
results %>%
  filter(age_in_years_at_screening %in% c(40, 79) & grepl("step", metric) & !grepl("peak", metric)) %>%
  select(metric, mean, fitted_mean, age = age_in_years_at_screening) %>%
  pivot_wider(names_from = age, values_from = c(mean, fitted_mean))

# name color vector as desired
names(col_vec) = c("Actilife", "ADEPT", "Oak", "Stepcount RF","Stepcount SSL", "Verisense", "Verisense rev.")
# panel A
p1 = results %>%
  filter(grepl("step", metric) & grepl("total", metric)) %>%
  mutate(across(contains("fitted"), ~.x / 1000),
         metric = factor(metric, levels = c("total_actisteps",
                                            "total_adeptsteps",
                                            "total_oaksteps",
                                            "total_scrfsteps",
                                            "total_scsslsteps",
                                            "total_vssteps",
                                            "total_vsrevsteps"),
                         labels = c("Actilife", "ADEPT", "Oak", "Stepcount RF", "Stepcount SSL",
                                    "Verisense", "Verisense rev."))) %>%
  ggplot(aes(x = age_in_years_at_screening, y = fitted_mean,
             ymin = fitted_lb, ymax = fitted_ub, color = metric, fill = metric)) +
  facet_grid(. ~ metric) +
  geom_line(linewidth = 1) +
  geom_ribbon(alpha = .2, linetype = 0) +
  scale_fill_manual(values = col_vec, name = "Algorithm") +
  theme_light() +
  scale_color_manual(values = col_vec, name = "Algorithm") +
  geom_hline(aes(yintercept = 10), col = "darkgrey", linetype = "dashed") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        text =element_text(size = 14),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank()) +
  labs(x = "Age (years)", y = "Mean Daily Steps x 1000",
       title = "Smoothed survey weighted mean daily steps by age ")+
  scale_y_continuous(breaks=seq(0,16,1))+
  scale_x_continuous(breaks=seq(20,80,10))

p1


p2 = results %>%
  filter(grepl("peak", metric) & grepl("step", metric)) %>%
  mutate(type = sub("_.*", "", metric),
         method = sub("^[^_]*_", "", metric),
         type = factor(type, labels = c("1-minute", "30-minute"))) %>%
  filter(grepl("step", metric)) %>%
  mutate(method = factor(method, levels = c("actisteps",
                                            "adeptsteps",
                                            "oaksteps",
                                            "scrfsteps",
                                            "scsslsteps",
                                            "vssteps",
                                            "vsrevsteps"),
                         labels = c("Actilife", "ADEPT", "Oak", "Stepcount RF", "Stepcount SSL", "Verisense", "Verisense rev."))) %>%
  ggplot(aes(x = age_in_years_at_screening, y = fitted_mean,
             ymin = fitted_lb, ymax = fitted_ub, fill = method, color = method)) +
  facet_grid(. ~ type) +
  geom_line() +
  geom_ribbon(alpha = 0.2, color = NA) +
  scale_fill_manual(values = col_vec, name = "Algorithm") +
  scale_color_manual(values = col_vec, name = "Algorithm") +
  theme_light() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text =  element_text(size = 12),
        panel.grid.minor = element_blank()) +
  labs(x = "Age (years)", y = "Peak Cadence (steps/min)",
       title = "Smoothed Survey Weighted Peak Cadence by Age")+
  scale_y_continuous(breaks=seq(20, 120,10))+
  scale_x_continuous(breaks=seq(20,80,10))
p2


p3 = means_df %>%
  filter(grepl("step", metric) & grepl("total", metric)) %>%
  mutate(metric = factor(metric, levels = c("total_actisteps",
                                            "total_adeptsteps",
                                            "total_oaksteps",
                                            "total_scrfsteps",
                                            "total_scsslsteps",
                                            "total_vssteps",
                                            "total_vsrevsteps"),
                         labels = c("Actilife", "ADEPT", "Oak", "Stepcount RF", "Stepcount SSL",
                                    "Verisense", "Verisense rev."))) %>%
  group_by(metric) %>%
  mutate(pct_chg = (mean - dplyr::lag(mean))/dplyr::lag(mean)*100) %>%
  ggplot(aes(x = age_in_years_at_screening, y = pct_chg, color = metric))+
  geom_smooth(method = "loess", se = FALSE) +
  # geom_line() +
  scale_color_manual(values = col_vec, name = "Algorithm") +
  geom_hline(aes(yintercept = 0), col = "darkgrey", linetype = "dashed", linewidth = 1.1) +
  theme_light() +
  labs(x = "Age (years)", y = "% change from previous year",
       title = "Survey weighted estimated per-year difference in mean daily steps")+
  theme(legend.position = c(.1, .4))+
  scale_x_continuous(breaks=seq(20,80,10))+
  scale_y_continuous(breaks=seq(-5, 5, 1))

label_df =
  results %>%
  filter(grepl("step", metric) & grepl("total", metric)) %>%
  mutate(metric = factor(metric, levels = c("total_actisteps",
                                            "total_adeptsteps",
                                            "total_oaksteps",
                                            "total_scrfsteps",
                                            "total_scsslsteps",
                                            "total_vssteps",
                                            "total_vsrevsteps"),
                         labels = c("Actilife", "ADEPT", "Oak", "Stepcount RF", "Stepcount SSL",
                                    "Verisense", "Verisense rev."))) %>%
  group_by(metric) %>%
  mutate(pct_chg = (fitted_mean - dplyr::lag(fitted_mean))/dplyr::lag(fitted_mean)*100)  %>%
  filter(age_in_years_at_screening == 79)
p3 = results %>%
  filter(grepl("step", metric) & grepl("total", metric)) %>%
  mutate(metric = factor(metric, levels = c("total_actisteps",
                                            "total_adeptsteps",
                                            "total_oaksteps",
                                            "total_scrfsteps",
                                            "total_scsslsteps",
                                            "total_vssteps",
                                            "total_vsrevsteps"),
                         labels = c("Actilife", "ADEPT", "Oak", "Stepcount RF", "Stepcount SSL",
                                    "Verisense", "Verisense rev."))) %>%
  group_by(metric) %>%
  mutate(pct_chg = (fitted_mean - dplyr::lag(fitted_mean))/dplyr::lag(fitted_mean)*100) %>%
  ggplot(aes(x = age_in_years_at_screening, y = pct_chg, color = metric, fill = metric))+
  geom_line(linewidth = 1) +
  geom_ribbon(alpha = .2, linetype = 0, aes(ymax = 0, ymin = 0)) +
  scale_fill_manual(values = col_vec, name = "Algorithm") +
  theme_light() +
  scale_color_manual(values = col_vec, name = "Algorithm") +
  geom_hline(aes(yintercept = 0), col = "darkgrey", linetype = "dashed", linewidth = 1.1) +
  theme_light() +
  labs(x = "Age (years)", y = "% change from previous year",
       title = "Survey weighted estimated per-year difference in mean daily steps")+
  theme(legend.position = c(0.4, 0.4),
        legend.title = element_blank(),
        text = element_text(size = 14))+
  scale_x_continuous(breaks=seq(20,80,10))+
  ggrepel::geom_text_repel(data = label_df, aes(label = metric, x = 79, y = pct_chg), size = 4, nudge_x = 1,
                            inherit.aes = FALSE, box.padding = 0.125, point.padding = 0.125, segment.color = "grey50",
                            max.overlaps = Inf)+
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2))

# export 900x500
  # scale_y_continuous(limits = c(-6, 5), breaks=seq(-5, 5, 1))

p3


p1 / (p2 + p3 ) + plot_layout(guides = "collect", nrow = 2, axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom')
p1 /  p3  + plot_layout(guides = "collect", nrow = 2, axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom')

if(save) {
  ggsave(
    here::here("manuscript", "figures", "distributions.png"),
    dpi = 400,
    width = 10,
    height = 8
  )
}


#### correlation plot
# function to get mean diff or mean absolute diff
mean_mat = function(df, abs = FALSE) {
  # Ensure the input is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }

  # Check if all columns are numeric
  if (!all(sapply(df, is.numeric))) {
    stop("All columns in the dataframe must be numeric")
  }

  # Get the number of columns
  num_cols <- ncol(df)

  # Initialize a matrix to store the mean absolute differences
  mad_matrix <- matrix(NA, nrow = num_cols, ncol = num_cols)
  rownames(mad_matrix) <- colnames(df)
  colnames(mad_matrix) <- colnames(df)

  # Calculate the mean absolute difference for each pair of columns
  for (i in 1:num_cols) {
    for (j in 1:num_cols) {
      if (i <= j) {
        # Calculate mean absolute difference, handling potential NAs
        if(abs){
          d = abs(df[, i] - df[, j])
        }
        else{
          d = df[, i] - df[, j]
        }
        # Debug output
        # print(paste("Calculating MAD for columns", colnames(df)[i], "and", colnames(df)[j]))
        # print(head(abs_diff))

        # Calculate mean
        # mad_matrix[i, j] <- mean(abs_diff %>% unlist, na.rm = TRUE)
        mad_matrix[i, j] <- mean(d %>% unlist, na.rm = TRUE)
        mad_matrix[j, i] <- mad_matrix[i, j]  # Symmetric matrix
      }
    }
  }

  return(mad_matrix)
}

# correlation matrix
cor_mat =
  df_accel %>% select(contains("total")) %>%
  select(
    contains("acti"),
    contains("adept"),
    contains("oak"),
    contains("sc"),
    contains("vss"),
    contains("vsr"),
    total_AC,
    total_PAXMTSM,
    total_log10AC,
    total_log10PAXMTSM
  ) %>%
  cor(., use = "complete", method = "spearman")

cor_mat_pearson =
  df_accel %>% select(contains("total")) %>%
  select(
    contains("acti"),
    contains("adept"),
    contains("oak"),
    contains("sc"),
    contains("vss"),
    contains("vsr"),
    total_AC,
    total_PAXMTSM,
    total_log10AC,
    total_log10PAXMTSM
  ) %>%
  cor(., use = "complete", method = "pearson")

# correlation p value matrix
pvals = df_accel %>% select(contains("total")) %>%
  select(
    contains("acti"),
    contains("adept"),
    contains("oak"),
    contains("sc"),
    contains("vss"),
    contains("vsr"),
    total_AC,
    total_PAXMTSM,
    total_log10AC,
    total_log10PAXMTSM
  ) %>%
  rstatix::cor_pmat(.) %>%
  select(-rowname) %>%
  as.matrix()


colnames(cor_mat) = rownames(cor_mat)  = colnames(cor_mat_pearson) =
  rownames(cor_mat_pearson) = colnames(pvals) = rownames(pvals) = c(
  "Actilife steps",
  "ADEPT",
  "Oak",
  "Stepcount RF",
  "Stepcount SSL",
  "Verisense",
  "Verisense rev.",
  "AC",
  "MIMS",
  "log10 AC",
  "log10 MIMS"
)

# r for box around correlation
r = c("ADEPT", "Actilife steps", "Verisense rev.", "Verisense")

corrplot::corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  col = paletteer_d("colorBlindness::Blue2Orange8Steps"),
  tl.col = "black",
  tl.srt = 30,
  p.mat = pvals,
  col.lim = c(0.2, 1),
  sig.level = 0.05,
  insig = "blank",
  is.corr = FALSE,
  addgrid.col = "white",
  diag = FALSE,
  title = "Spearman Correlations",
  addCoef.col = 'grey50'
) %>%
  corrplot::corrRect(namesMat  = r)

# r = c("Actilife steps", "ADEPT", "Verisense", "Verisense rev.")


corrplot::corrplot(
  cor_mat_pearson,
  method = "color",
  type = "upper",
  col = paletteer_d("colorBlindness::Blue2Orange8Steps"),
  tl.col = "black",
  tl.srt = 30,
  p.mat = pvals,
  col.lim = c(0.2, 1),
  sig.level = 0.05,
  insig = "blank",
  is.corr = FALSE,
  addgrid.col = "white",
  diag = FALSE,
  title = "Pearson Correlations",
  addCoef.col = 'grey50'
) %>%
  corrplot::corrRect(namesMat  = r)



# difference matrix
diff_mat =
  df_accel %>% select(contains("total")) %>%
  select(
    contains("acti"),
    contains("adept"),
    contains("oak"),
    contains("sc"),
    contains("vss"),
    contains("vsr"),
    total_AC,
    total_PAXMTSM,
    total_log10AC,
    total_log10PAXMTSM
  ) %>%
  mean_mat()
# absolute difference matrix
abs_diff_mat =
  df_accel %>% select(contains("total")) %>%
  select(
    contains("acti"),
    contains("adept"),
    contains("oak"),
    contains("sc"),
    contains("vss"),
    contains("vsr"),
    total_AC,
    total_PAXMTSM,
    total_log10AC,
    total_log10PAXMTSM
  ) %>%
  mean_mat(., abs = TRUE)

# make NA for non-step algos
diff_mat[, c("total_AC",
             "total_log10AC",
             "total_PAXMTSM",
             "total_log10PAXMTSM")] <- NA
diff_mat[c("total_AC",
           "total_log10AC",
           "total_PAXMTSM",
           "total_log10PAXMTSM"), ] <- NA
colnames(diff_mat) = rownames(diff_mat) =  c(
  "Actilife steps",
  "ADEPT",
  "Oak",
  "Stepcount RF",
  "Stepcount SSL",
  "Verisense",
  "Verisense rev.",
  "AC",
  "MIMS",
  "log10 AC",
  "log10 MIMS"
)
diff_mat = diff_mat / 1000
corrplot::corrplot(
  diff_mat,
  method = "color",
  type = "lower",
  col = paletteer_c("grDevices::Purple-Green", 10),
  col.lim = c(-10, 10),
  tl.col = "black",
  tl.srt = 30,
  insig = "blank",
  is.corr = FALSE,
  diag = FALSE,
  title = "Mean Difference in Steps (x1000)",
  addgrid.col = "white",
  # addCoef.col = 'black',
  na.label = "-"
)


abs_diff_mat[, c("total_AC",
                 "total_log10AC",
                 "total_PAXMTSM",
                 "total_log10PAXMTSM")] <- NA
abs_diff_mat[c("total_AC",
               "total_log10AC",
               "total_PAXMTSM",
               "total_log10PAXMTSM"), ] <- NA
colnames(abs_diff_mat) = rownames(abs_diff_mat) = c(
  "Actilife steps",
  "ADEPT",
  "Oak",
  "Stepcount RF",
  "Stepcount SSL",
  "Verisense",
  "Verisense rev.",
  "AC",
  "MIMS",
  "log10 AC",
  "log10 MIMS"
)
abs_diff_mat = abs_diff_mat / 1000
corrplot::corrplot(
  abs_diff_mat,
  method = "color",
  type = "lower",
  col = paletteer_c("grDevices::Purple-Green", 10, direction = -1),
  col.lim = c(0, 10),
  tl.col = "black",
  tl.srt = 30,
  insig = "blank",
  is.corr = FALSE,
  diag = FALSE,
  title = "Mean Absolute Difference",
  addgrid.col = "white",
  # addCoef.col = 'black',
  na.label = "-"
)


vars = df_accel %>%
  select(contains("total") & contains("steps")) %>%
  colnames()
vnames = c(
  "Actilife steps",
  "ADEPT",
  "Oak",
  "Stepcount RF",
  "Stepcount SSL",
  "Verisense rev.",
  "Verisense"
)

plotlist = list()
for(var1 in vars){
  for(var2 in vars){
    if(var1 != var2 & which(vars==var1) < which(vars==var2)){
      vname1 = vnames[which(vars==var1)]; vname2 = vnames[which(vars==var2)]
      p = df_accel %>%
        select(age_in_years_at_screening, var1 = all_of(var1), var2 = all_of(var2)) %>%
        rowwise() %>%
        mutate(diff = (var1 - var2)/1000,
               mean = (mean(c(var1, var2)))/1000) %>%
        ggplot(aes(x = mean, y = diff, col = age_in_years_at_screening))+
        geom_point(size = .5) +
        scale_color_viridis_c(name = "Age", option = "C") +
        geom_hline(aes(yintercept = 0), col = "grey50") +
        theme_classic() +
        theme(legend.position = "none",
              axis.title = element_blank(),
              axis.text = element_text(size = 12))+
        labs(x = paste0("Mean of ", vname1, " and ", vname2, " (x1000)"),
             y = paste0(vname1, " minus ", vname2, " (x1000)"))
      plotlist[[length(plotlist)+1]] = p
    }
  }
}

p = df_accel %>%
  select(age_in_years_at_screening, var1 = all_of(var1), var2 = all_of(var2)) %>%
  rowwise() %>%
  mutate(diff = (var1 - var2)/1000,
         mean = (mean(c(var1, var2)))/1000) %>%
  ggplot(aes(x = mean, y = diff, col = age_in_years_at_screening))+
  geom_point(size = .5) +
  scale_color_viridis_c(name = "Age", option = "C") +
  geom_hline(aes(yintercept = 0), col = "grey50") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_text(size = 12))+
  labs(x = paste0("Mean of ", vname1, " and ", vname2, " (x1000)"),
       y = paste0(vname1, " minus ", vname2, " (x1000)"))

layout <- '
A#####
BG####
CHL###
DIMP##
EJNQS#
FKORTU
'
wrap_plots(plotlist[[1]], plotlist[[2]], plotlist[[3]],
           plotlist[[4]] , plotlist[[5]] , plotlist[[6]],
  plotlist[[7]] , plotlist[[8]] , plotlist[[9]] , plotlist[[10]] , plotlist[[11]]  ,
  plotlist[[12]], plotlist[[13]] , plotlist[[14]] , plotlist[[15]]  ,
  plotlist[[16]] , plotlist[[17]] , plotlist[[18]]  ,
 plotlist[[19]] , plotlist[[20]], plotlist[[21]]) + plot_layout(design = layout)

if (save) {
  ggsave(
    here::here("manuscript", "figures", "bland_altman.png"),
    dpi = 400,
    width = 14,
    height = 14
  )
}

df_accel %>%
  rowwise() %>%
  mutate(diff = total_scrfsteps - total_vssteps,
         mean = mean(c(total_scrfsteps, total_vssteps))) %>%
  ggplot(aes(x = mean, y = diff, col = age_in_years_at_screening))+
  geom_point() +
  scale_color_viridis_c() +
  geom_hline(aes(yintercept = 0))
### concordance figure, single variable
# var labels df
var_labels =
  tibble(names =
           unique(wt_single$variable),
         labels = c("Age at screening", "Diabetes", "Mobility problem",
                    "Cancer", "Alcohol use",
                    "BMI Category", "Education level", "Smoking status",
                    "CHF", "CHD", "Gender", "Self-reported health", "Heart attack", "Peak1 AC", "Peak1 MIMS", "Peak1 Actilife steps",
                    "Peak1 ADEPT steps", "Peak1 log10 AC", "Peak1 log10 MIMS", "Peak1 Oak steps",
                    "Peak1 Stepcount RF steps", "Peak1 Stepcount SSL steps", "Peak1 Verisense rev. steps",
                    "Peak1 Verisense steps",
                    "Peak30 AC", "Peak30 MIMS", "Peak30 Actilife steps",
                    "Peak30 ADEPT steps", "Peak30 log10 AC", "Peak30 log10 MIMS", "Peak30 Oak steps",
                    "Peak30 Stepcount RF steps", "Peak30 Stepcount SSL steps", "Peak30 Verisense rev. steps",
                    "Peak30 Verisense steps",
                    "Race/ethnicity", "Stroke",
                    "AC", "MIMS", "Actilife steps",
                    "ADEPT steps", "log10 AC", "log10 MIMS", "Oak steps",
                    "Stepcount RF steps", "Stepcount SSL steps", "Verisense rev. steps",
                    "Verisense steps"))

wt_single %>%
  filter(!grepl("peak", variable)) %>%
  mutate(var_group = case_when(
    grepl("steps", variable) ~ "Step variable",
    grepl("total", variable) ~ "Non-step accelerometry variable",
    TRUE ~ "Non-accelerometry variable"
  ),
  var_group = factor(var_group, levels = c("Step variable", "Non-step accelerometry variable", "Non-accelerometry variable"))) %>%
  group_by(variable, var_group) %>%
  summarize(mean = mean(concordance),
            sd = sd(concordance),
            se = sd(concordance)/sqrt(n())) %>%
  mutate(ci_low = mean - 1.96*se,
         ci_high = mean + 1.96*se) %>%
  ungroup() %>%
  left_join(var_labels, by = c("variable" = "names")) %>%
  mutate(labels = factor(labels),
         labels = fct_reorder(labels, mean)) %>%
  ggplot(aes(y = labels, x = mean, xmin = ci_low, xmax = ci_high, color = var_group, shape = var_group))+
  geom_point(size = 3) +
  # geom_errorbarh(height = .3) +
  theme_bw() +
  # scale_color_manual(values = c("#CC79A7FF", "#009E73FF", "#0072B2FF"), name = "")+
  scale_color_manual(values = c("#FF6DB6", "#009292", "#006DDB"), name = "")+ # colors from paletteer_d("colorBlindness::paletteMartin")
  scale_shape_manual(values = c(8, 17, 16), name = "")+
  scale_x_continuous(limits=c(0.5, 0.75), breaks=seq(0.5, 0.75, .05))+
  theme(legend.position = c(.3, .75),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 14))+
  labs(x = "Mean 100-times repeated 10-fold Cross-Validated Survey-Weighted Concordance", y = "")

if(save) {
  # ggsave(here::here("manuscript", "figures", "single_concordance.svg"), width = 8, height = 8)
  ggsave(
    here::here("manuscript", "figures", "single_concordance.png"),
    dpi = 400,
    width = 10,
    height = 8
  )
}

## concordance including "peak" variables
wt_single %>%
  filter(grepl("peak", variable) | grepl("steps", variable)) %>%
  mutate(var_group = factor(case_when(
    grepl("peak1", variable) ~ "Peak 1-min variable",
    grepl("peak30", variable) ~ "Peak 30-min variable",
    TRUE ~ "Mean daily total variable"
  ), levels = c("Peak 1-min variable", "Peak 30-min variable", "Mean daily total variable"))) %>%
  group_by(variable, var_group) %>%
  summarize(mean = mean(concordance),
            sd = sd(concordance),
            se = sd(concordance)/sqrt(n())) %>%
  mutate(ci_low = mean - 1.96*se,
         ci_high = mean + 1.96*se) %>%
  ungroup() %>%
  left_join(var_labels, by = c("variable" = "names")) %>%
  mutate(labels = factor(labels),
         labels = fct_reorder(labels, mean)) %>%
  ggplot(aes(y = labels, x = mean, xmin = ci_low, xmax = ci_high, color = var_group))+
  geom_point() +
  geom_errorbarh() +
  theme_bw() +
  # scale_color_manual(values = c("#E69F00FF", "#D55E00FF", "#56B4E9FF"), name = "")+
  scale_color_manual(values = c("#FF7F00", "#FFBF7F", "#654CFF"), name = "")+ # from paletteer_d("colorBlindness::PairedColor12Steps")
  scale_x_continuous(limits=c(0.687, 0.738), breaks=seq(0.675, 0.775, .0125))+
  theme(legend.position = c(.3, .7),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title= element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14))+
  labs(x = "Mean 100-times repeated 10-fold Cross-Validated Survey-Weighted Concordance", y = "")


if (save) {
  ggsave(
    here::here("manuscript", "figures", "single_concordance_cadence.png"),
    width = 8,
    height = 8,
    dpi = 400
  )
}

