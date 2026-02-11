# (0) Setup
library(tidyverse)
library(knitr)
library(kableExtra)
library(fitdistrplus)
library(truncnorm)

survey_raw <- read.csv("ESS11_23_24.csv")

# (1) Pre-processing
# (1.1) Missing values and invalid codes → NA
recode_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[x %in% c(77,88,99,777,888,999,7777,8888,9999)] <- NA
  x[x < 0] <- NA
  x
}

# (1.2) Core fields and validity filters
survey <- survey_raw %>%
  transmute(
    country = as.factor(cntry),
    happiness = recode_na(happy),
    social_contact = recode_na(sclmeet),
    age = recode_na(agea)
  ) %>%
  filter(
    !is.na(country), !is.na(happiness), !is.na(social_contact), !is.na(age),
    happiness %in% 0:10, social_contact %in% 1:7
  )

# (1.3) Age bands
survey <- survey %>%
  mutate(age_band = cut(
    age,
    breaks = c(15,29,44,64,120),
    labels = c("18–29","30–44","45–64","65+"),
    include.lowest = TRUE
  ))

# (1.4) Spain subset
spain_df <- survey %>% filter(country %in% "ES")

# (2) Descriptive analysis
# (2.1) Happiness summary (Spain, 2024)
summary_happy <- spain_df %>%
  summarise(
    N = n(),
    Min = min(happiness),
    Q1 = quantile(happiness, 0.25),
    Median = median(happiness),
    Mean = mean(happiness),
    Q3 = quantile(happiness, 0.75),
    Max = max(happiness),
    SD = sd(happiness)
  )

kable(
  summary_happy,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  align = "rrrrrrr",
  row.names = FALSE,
  linesep = "",
  caption = "Happiness summary statistics (Spain, 2024)"
) |>
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

theme_large <- theme_minimal() +
  theme(
    axis.title.x = element_text(size = 19, margin = margin(t = 12)),
    axis.title.y = element_text(size = 19, margin = margin(r = 12)),
    axis.text.x  = element_text(size = 18, margin = margin(t = 5)),
    axis.text.y  = element_text(size = 18, margin = margin(r = 5)),
    axis.ticks        = element_line(linewidth = 0.7),
    axis.ticks.length = unit(6, "pt"),
    axis.line         = element_line(linewidth = 0.7),
    plot.background   = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.margin       = margin(17, 17, 17, 17)
  )

theme_compact <- theme_minimal() +
  theme(
    axis.title.x = element_text(size = 13, margin = margin(t = 12)),
    axis.title.y = element_text(size = 13, margin = margin(r = 12)),
    axis.text.x  = element_text(size = 12, margin = margin(t = 5)),
    axis.text.y  = element_text(size = 12, margin = margin(r = 5)),
    axis.ticks        = element_line(linewidth = 0.7),
    axis.ticks.length = unit(6, "pt"),
    axis.line         = element_line(linewidth = 0.7),
    plot.background   = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.margin       = margin(17, 17, 17, 17),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 12),
    legend.key   = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = "grey80", linewidth = 0.5),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.key.size = unit(1.2, "lines")
  )

ggplot(spain_df, aes(happiness)) +
  geom_histogram(binwidth = 1, boundary = 0.5, color = "white") +
  scale_x_continuous(breaks = 0:10, limits = c(-0.5, 10.5)) +
  labs(x = "Happiness", y = "Count") +
  theme_large

ggplot(spain_df, aes(y = happiness, x = "")) +
  geom_boxplot(na.rm = TRUE, outlier.alpha = 0.6, width = 0.25) +
  coord_flip() +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  labs(x = NULL, y = "Happiness") +
  theme_large

# (2.2) Social contact summaries
contact_labels <- c(
  "1" = "(1) Never",
  "2" = "(2) Less than once a month",
  "3" = "(3) Once a month",
  "4" = "(4) Several times a month",
  "5" = "(5) Once a week",
  "6" = "(6) Several times a week",
  "7" = "(7) Every day"
)

summary_contact <- spain_df %>%
  summarise(
    N = n(),
    Min = min(social_contact),
    Q1 = quantile(social_contact, 0.25),
    Median = median(social_contact),
    Mean = mean(social_contact),
    Q3 = quantile(social_contact, 0.75),
    Max = max(social_contact),
    SD = sd(social_contact)
  )

kable(
  summary_contact,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  align = "rrrrrrr",
  row.names = FALSE,
  linesep = "",
  caption = "Social contact summary statistics (Spain, 2024)"
) |>
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

spain_df <- spain_df %>%
  mutate(social_contact_f = factor(social_contact, levels = 1:7, labels = contact_labels, ordered = TRUE))

summary_happy_by_contact <- spain_df %>%
  group_by(social_contact_f) %>%
  summarise(
    N = n(),
    Min = min(happiness),
    Q1 = quantile(happiness, 0.25),
    Median = median(happiness),
    Mean = mean(happiness),
    Q3 = quantile(happiness, 0.75),
    Max = max(happiness),
    SD = sd(happiness),
    .groups = "drop"
  )

kable(
  summary_happy_by_contact,
  format = "latex",
  booktabs = TRUE,
  digits = 2,
  align = "rrrrrrr",
  row.names = FALSE,
  linesep = "",
  caption = "Happiness by social contact summary statistics (Spain, 2024)"
) |>
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

ggplot(spain_df, aes(social_contact)) +
  geom_histogram(binwidth = 1, boundary = 0.5, color = "white") +
  scale_x_continuous(breaks = 1:7, limits = c(0.5, 7.5)) +
  labs(x = "Social contact", y = "Count") +
  theme_large

ggplot(spain_df, aes(x = factor(social_contact, levels = 1:7), y = happiness)) +
  geom_boxplot(width = 0.75, outlier.alpha = 0.6, na.rm = TRUE) +
  scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
  labs(x = "Social contact", y = "Happiness") +
  theme_large

# (3) Model fitting
happy_raw  <- spain_df$happiness
happy_trim <- pmin(pmax(happy_raw, 1e-6), 10 - 1e-6)

# (3.1) Method of moments
mom_norm_mean <- mean(happy_trim)
mom_norm_sd   <- sd(happy_trim)

x_01 <- happy_trim / 10
mean_x <- mean(x_01)
var_x  <- var(x_01)
alpha_mom <- mean_x * ((mean_x * (1 - mean_x)) / var_x - 1)
beta_mom  <- (1 - mean_x) * ((mean_x * (1 - mean_x)) / var_x - 1)
if (alpha_mom <= 0 | beta_mom <= 0) { alpha_mom <- 1; beta_mom <- 1 }

mom_gamma_shape <- (mean(happy_trim)^2) / var(happy_trim)
mom_gamma_rate  <- mean(happy_trim) / var(happy_trim)

# (3.2) Distributions on [0,10]
dbeta_0_10 <- function(x, shape1, shape2, log = FALSE) {
  dens <- ifelse(x <= 0 | x >= 10, 0, dbeta(x / 10, shape1 = shape1, shape2 = shape2) / 10)
  if (isTRUE(log)) log(dens) else dens
}
pbeta_0_10 <- function(q, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
  q <- pmin(pmax(q, 0), 10)
  pbeta(q / 10, shape1 = shape1, shape2 = shape2, lower.tail = lower.tail, log.p = log.p)
}
dtn_0_10 <- function(x, mean, sd, log = FALSE) {
  dens <- dtruncnorm(x, a = 0, b = 10, mean = mean, sd = sd)
  if (isTRUE(log)) log(dens) else dens
}
ptn_0_10 <- function(q, mean, sd, lower.tail = TRUE, log.p = FALSE) {
  ptruncnorm(q, a = 0, b = 10, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p)
}

# (3.3) MLE fits
fit_norm_mle  <- fitdist(happy_trim, "norm")
fit_beta_mle  <- fitdist(
  happy_trim, "beta_0_10",
  start = list(shape1 = max(0.5, alpha_mom), shape2 = max(0.5, beta_mom)),
  lower = c(1e-6, 1e-6)
)
fit_gamma_mle <- fitdist(
  happy_trim, "gamma",
  start = list(shape = mom_gamma_shape, rate = mom_gamma_rate),
  lower = c(1e-6, 1e-6)
)
fit_unif_mle  <- fitdist(happy_trim, "unif")
fit_tnorm_mle <- fitdist(happy_trim, "tn_0_10", start = list(mean = mean(happy_trim), sd = sd(happy_trim)))

# (3.4) AIC
loglik_norm_mom  <- sum(dnorm(happy_trim, mean = mom_norm_mean, sd = mom_norm_sd, log = TRUE))
loglik_beta_mom  <- sum(dbeta_0_10(happy_trim, shape1 = alpha_mom, shape2 = beta_mom, log = TRUE))
loglik_gamma_mom <- sum(dgamma(happy_trim, shape = mom_gamma_shape, rate = mom_gamma_rate, log = TRUE))

AIC_norm_mom  <- 2 * 2 - 2 * loglik_norm_mom
AIC_beta_mom  <- 2 * 2 - 2 * loglik_beta_mom
AIC_gamma_mom <- 2 * 2 - 2 * loglik_gamma_mom

AIC_norm_mle  <- AIC(fit_norm_mle)
AIC_beta_mle  <- AIC(fit_beta_mle)
AIC_gamma_mle <- AIC(fit_gamma_mle)
AIC_unif_mle  <- AIC(fit_unif_mle)
AIC_tnorm_mle <- AIC(fit_tnorm_mle)

# (3.5) Results table
fit_table <- tibble(
  Method = c("MoM","MLE","MoM","MLE","MoM","MLE","MLE","MLE"),
  Distribution = c("Normal","Normal","Beta[0–10]","Beta[0–10]","Gamma","Gamma","Uniform","TruncNormal[0–10]"),
  Param1 = c(
    mom_norm_mean,
    fit_norm_mle$estimate["mean"],
    alpha_mom,
    fit_beta_mle$estimate["shape1"],
    mom_gamma_shape,
    fit_gamma_mle$estimate["shape"],
    fit_unif_mle$estimate["min"],
    fit_tnorm_mle$estimate["mean"]
  ),
  Param2 = c(
    mom_norm_sd,
    fit_norm_mle$estimate["sd"],
    beta_mom,
    fit_beta_mle$estimate["shape2"],
    mom_gamma_rate,
    fit_gamma_mle$estimate["rate"],
    fit_unif_mle$estimate["max"],
    fit_tnorm_mle$estimate["sd"]
  ),
  AIC = c(
    AIC_norm_mom, AIC_norm_mle,
    AIC_beta_mom, AIC_beta_mle,
    AIC_gamma_mom, AIC_gamma_mle,
    AIC_unif_mle, AIC_tnorm_mle
  )
) %>% arrange(AIC)

print(fit_table)

# (3.6) Density overlay
grid_y <- seq(0, 10, length.out = 600)

dens_norm  <- dnorm(grid_y, mean = fit_norm_mle$estimate["mean"], sd = fit_norm_mle$estimate["sd"])
dens_beta  <- dbeta_0_10(grid_y, shape1 = fit_beta_mle$estimate["shape1"], shape2 = fit_beta_mle$estimate["shape2"])
dens_gamma <- dgamma(grid_y, shape = fit_gamma_mle$estimate["shape"], rate  = fit_gamma_mle$estimate["rate"])
dens_tnorm <- dtn_0_10(grid_y, mean = fit_tnorm_mle$estimate["mean"], sd = fit_tnorm_mle$estimate["sd"])

dens_df <- tibble(
  Happiness = grid_y,
  Normal = dens_norm,
  Beta = dens_beta,
  Gamma = dens_gamma,
  TruncNormal = dens_tnorm
) %>%
  pivot_longer(-Happiness, names_to = "Distribution", values_to = "Density")

hist_data <- hist(happy_trim, breaks = 30, plot = FALSE)
peak_scale <- max(hist_data$density)

dens_df <- dens_df %>%
  group_by(Distribution) %>%
  mutate(Density = Density * peak_scale / max(Density))

u_min <- fit_unif_mle$estimate["min"]
u_max <- fit_unif_mle$estimate["max"]
u_height <- dunif(mean(c(u_min, u_max)), min = u_min, max = u_max)
u_height <- u_height * peak_scale / max(dens_df$Density)

ggplot() +
  geom_histogram(aes(x = happy_trim, y = ..density..), bins = 30, fill = "grey85", color = "white") +
  geom_line(data = dens_df, aes(x = Happiness, y = Density, color = Distribution), linewidth = 1.2) +
  geom_segment(aes(x = u_min, xend = u_max, y = u_height, yend = u_height, color = "Uniform"), linewidth = 1.2) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
  labs(x = "Happiness", y = "Density", color = "Distribution") +
  theme_compact

# (3.7) LaTeX table
fit_table %>%
  mutate(across(c(Param1, Param2, AIC), ~ round(., 3))) %>%
  kable(format = "latex", booktabs = TRUE,
        caption = "Parameter estimates and AIC for fitted distributions") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  add_header_above(c(" " = 2, "Parameters" = 2, " " = 1))

# (4) Statistical inference of one variable
# (4.1) Confidence interval for mean happiness in Spain
n_sp <- nrow(spain_df)
mean_h <- mean(spain_df$happiness)
sd_h <- sd(spain_df$happiness)
se_h <- sd_h / sqrt(n_sp)
alpha <- 0.05
tcrit <- qt(1 - alpha / 2, df = n_sp - 1)
ci_lower <- mean_h - tcrit * se_h
ci_upper <- mean_h + tcrit * se_h
ci_mean <- c(lower = ci_lower, mean = mean_h, upper = ci_upper)
ci_mean

# (4.2) Plot with labels nudged to the side
hbar <- mean(spain_df$happiness)
x_off <- 0.15

ggplot(spain_df, aes(x = happiness)) +
  geom_histogram(binwidth = 1, fill = "grey85", color = "white") +
  geom_vline(xintercept = 7, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = hbar, color = "blue", linewidth = 1) +
  annotate("text", x = 7 + x_off,    y = Inf, label = sprintf("%.2f", 7),    vjust = 1.2, hjust = 0, color = "red") +
  annotate("text", x = hbar + x_off, y = Inf, label = sprintf("%.2f", hbar), vjust = 1.2, hjust = 0, color = "blue") +
  labs(x = "Happiness", y = "Count") +
  theme_large

t.test(spain_df$happiness, mu = 7, alternative = "greater")

# (5) Statistical inference of two variables
# (5.1) Hypothesis 2: happiness increases with social contact (linear trend)
model_h2 <- lm(happiness ~ social_contact, data = spain_df)
summary(model_h2)
confint(model_h2)

slope_est <- coef(summary(model_h2))["social_contact", "Estimate"]
slope_se  <- coef(summary(model_h2))["social_contact", "Std. Error"]
t_val <- slope_est / slope_se
p_one_sided <- 1 - pt(t_val, df = model_h2$df.residual)
c(t_val = t_val, p_one_sided = p_one_sided)

ggplot(spain_df, aes(x = social_contact, y = happiness)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "Social contact", y = "Happiness") +
  theme_large


# (5.2) Monotone association (Spearman, one-sided > 0)
cor_sc <- cor.test(spain_df$happiness, spain_df$social_contact,
                   method = "spearman", alternative = "greater")
cor_sc

means_by_contact <- spain_df %>%
  group_by(social_contact) %>%
  summarise(N = n(),
            mean_h = mean(happiness),
            se = sd(happiness) / sqrt(N),
            .groups = "drop")

ggplot(means_by_contact, aes(x = social_contact, y = mean_h)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_h - 1.96 * se, ymax = mean_h + 1.96 * se), width = 0.15) +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "Social contact", y = "Mean happiness") +
  theme_large


# (5.3) Differences across age groups (ANOVA + Tukey)
anova_age <- aov(happiness ~ age_band, data = spain_df)
summary(anova_age)
TukeyHSD(anova_age)

ggplot(spain_df, aes(x = age_band, y = happiness)) +
  geom_boxplot(outlier.alpha = 0.6) +
  labs(x = "Age group", y = "Happiness") +
  theme_large


# (5.4) High vs low social contact (two-sample t-test; High > Low)
contact_groups <- spain_df %>%
  mutate(group = case_when(
    social_contact %in% 1:3 ~ "Low",
    social_contact %in% 5:7 ~ "High",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(group))

x_high <- contact_groups %>% filter(group %in% "High") %>% pull(happiness)
x_low  <- contact_groups %>% filter(group %in% "Low")  %>% pull(happiness)
t_high_gt_low <- t.test(x_high, x_low, alternative = "greater")
t_high_gt_low

ggplot(contact_groups, aes(x = group, y = happiness)) +
  geom_boxplot(outlier.alpha = 0.6) +
  labs(x = "Contact group", y = "Happiness") +
  theme_large


# (5.5) Proportion of high happiness by high contact (chi-square)
prop_df <- spain_df %>%
  mutate(
    high_happy = ifelse(happiness >= 8, 1, 0),
    high_contact = ifelse(social_contact >= 5, 1, 0)
  )

tab_hh <- table(prop_df$high_happy, prop_df$high_contact)
chisq.test(tab_hh)

bar_df <- prop_df %>%
  group_by(high_contact) %>%
  summarise(prop_high = mean(high_happy), .groups = "drop") %>%
  mutate(high_contact = factor(high_contact, levels = c(0,1), labels = c("Low/Med", "High")))

ggplot(bar_df, aes(x = high_contact, y = prop_high)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.2f", prop_high)), vjust = -0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Social contact (5–7 vs others)", y = "Proportion with happiness ≥ 8") +
  theme_large




