################################################################################
# download data
################################################################################
load(file = paste0("./result_1.rda"))
load(file = paste0("./result_2.rda"))
load(file = paste0("./result_3.rda"))
load(file = paste0("./result_4.rda"))
load(file = paste0("./results_all.rda"))

a = list(result1, result2, result3, result4)
sd_list = list()
result_list = list()
for (ii in 1:4){
  result = a[[ii]]
  for (jj in 1:5){
    kk = jj*10
    DD = subset(result, k==kk)
    result_list = c(result_list, list(DD))
    true_sd = sd(DD$p11_hat)
    sd_list = c(sd_list, list(true_sd))
  }
}
rm(ii,jj,kk,true_sd,DD,result)
# difference and ratio of estimated SE
for (ii in 1:20){
  result = result_list[[ii]]
  for (jj in 1:1000){
    result[jj, 12] = result[jj, 11] - sd_list[[ii]]
    result[jj, 13] = abs(result[jj, 12])/(result[jj, 6])
  }
  result_list[[ii]] = result
}
rm(result)

results = result_list[[1]]
for (ii in 2:20){
  results = rbind(results, result_list[[ii]])
}


################################################################################
# Confidence Interval
################################################################################
# CI coverage rate
compute_ci_coverage <- function(data, ci_col = "CI_2_cover", group_vars = c("k", "p11_true", "ni_around"), drop_na = TRUE) {
  data <- data[!is.na(data[[ci_col]]), ]
  if (drop_na && ci_col == "CI_1_cover") {
    data <- subset(data, CI_1_cover != 9)
  }
  coverage <- aggregate(data[[ci_col]] == 1, by = data[group_vars], FUN = mean)
  names(coverage)[ncol(coverage)] <- "coverage_rate"
  return(coverage)
}

# CI average width
compute_ci_width <- function(data, ci_up_col, ci_down_col, group_vars = c("k", "p11_true", "ni_around"), cover_filter = NULL) {
  if (!is.null(cover_filter)) {
    data <- subset(data, data[[cover_filter]] == 1)
  }
  data$ci_width <- data[[ci_up_col]] - data[[ci_down_col]]
  width <- aggregate(data$ci_width, by = data[group_vars], FUN = mean)
  names(width)[ncol(width)] <- "avg_width"
  return(width)
}

ci2_cover_summary <- compute_ci_coverage(results, ci_col = "CI_2_cover")
ci1_cover_summary <- compute_ci_coverage(results, ci_col = "CI_1_cover", drop_na = TRUE)

ci2_width_summary <- compute_ci_width(results, "CI_2_up", "CI_2_down")
ci1_width_summary <- compute_ci_width(results, "CI_1_up", "CI_1_down", cover_filter = "CI_1_cover")

################################################################################
# p11 results
################################################################################
library(ggplot2)

DD1 = subset(results, p11_true == 0.25 & ni_around == 1000)
DD2 = subset(results, p11_true == 0.25 & ni_around == 100)

DD1$group <- "ni around 1000"
DD2$group <- "ni around 100"
combined_data <- rbind(DD1, DD2)

combined_data$k <- as.factor(combined_data$k)
combined_data$group <- as.factor(combined_data$group)

ggplot(combined_data, aes(x = p11_hat, y = k, fill = group)) +
  geom_boxplot() +
  geom_vline(xintercept = 0.25, color = "red", linetype = "dashed", size = 0.8) +
  scale_fill_manual(values = c( "lightgrey","white"),
                    guide = guide_legend(reverse = TRUE) ) +
  labs(
    title = expression(p[11] == 0.25 ~ "for different" ~ n[i] ~ "ranges"),
    x = expression(hat(p)[11]),
    y = expression(k)
  ) +
  xlim(0.15, 0.35) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "serif"),
    legend.title = element_blank(),
    axis.title.y = element_text(angle = 0, size = 16, family = "serif"),
    axis.title.x = element_text(size = 16, family = "serif"),
    axis.text = element_text(size = 14, family = "serif"),
    legend.text = element_text(size = 14, family = "serif"),
    #legend.position = c(0.01, 0.99),
    #legend.justification = c("left", "top")
    legend.position = "none"
  )




DD1 = subset(results, p11_true == 0.75 & ni_around == 1000)
DD2 = subset(results, p11_true == 0.75 & ni_around == 100)

DD1$group <- "ni around 1000"
DD2$group <- "ni around 100"
combined_data <- rbind(DD1, DD2)

combined_data$k <- as.factor(combined_data$k)
combined_data$group <- as.factor(combined_data$group)

ggplot(combined_data, aes(x = p11_hat, y = k, fill = group)) +
  geom_boxplot() +
  geom_vline(xintercept = 0.75, color = "red", linetype = "dashed", size = 0.8) +
  scale_fill_manual(values = c( "lightgrey","white"),
                    guide = guide_legend(reverse = TRUE),
                    labels = c("100 ≤ ni ≤ 200", "800 ≤ ni ≤ 1000")) +
  labs(
    title = expression(p[11] == 0.75 ~ "for different" ~ n[i] ~ "ranges"),
    x = expression(hat(p)[11]),
    y = expression(k)
  ) +
  xlim(0.6, 0.8) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "serif"),
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    #axis.title.y = element_text(angle = 0, size = 16, family = "serif"),
    axis.title.x = element_text(size = 16, family = "serif"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 14, family = "serif"),
    legend.text = element_text(size = 14, family = "serif"),
    legend.position = c(0.01, 0.99),
    legend.justification = c("left", "top")
  )


################################################################################
# SE results
################################################################################
library(ggplot2)
DD1 = subset(results, p11_true == 0.25 & ni_around == 1000)
DD2 = subset(results, p11_true == 0.25 & ni_around == 100)

DD1$k <- as.factor(DD1$k)
DD2$k <- as.factor(DD2$k)

ggplot(DD1, aes(x = sd_error, y = k)) +
  geom_boxplot() +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 0.8) +
  labs(
    title = expression(p[11] == 0.25 ~ "for 800 ≤" ~ n[i] ~ "≤ 1000"),
    x = expression(hat(SE) - SE),
    y = expression(k)
  ) +
  xlim(-0.1, 0.1) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "serif"),
    axis.title.y = element_text(angle = 0, size = 16, family = "serif"),
    axis.title.x = element_text(size = 16, family = "serif"),
    axis.text = element_text(size = 14, family = "serif")
  )

ggplot(DD2, aes(x = sd_error, y = k)) +
  geom_boxplot() +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 0.8) +
  labs(
    title = expression(p[11] == 0.25 ~ "for 100 ≤" ~ n[i] ~ "≤ 200"),
    x = expression(hat(SE) - SE),
    y = expression(k)
  ) +
  xlim(-0.1, 0.1) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "serif"),
    axis.title.y = element_text(angle = 0, size = 16, family = "serif"),
    axis.title.x = element_text(size = 16, family = "serif"),
    axis.text = element_text(size = 14, family = "serif")
  )


DD1 = subset(results, p11_true == 0.75 & ni_around == 1000)
DD2 = subset(results, p11_true == 0.75 & ni_around == 100)
DD1$k <- as.factor(DD1$k)
DD2$k <- as.factor(DD2$k)

ggplot(DD1, aes(x = sd_error, y = k)) +
  geom_boxplot() +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 0.8) +
  labs(
    title = expression(p[11] == 0.75 ~ "for 800 ≤" ~ n[i] ~ "≤ 1000"),
    x = expression(hat(SE) - SE),
    y = expression(k)
  ) +
  xlim(-0.03, 0.03) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "serif"),
    axis.title.y = element_text(angle = 0, size = 16, family = "serif"),
    axis.title.x = element_text(size = 16, family = "serif"),
    axis.text = element_text(size = 14, family = "serif")
  )

ggplot(DD2, aes(x = sd_error, y = k)) +
  geom_boxplot() +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 0.8) +
  labs(
    title = expression(p[11] == 0.75 ~ "for 100 ≤" ~ n[i] ~ "≤ 200"),
    x = expression(hat(SE) - SE),
    y = expression(k)
  ) +
  xlim(-0.03, 0.03) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "serif"),
    axis.title.y = element_text(angle = 0, size = 16, family = "serif"),
    axis.title.x = element_text(size = 16, family = "serif"),
    axis.text = element_text(size = 14, family = "serif")
  )

################################################################################
#QQ & hist
################################################################################
plot_histograms <- function(results, ni_val, p11_val) {
  ni_label <- if (ni_val < 500) "small" else "large"
  par(family = "serif")
  par(mfcol = c(1, 5))
  for (ii in 1:5) {
    kk <- 10 * ii
    DD <- subset(results, k == kk & ni_around == ni_val & p11_true == p11_val)
    hist(DD$p11_hat, xlab = expression(p[11]), 
         main = bquote(p[11] == .(p11_val) ~ " & " ~ .(ni_label) ~ " " ~ n[i] ~ " & " ~ k == .(kk)))
  }
}


plot_qqplots <- function(results, ni_val, p11_val) {
  ni_label <- if (ni_val < 500) "small" else "large"
  par(family = "serif")
  par(mfcol = c(1, 5))
  for (ii in 1:5) {
    kk <- 10 * ii
    DD <- subset(results, k == kk & ni_around == ni_val & p11_true == p11_val)
    qqnorm(DD$p11_hat, 
           main = bquote(p[11] == .(p11_val) ~ " & " ~ .(ni_label) ~ " " ~ n[i] ~ " & " ~ k == .(kk)))
    qqline(DD$p11_hat, distribution = qnorm, qtype = 7, col = 'darkred')
  }
}

for (p11 in c(0.25, 0.75)) {
  for (ni in c(100, 1000)) {
    plot_histograms(results, ni_val = ni, p11_val = p11)
    plot_qqplots(results, ni_val = ni, p11_val = p11)
  }
}


