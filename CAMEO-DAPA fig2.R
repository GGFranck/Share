# import--------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggprism)
library(patchwork)
library(ggbeeswarm)
library(ggpubr)
library(ggtext)
# fig A --------
placebo_data <- tibble(
  treat = c(rep("Rest", 2), rep("Exercise", 2)),
  pcwp = c(15, 16, 31, 30),
  time = c("Baseline", "24 weeks", "Baseline", "24 weeks"),
  ymax = c(5, NA, 5, NA),
  ymin = c(-5, NA, -7, NA)
)
p1 <- placebo_data %>%
  ggplot(aes(x = fct_inorder(treat), y = pcwp, color = time)) +
  geom_errorbar(aes(ymin = pcwp + ymin, ymax = pcwp + ymax), width = 0.05, key_glyph = "point") +
  geom_path(aes(group = time), key_glyph = "point") +
  geom_point(size = 4, ) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "PCWP (mmHg)", title = "Placebo") +
  theme_prism() +
  scale_color_manual(
    values = c("24 weeks" = "#00B0F0", "Baseline" = "#0000C0"),
  ) +
  theme(
    legend.position = c(0.2, 0.9),
    legend.box.background = element_rect(colour = "black"),
    axis.ticks.x = element_blank()
  ) +
  guides(col = guide_legend(reverse = TRUE))
p1
# fig B -------
dapagliflozin_data <- tibble(
  treat = c(rep("Rest", 2), rep("Exercise", 2)),
  pcwp = c(13, 17, 25, 32),
  time = c("24 weeks", "Baseline", "24 weeks", "Baseline"),
  ymin = c(-4, NA, -6, NA),
  ymax = c(NA, 4, NA, 7)
)
p2 <- dapagliflozin_data %>%
  ggplot(aes(x = fct_inorder(treat), y = pcwp, color = time)) +
  geom_point(size = 4) +
  geom_path(aes(group = time), key_glyph = "point") +
  geom_errorbar(
    aes(ymin = pcwp + ymin, ymax = pcwp + ymax),
    width = 0.05, key_glyph = "point"
  ) +
  geom_segment(
    aes(x = treat, xend = treat, y = pcwp, yend = pcwp + ymax),
    key_glyph = "point"
  ) +
  geom_segment(
    aes(x = treat, xend = treat, y = pcwp, yend = pcwp + ymin),
    key_glyph = "point"
  ) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "PCWP (mmHg)", title = "Dapagliflozin") +
  theme_prism() +
  scale_color_manual(
    values = c("24 weeks" = "#FFC000", "Baseline" = "#FF6000"),
  ) +
  theme(
    legend.position = c(0.2, 0.9),
    legend.box.background = element_rect(colour = "black"),
    axis.ticks.x = element_blank()
  ) +
  guides(col = guide_legend(reverse = TRUE))
p2

# fig C ----
set.seed(1234)
pcwp <- c(
  runif(16, min = -10, max = 13),
  runif(20, min = -10, max = 3),
  runif(16, min = -12, max = 15),
  runif(20, min = -25, max = 4)
)
treat <- c(rep("Rest", 36), rep("Exercise", 36))
cohort <- c(
  rep("Placebo", 16),
  rep("Dapagliflozin", 20),
  rep("Placebo", 16),
  rep("Dapagliflozin", 20)
)
mydata <- tibble(pcwp, treat, cohort)

p3 <- mydata %>%
  ggplot(aes(x = fct_inorder(interaction(treat, cohort)), y = pcwp)) +
  geom_hline(yintercept = 0, linetype = 3, linewidth = 1) +
  geom_boxplot(
    aes(
      color = cohort,
      fill = factor(cohort, levels = c("Placebo", "Dapagliflozin"))
    ),
    key_glyph = "rect",
    width = 0.6, linewidth = 0.5
  ) +
  stat_boxplot(aes(color = cohort),
    geom = "errorbar", width = 0.2
  ) +
  stat_summary(
    fun = "median", geom = "crossbar",
    width = 0.8, color = "white",
    linewidth = 0.3
  ) +
  geom_beeswarm(
    aes(fill = cohort),
    shape = 21, size = 3, cex = 2, show.legend = FALSE
  ) +
  stat_compare_means(
    comparisons = list(c(1, 2), c(3, 4)),
    size = 0,
    label.y = 16,
  ) +
  coord_cartesian(ylim = c(-30, 20), clip = "off") +
  scale_color_manual(values = c("Placebo" = "#4472C4", "Dapagliflozin" = "#FF9900")) +
  scale_fill_manual(values = c("Placebo" = "#4472C4", "Dapagliflozin" = "#FF9900")) +
  scale_y_continuous(expand = c(0, 0)) +
  annotate(
    geom = "text",
    x = c(1.5, 3.5), y = -32,
    label = c("Rest", "Exercise"),
    size = 5,
    fontface = "bold",
  ) +
  annotate(
    geom = "richtext",
    x = c(1.5, 3.5), y = 24,
    label = c(
      "&Delta;-3.5 mmHg<br>(95% CI -6.7 to -0.4)<br>p=0.029",
      "&Delta;-5.7 mmHg<br>(95% CI -10.8 to -0.7)<br>p=0.027"
    ),
    size = 5, label.color = NA, fill = NA
  ) +
  labs(x = NULL, y = "Change in PCWP (mmHg)") +
  theme_prism() +
  theme(
    legend.box.background = element_rect(colour = "black"),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = unit(c(3, 0.5, 1.5, 0.5), "cm"),
  ) +
  guides(colour = FALSE)
p3
(p1 + p2) / (p3 + plot_spacer() + plot_layout(widths = c(3, 0))) +
  plot_annotation(tag_levels = "A") +
  plot_layout(heights = c(1, 1.5))

path <- "D:\\Data\\研一\\必修课\\论文指导\\图表分析\\fig\\"
ggsave(path = path, filename = "fig2.png", width = 10, height = 10)
