# Average (mean) resolution time for NYC 311 requests, by borough.
#
# Resolution time := closed_date - created_date, in days.
# Only CLOSED requests can contribute: 78 requests that were never closed have no
# resolution time and are necessarily excluded (they are also, in all likelihood,
# the slowest cases - so these means are optimistic). 3 records closed before they
# were created are dropped as data errors.
#
# The distribution is severely right-skewed (citywide median 0.25 days vs mean
# 12.5 days), so the confidence intervals here are bootstrap percentile intervals
# rather than normal-theory ones.

library(readr); library(dplyr); library(lubridate)
library(ggplot2); library(scales)

csv_path <- file.path("data", "nyc_311_sample.csv")
out_path <- "output/311-mean-resolution-time-by-borough.png"

# ---- palette ----
surface <- "#fcfcfb"; series <- "#2a78d6"
ink     <- "#0b0b0b"; ink2   <- "#52514e"
muted   <- "#898781"; grid   <- "#e1e0d9"; axis <- "#c3c2b7"

# ---- data ----
raw <- read_csv(csv_path, col_types = cols(.default = col_character()))

req <- raw %>%
  mutate(
    created = ymd_hms(created_date, quiet = TRUE),
    closed  = ymd_hms(closed_date,  quiet = TRUE),
    days    = as.numeric(difftime(closed, created, units = "days"))
  ) %>%
  filter(is.na(days) | days >= 0)          # drop 3 records closed before created

resolved <- req %>% filter(!is.na(days))   # mean is only defined on closed requests
n_open   <- sum(is.na(req$days))

# ---- bootstrap percentile CI for the mean (skew makes t-intervals unreliable) ----
set.seed(232)
boot_ci <- function(x, B = 4000) {
  m <- replicate(B, mean(sample(x, length(x), replace = TRUE)))
  quantile(m, c(0.025, 0.975), names = FALSE)
}

by_boro <- resolved %>%
  group_by(borough) %>%
  summarise(
    n      = n(),
    mean   = mean(days),
    median = median(days),
    ci     = list(boot_ci(days)),
    .groups = "drop"
  ) %>%
  mutate(
    lo    = vapply(ci, `[`, numeric(1), 1),
    hi    = vapply(ci, `[`, numeric(1), 2),
    label = tools::toTitleCase(tolower(borough))
  ) %>%
  select(-ci) %>%
  arrange(desc(mean)) %>%
  mutate(label = factor(label, levels = label))

print(as.data.frame(by_boro), digits = 4)

citywide_mean   <- mean(resolved$days)
citywide_median <- median(resolved$days)

# ---- chart ----
top <- 30   # headroom for labels above the widest interval

p <- ggplot(by_boro, aes(x = label, y = mean)) +
  geom_hline(yintercept = citywide_mean, colour = axis,
             linetype = "dashed", linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = axis, linewidth = 0.4) +
  # one measure, one colour: hue carries no information, so it must not vary by rank
  geom_col(fill = series, width = 0.62) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12,
                colour = ink2, linewidth = 0.45, alpha = 0.75) +
  geom_text(aes(y = hi, label = sprintf("%.1f", mean)),
            vjust = -0.9, colour = ink, size = 3.9, fontface = "bold") +
  geom_text(aes(y = -1.6, label = paste0("n = ", comma(n))),
            colour = muted, size = 3.0) +
  geom_text(aes(y = -3.3, label = paste0("median ", sprintf("%.2f", median))),
            colour = muted, size = 2.9) +
  annotate("text", x = 5.42, y = citywide_mean,
           label = paste0("Citywide\n", sprintf("%.1f", citywide_mean), " days"),
           hjust = 0, colour = muted, size = 3.2, lineheight = 1.0) +
  scale_y_continuous(breaks = seq(0, top, 5),
                     limits = c(-4.2, top), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "Mean resolution time is driven by a small tail of very slow cases",
    subtitle = paste0("Mean days from request to close, with bootstrap 95% confidence intervals. ",
                      "Medians are shown\nbelow each bar: citywide the median is ",
                      sprintf("%.2f", citywide_median), " days against a mean of ",
                      sprintf("%.1f", citywide_mean), " days. NYC 311 sample,\n",
                      comma(nrow(resolved)), " closed requests created Jan 2024 - Dec 2025 (",
                      n_open, " never-closed requests excluded)."),
    x = NULL, y = "Mean resolution time (days)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = surface, colour = NA),
    panel.background   = element_rect(fill = surface, colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = grid, linewidth = 0.3),
    axis.ticks   = element_blank(),
    axis.text    = element_text(colour = muted),
    axis.text.x  = element_text(colour = ink2, size = 10.5, margin = margin(t = 22)),
    axis.title.y = element_text(colour = ink2, size = 10.5, margin = margin(r = 9)),
    plot.title    = element_text(colour = ink, size = 12.5, face = "bold",
                                 margin = margin(b = 8)),
    plot.subtitle = element_text(colour = ink2, size = 9, lineheight = 1.15,
                                 margin = margin(b = 14)),
    plot.title.position = "plot",
    plot.margin = margin(18, 80, 14, 14)
  )

ggsave(out_path, p, width = 7.8, height = 5.0, dpi = 200, bg = surface)
cat("saved", out_path, "\n")
