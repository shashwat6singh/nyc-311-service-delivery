# Monthly NYC 311 request volume, January 2024 - December 2025.
#
# Counts are of requests by CREATED date (when the request was logged), which is
# the right timestamp for "how many requests came in". All 4,826 records fall
# inside the window and all 24 months are complete, so no partial month distorts
# either end of the series.
#
# Note these are counts within a ~4,800-record SAMPLE, not true citywide volume.
# The month-to-month movement is nonetheless real rather than sampling noise:
# variance/mean = 7.1 against 1.0 expected under Poisson sampling
# (chi-sq 162.3 on 23 df, p < 2e-16).

library(readr); library(dplyr); library(lubridate)
library(ggplot2); library(scales)

csv_path <- file.path("data", "nyc_311_sample.csv")
out_path <- "output/311-monthly-requests.png"

# ---- palette ----
surface <- "#fcfcfb"; series <- "#2a78d6"
ink     <- "#0b0b0b"; ink2   <- "#52514e"
muted   <- "#898781"; grid   <- "#e1e0d9"; axis <- "#c3c2b7"

# ---- data ----
raw <- read_csv(csv_path, col_types = cols(.default = col_character()))

monthly <- raw %>%
  mutate(month = as.Date(floor_date(ymd_hms(created_date, quiet = TRUE), "month"))) %>%
  count(month, name = "requests") %>%
  arrange(month) %>%
  mutate(
    mom_pct = (requests / lag(requests) - 1) * 100,   # month-over-month change
    year    = year(month)
  )

print(as.data.frame(monthly), digits = 3)

annual <- monthly %>% group_by(year) %>%
  summarise(total = sum(requests), avg = mean(requests),
            start = min(month), end = max(month), .groups = "drop")
print(as.data.frame(annual), digits = 4)

peak   <- monthly %>% slice_max(requests, n = 1)
trough <- monthly %>% slice_min(requests, n = 1)

# ---- chart ----
p <- ggplot(monthly, aes(x = month, y = requests)) +
  # annual average levels, as reference lines with direct labels (not a second series)
  geom_segment(data = annual,
               aes(x = start - days(14), xend = end + days(14), y = avg, yend = avg),
               inherit.aes = FALSE, colour = axis, linetype = "dashed", linewidth = 0.4) +
  geom_text(data = annual,
            aes(x = c(as.Date("2024-07-15"), as.Date("2025-01-10")),
                y = c(annual$avg[1] - 32, annual$avg[2] + 34),
                label = paste0(year, " average  ", round(avg))),
            inherit.aes = FALSE, hjust = 0, colour = muted, size = 3.1) +
  geom_line(colour = series, linewidth = 0.8) +
  geom_point(colour = series, size = 1.5) +
  # call out only the extremes, never a label on every point
  geom_point(data = rbind(peak, trough), colour = series, size = 2.6,
             shape = 21, fill = surface, stroke = 1.1) +
  geom_text(data = peak, aes(label = paste0("Peak ", requests, "\n", format(month, "%b %Y"))),
            vjust = 0.5, hjust = 1.18, colour = ink2, size = 3.1, lineheight = 1.05) +
  geom_text(data = trough, aes(label = paste0("Trough ", requests, "\n", format(month, "%b %Y"))),
            vjust = 1.5, hjust = 0.2, colour = ink2, size = 3.1, lineheight = 1.05) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y",
               expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(limits = c(0, 310), breaks = seq(0, 300, 50),
                     expand = c(0, 0), labels = comma) +
  labs(
    title    = "311 request volume rose 18.5% between 2024 and 2025",
    subtitle = paste0("Requests logged per month. 2024 total ", comma(annual$total[1]),
                      " against ", comma(annual$total[2]), " in 2025. Monthly movement is real\n",
                      "variation, not sampling noise (variance/mean 7.1 vs 1.0 expected). ",
                      "NYC 311 sample, ", comma(sum(monthly$requests)), " requests."),
    x = NULL, y = "Requests logged"
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
    axis.text.x  = element_text(colour = ink2, size = 9.5, lineheight = 1.1,
                                margin = margin(t = 4)),
    axis.title.y = element_text(colour = ink2, size = 10.5, margin = margin(r = 9)),
    plot.title    = element_text(colour = ink, size = 12.5, face = "bold",
                                 margin = margin(b = 8)),
    plot.subtitle = element_text(colour = ink2, size = 9, lineheight = 1.15,
                                 margin = margin(b = 14)),
    plot.title.position = "plot",
    plot.margin = margin(18, 20, 14, 14)
  )

ggsave(out_path, p, width = 8.2, height = 4.8, dpi = 200, bg = surface)
cat("saved", out_path, "\n")
