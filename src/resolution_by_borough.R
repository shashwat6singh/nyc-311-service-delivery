# Share of NYC 311 requests resolved within 7 days, by borough.
#
# "Resolved within 7 days" := closed_date is present and
# (closed_date - created_date) <= 7 days. The denominator is every request
# logged in that borough, so requests never closed count as NOT resolved
# within 7 days.

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

csv_path <- file.path("data", "nyc_311_sample.csv")
out_path <- "output/311-resolved-7days-by-borough-R.png"

# ---- palette (same tokens as the Python version) ----
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
  filter(is.na(days) | days >= 0) %>%          # drop 3 records closed before created
  mutate(within7 = !is.na(days) & days <= 7)

citywide <- mean(req$within7)

z <- 1.96
by_boro <- req %>%
  group_by(borough) %>%
  summarise(n = n(), k = sum(within7), .groups = "drop") %>%
  mutate(
    p      = k / n,
    den    = 1 + z^2 / n,
    centre = (p + z^2 / (2 * n)) / den,
    half   = z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den,
    lo     = centre - half,
    hi     = centre + half,
    label  = tools::toTitleCase(tolower(borough))
  ) %>%
  arrange(desc(p)) %>%
  mutate(label = factor(label, levels = label))   # lock the sorted order

print(by_boro %>% select(label, n, k, p, lo, hi))

# ---- chart ----
p <- ggplot(by_boro, aes(x = label, y = p)) +
  geom_hline(yintercept = citywide, colour = axis,
             linetype = "dashed", linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = axis, linewidth = 0.4) +   # baseline under the bars
  # one measure, one colour: hue carries no information, so it must not vary by rank
  geom_col(fill = series, width = 0.62) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12,
                colour = ink2, linewidth = 0.45, alpha = 0.75) +
  geom_text(aes(y = hi, label = percent(p, accuracy = 0.1)),
            vjust = -0.9, colour = ink, size = 3.9, fontface = "bold") +
  geom_text(aes(y = -0.055, label = paste0("n = ", comma(n))),
            colour = muted, size = 3.0) +
  annotate("text", x = 5.42, y = citywide,
           label = paste0("Citywide\n", percent(citywide, accuracy = 0.1)),
           hjust = 0, colour = muted, size = 3.2, lineheight = 1.0) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2),
                     limits = c(-0.075, 1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "Boroughs resolve 311 requests within a week at broadly similar rates",
    subtitle = paste("Share of logged requests closed within 7 days of creation, with 95% confidence\n",
                     "intervals. NYC 311 sample, ", comma(nrow(req)),
                     " requests created Jan 2024 - Dec 2025.", sep = ""),
    x = NULL, y = "Requests resolved within 7 days"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background   = element_rect(fill = surface, colour = NA),
    panel.background  = element_rect(fill = surface, colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = grid, linewidth = 0.3),
    axis.ticks   = element_blank(),
    axis.text    = element_text(colour = muted),
    axis.text.x  = element_text(colour = ink2, size = 10.5, margin = margin(t = 4)),
    axis.title.y = element_text(colour = ink2, size = 10.5, margin = margin(r = 9)),
    plot.title    = element_text(colour = ink, size = 12.5, face = "bold",
                                 margin = margin(b = 8)),
    plot.subtitle = element_text(colour = ink2, size = 9, lineheight = 1.15,
                                 margin = margin(b = 14)),
    plot.title.position = "plot",
    plot.margin = margin(18, 74, 14, 14)
  )

ggsave(out_path, p, width = 7.6, height = 4.6, dpi = 200, bg = surface)
cat("saved", out_path, "\n")
