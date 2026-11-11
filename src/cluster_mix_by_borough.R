# Cluster mix by borough: what share of EACH borough's 311 requests falls in each
# cluster. Shares are within-borough, so every borough's nine bars sum to 100% and
# boroughs of very different size stay comparable.
#
# Each panel carries a dashed citywide reference line, so the question a reader
# asks -- "is this borough over- or under-represented here?" -- is answered by
# position relative to that line rather than by comparing panels.

library(readr); library(dplyr); library(ggplot2); library(scales)

csv_path <- file.path("data", "nyc_311_sample.csv")
map_path <- file.path("data", "complaint_type_clusters_revised.csv")
out_path <- "output/311-cluster-mix-by-borough.png"

# ---- palette ----
surface <- "#fcfcfb"; series <- "#2a78d6"
ink     <- "#0b0b0b"; ink2   <- "#52514e"
muted   <- "#898781"; grid   <- "#e1e0d9"; axis <- "#c3c2b7"
panelbg <- "#f5f5f2"

raw <- read_csv(csv_path, col_types = cols(.default = col_character()))
map <- read_csv(map_path, col_types = cols(.default = col_character()))

d <- raw %>%
  left_join(map %>% select(complaint_type, cluster = cluster_final), by = "complaint_type")
stopifnot(!any(is.na(d$cluster)))

# citywide mix, and the panel order (largest cluster first)
city_tbl <- d %>% count(cluster, name = "cn") %>% mutate(city = 100 * cn / sum(cn)) %>% arrange(desc(city))

# within-borough mix, completed so a zero cell is drawn as zero rather than dropped
boro_n <- d %>% count(borough, name = "boro_total")
mix <- expand.grid(borough = boro_n$borough, cluster = city_tbl$cluster,
                   stringsAsFactors = FALSE) %>%
  left_join(d %>% count(borough, cluster), by = c("borough", "cluster")) %>%
  mutate(n = ifelse(is.na(n), 0L, n)) %>%
  left_join(boro_n, by = "borough") %>%
  left_join(city_tbl %>% select(cluster, city), by = "cluster") %>%
  mutate(share = 100 * n / boro_total,
         diff  = share - city)

# boroughs ordered by volume, so the smallest-sample borough reads last everywhere
boro_order <- boro_n %>% arrange(boro_total) %>% pull(borough)
lab <- function(x) tools::toTitleCase(tolower(x))

mix <- mix %>%
  mutate(borough_lab = factor(lab(borough), levels = lab(boro_order)),
         panel = factor(sprintf("%s\ncitywide %.1f%%", cluster, city),
                        levels = sprintf("%s\ncitywide %.1f%%", city_tbl$cluster, city_tbl$city)))

print(mix %>% arrange(desc(abs(diff))) %>%
        select(borough, cluster, n, share, city, diff) %>% head(10) %>% as.data.frame(), digits = 3)

ref <- mix %>% distinct(panel, city)

p <- ggplot(mix, aes(x = share, y = borough_lab)) +
  geom_vline(data = ref, aes(xintercept = city),
             colour = axis, linetype = "dashed", linewidth = 0.4) +
  # one measure, one colour -- hue would carry no information across boroughs
  geom_col(fill = series, width = 0.62) +
  geom_text(aes(label = sprintf("%.0f", share)), hjust = -0.35,
            colour = ink2, size = 2.8) +
  facet_wrap(~panel, ncol = 3) +
  scale_x_continuous(limits = c(0, 41), breaks = seq(0, 40, 10),
                     labels = function(v) paste0(v, "%"), expand = c(0, 0)) +
  labs(
    title = "Each borough reports a different mix of problems",
    subtitle = paste0(
      "Share of each borough's own 311 requests falling in each cluster; the nine bars for a borough sum to 100%.\n",
      "Dashed line marks the citywide share, so a bar past it means that borough is over-represented in that cluster.\n",
      "Sample sizes: Brooklyn 1,484 | Queens 1,190 | Manhattan 1,020 | Bronx 941 | Staten Island 191."),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = surface, colour = NA),
    panel.background   = element_rect(fill = panelbg, colour = NA),
    panel.spacing.x    = unit(16, "pt"),
    panel.spacing.y    = unit(13, "pt"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = grid, linewidth = 0.3),
    strip.text = element_text(colour = ink, size = 9.2, face = "bold", lineheight = 1.2,
                              hjust = 0, margin = margin(b = 5, t = 2)),
    axis.text.y = element_text(colour = ink2, size = 9),
    axis.text.x = element_text(colour = muted, size = 8.5),
    axis.ticks  = element_blank(),
    plot.title    = element_text(colour = ink, size = 14, face = "bold", margin = margin(b = 7)),
    plot.subtitle = element_text(colour = ink2, size = 9.2, lineheight = 1.45, margin = margin(b = 16)),
    plot.title.position = "plot",
    plot.margin = margin(18, 18, 14, 14)
  )

ggsave(out_path, p, width = 10.2, height = 8.0, dpi = 200, bg = surface)
cat("saved", out_path, "\n")
