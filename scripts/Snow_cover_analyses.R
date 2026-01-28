# ==========================================
# MODIS Snow Cover (Daily) ??? Bar + Rolling Mean
# Main panel + 30-day zoom inset + shaded window
# ==========================================

library(readr)
library(dplyr)
library(ggplot2)
library(zoo)

has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
has_cowplot   <- requireNamespace("cowplot", quietly = TRUE)

file_path <- "F:/Kurtun_heyelan/Makale/ilk_yuklenen/Red_sonrasi/Data/MODIS_snow_cover_rate_2025.csv"

df_raw <- read_delim(file_path, delim = ";", trim_ws = TRUE, show_col_types = FALSE)

date_col <- names(df_raw)[grepl("^date$|tarih", names(df_raw), ignore.case = TRUE)][1]
snow_col <- names(df_raw)[grepl("snow|cover|snow_cover|kar", names(df_raw), ignore.case = TRUE)][1]
if (is.na(date_col) || is.na(snow_col)) stop("Could not detect Date/Snow_cover columns.")

df_snow <- df_raw %>%
  transmute(
    Date_Clean = as.Date(as.character(.data[[date_col]]), format = "%Y.%m.%d"),
    Snow_cover = as.numeric(gsub(",", ".", as.character(.data[[snow_col]])))
  ) %>%
  filter(!is.na(Date_Clean)) %>%
  arrange(Date_Clean) %>%
  mutate(
    Snow_7d_mean = zoo::rollapply(Snow_cover, width = 7, FUN = mean, align = "right",
                                  fill = NA, na.rm = TRUE, partial = TRUE)
  )

landslide_date  <- as.Date("2025-04-22")
peak_rain_start <- as.Date("2025-04-08")
peak_rain_end   <- as.Date("2025-04-09")

# Main plot (bars for daily snow cover)
p_main <- ggplot(df_snow, aes(x = Date_Clean)) +
  annotate("rect",
           xmin = peak_rain_start, xmax = peak_rain_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.15) +
  # Daily snow cover as bars
  geom_col(aes(y = Snow_cover, fill = "Daily Snow Cover"),
           width = 0.9, alpha = 0.75, na.rm = TRUE) +
  # 7-day rolling mean as a line
  geom_line(aes(y = Snow_7d_mean, color = "7-day Rolling Mean"),
            linewidth = 1.0, na.rm = TRUE) +
  # Landslide date
  geom_vline(xintercept = landslide_date, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = landslide_date, y = 90, label = "Landslide Event",
           angle = 90, vjust = -0.5, color = "red", size = 3.5) +
  annotate("text", x = peak_rain_start, y = 95, label = "Peak rainfall & rapid melt window",
           hjust = 0, size = 3.2) +
  scale_y_continuous(limits = c(0, 100), name = "Snow cover (%)") +
  scale_fill_manual(values = c("Daily Snow Cover" = "deepskyblue3")) +
  scale_color_manual(values = c("7-day Rolling Mean" = "red")) +
  labs(
    title = "Daily Snow Cover Dynamics (MODIS)",
    subtitle = "Daily bars with 7-day rolling mean and key-period highlighting",
    x = "Date (2025)",
    fill = "",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Zoom inset (last 30 days)
zoom_start <- landslide_date - 30

p_zoom <- ggplot(df_snow %>% filter(Date_Clean >= zoom_start & Date_Clean <= landslide_date),
                 aes(x = Date_Clean)) +
  annotate("rect",
           xmin = peak_rain_start, xmax = peak_rain_end,
           ymin = -Inf, ymax = Inf,
           alpha = 0.15) +
  geom_col(aes(y = Snow_cover),
           width = 0.9, alpha = 0.75, fill = "deepskyblue3", na.rm = TRUE) +
  geom_line(aes(y = Snow_7d_mean),
            linewidth = 0.9, color = "red", na.rm = TRUE) +
  geom_vline(xintercept = landslide_date, linetype = "dashed", color = "red", linewidth = 0.7) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 week") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(subtitle = "30-day Zoomed Window") +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    panel.grid.minor = element_blank()
  )

# Combine with inset
if (has_patchwork && "inset_element" %in% getNamespaceExports("patchwork")) {
  library(patchwork)
  final_plot <- p_main +
    patchwork::inset_element(p_zoom, left = 0.06, bottom = 0.70, right = 0.50, top = 0.99)
} else {
  if (!has_cowplot) stop("patchwork inset_element not available and cowplot not installed. Install cowplot.")
  library(cowplot)
  final_plot <- cowplot::ggdraw() +
    cowplot::draw_plot(p_main, 0, 0, 1, 1) +
    cowplot::draw_plot(p_zoom, 0.06, 0.55, 0.44, 0.35)
}

print(final_plot)
ggsave("Figure_11_Revised_SnowCover.png", final_plot, width = 10, height = 6, dpi = 300)
