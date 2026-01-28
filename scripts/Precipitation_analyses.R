# ==============================
# ERA5-Land Rainfall Plot (ROBUST FIX)
# Handles column-name conflicts like Date
# ==============================

library(readr)
library(dplyr)
library(ggplot2)
library(zoo)

has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
has_cowplot   <- requireNamespace("cowplot", quietly = TRUE)

# 1) Read file - try comma first; if only 1 column, retry with semicolon
file_path <- "F:/Kurtun_heyelan/Makale/ilk_yuklenen/Red_sonrasi/Data/ERA5_Daily_rainfall_2025.csv"

df_raw <- read_delim(file_path, delim = ",", trim_ws = TRUE, show_col_types = FALSE)

if (ncol(df_raw) == 1) {
  df_raw <- read_delim(file_path, delim = ";", trim_ws = TRUE, show_col_types = FALSE)
}

# 2) Inspect column names and pick the right ones safely
print(names(df_raw))

# Find date column (e.g., "Date", "date", "Tarih")
date_col <- names(df_raw)[grepl("^date$|tarih", names(df_raw), ignore.case = TRUE)][1]
# Find precipitation column (e.g., "Prec_mm", "prec_mm", "precip")
prec_col <- names(df_raw)[grepl("prec|rain|prcp|mm", names(df_raw), ignore.case = TRUE)][1]

if (is.na(date_col) || is.na(prec_col)) {
  stop("Could not detect Date/Precipitation columns. Please check names(df_raw).")
}

# 3) Clean & convert (NOTE: use .data[[...]] to avoid 'Date' conflicts)
df_rain <- df_raw %>%
  transmute(
    Date_Clean = as.Date(as.character(.data[[date_col]]), format = "%d/%m/%Y"),
    Prec_mm    = as.numeric(gsub(",", ".", as.character(.data[[prec_col]])))
  ) %>%
  filter(!is.na(Date_Clean), !is.na(Prec_mm)) %>%
  arrange(Date_Clean) %>%
  mutate(
    Prec_14d = zoo::rollsum(Prec_mm, 14, align = "right", fill = NA)
  )

stopifnot(nrow(df_rain) > 0)

# 4) Plot
coeff <- 4
ymax <- max(df_rain$Prec_mm, na.rm = TRUE)
if (!is.finite(ymax) || ymax == 0) ymax <- 1

p_main <- ggplot(df_rain, aes(x = Date_Clean)) +
  geom_col(aes(y = Prec_mm, fill = "Daily Precipitation"), alpha = 0.7, width = 0.9) +
  geom_line(aes(y = Prec_14d / coeff, color = "14-day Antecedent Sum"),
            linewidth = 1, na.rm = TRUE) +
  scale_y_continuous(
    name = "Daily Precipitation (mm)",
    sec.axis = sec_axis(~ . * coeff, name = "14-day Antecedent Rainfall (mm)")
  ) +
  scale_fill_manual(values = c("Daily Precipitation" = "steelblue")) +
  scale_color_manual(values = c("14-day Antecedent Sum" = "darkred")) +
  geom_vline(xintercept = as.Date("2025-04-22"), linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = as.Date("2025-04-22"), y = ymax * 0.5,
           label = "Landslide Event", angle = 90, vjust = -0.5, color = "red", size = 3.5) +
  annotate("text", x = as.Date("2025-04-08"), y = ymax * 0.9,
           label = "8 April: 53 mm", size = 3.5, fontface = "bold", color = "black") +
  labs(x = "Date", title = "Precipitation Dynamics (ERA5-Land Data)") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom")

p_zoom <- ggplot(df_rain %>% filter(Date_Clean >= as.Date("2025-03-22") & Date_Clean <= as.Date("2025-04-22")),
                 aes(x = Date_Clean)) +
  geom_col(aes(y = Prec_mm), fill = "steelblue", alpha = 0.8, width = 0.9) +
  geom_line(aes(y = Prec_14d / coeff), color = "darkred", linewidth = 0.8, na.rm = TRUE) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 week") +
  labs(subtitle = "30-day Zoomed Window") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 7),
        panel.grid.minor = element_blank())

if (has_patchwork && "inset_element" %in% getNamespaceExports("patchwork")) {
  library(patchwork)
  final_plot <- p_main + patchwork::inset_element(p_zoom, left = 0.06, bottom = 0.55, right = 0.48, top = 0.95)
} else {
  if (!has_cowplot) stop("patchwork inset_element not available and cowplot not installed. Install cowplot.")
  library(cowplot)
  final_plot <- cowplot::ggdraw() +
    cowplot::draw_plot(p_main, 0, 0, 1, 1) +
    cowplot::draw_plot(p_zoom, 0.06, 0.55, 0.42, 0.35)
}

print(final_plot)
ggsave("Figure_12_Revised_Rainfall.png", final_plot, width = 10, height = 6, dpi = 300)
