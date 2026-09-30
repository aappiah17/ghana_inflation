# =============================================================================
# Ghana Prime Building Cost Index (PBCI): charts for the August 2026 release
# Data:  GSS, PBCI Annexes August 2026 ("PBCI - ANNEXES August 2026.xlsx")
# Chart: alfredappiah.com
# =============================================================================



path    <- "data/pbci.xlsx"
out_dir <- "charts"
dir.create(out_dir, showWarnings = FALSE)

# ---- House style ------------------------------------------------------------
focus <- "#800020"   # series in focus
ctx   <- "#A6A6A6"   # context series
bg    <- "#FFFAF0"   # floralwhite

# Device default sans. Set PBCI_FONT (e.g. "Arial") to force a specific family.
font_family <- Sys.getenv("PBCI_FONT", "")
update_geom_defaults("text", list(family = font_family))



lead    <- function(x) paste0("<span style='color:", focus, "'>", x, "</span>")
src     <- "Data: Ghana Statistical Service"
credit  <- "Chart: alfredappiah.com"
save_it <- function(p, name, w = 10, h = 6.5) {
  ggsave(file.path(out_dir, name), p, width = w, height = h, dpi = 200,
         device = ragg::agg_png, bg = bg)
}

# ---- Helpers ------------------------------------------------------------------
month_rows <- function(df) {
  df |> filter(str_detect(.data[[names(df)[1]]], "^[A-Za-z]{3}_\\d{4}$"))
}
to_date <- function(x) as.Date(paste0("01_", str_to_title(x)), format = "%d_%b_%Y")

# ---- Annex 1: headline and group indices ------------------------------------
a1_raw <- read_excel(path, sheet = "Annex1_Headline&Group Inflation",
                     col_names = FALSE, .name_repair = "minimal")
names(a1_raw) <- paste0("c", seq_along(a1_raw))

a1 <- month_rows(a1_raw) |>
  transmute(
    date = to_date(c1),
    idx_national = as.numeric(c2),  idx_labour = as.numeric(c3),
    idx_materials = as.numeric(c4), idx_plant = as.numeric(c5),
    yoy_national = as.numeric(c6),  yoy_labour = as.numeric(c7),
    yoy_materials = as.numeric(c8), yoy_plant = as.numeric(c9),
    mom_national = as.numeric(c10), mom_labour = as.numeric(c11),
    mom_materials = as.numeric(c12), mom_plant = as.numeric(c13)
  )

a1_long <- a1 |>
  pivot_longer(-date, names_to = c("measure", "group"), names_sep = "_") |>
  mutate(group = str_to_title(group))

latest     <- max(a1$date)
latest_lab <- format(latest, "%b %Y")
lat        <- a1 |> filter(date == latest)

# ---- Annex 4: sub-group year-on-year inflation and contributions ----------
a4_raw <- read_excel(path, sheet = "Annex4_Contribution by subgroup",
                     col_names = FALSE, .name_repair = "minimal")
names(a4_raw) <- paste0("c", seq_along(a4_raw))

sg_names <- a4_raw |> filter(str_detect(c1, "SUB-GROUP")) |> unlist()
sg_names <- sg_names[seq(2, length(sg_names), by = 2)]
sg_names <- sg_names[!is.na(sg_names)] |>
  str_replace("Unkilled", "Unskilled") |> str_to_title()

a4_months <- month_rows(a4_raw)
a4 <- lapply(seq_along(sg_names), function(k) {
  tibble(date    = to_date(a4_months$c1),
         subgroup = sg_names[k],
         yoy     = as.numeric(a4_months[[2 * k]]),
         contrib = as.numeric(a4_months[[2 * k + 1]]))
}) |> bind_rows()

# =============================================================================
# Chart 1: headline year-on-year inflation
# =============================================================================
h <- a1 |> filter(!is.na(yoy_national))
pk <- h |> slice_max(yoy_national, n = 1)
tr <- h |> filter(date > pk$date) |> slice_min(yoy_national, n = 1)

p1 <- ggplot(h, aes(date, yoy_national)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  geom_line(colour = focus, linewidth = 1.4) +
  geom_point(data = bind_rows(pk, tr, lat), colour = focus, size = 3) +
  annotate("text", x = pk$date, y = pk$yoy_national + 1.6,
           label = paste0("Peak: ", round(pk$yoy_national, 1), "%\n(",
                          format(pk$date, "%b %Y"), ")"),
           colour = "grey30", fontface = "bold", size = 3.8, lineheight = 0.9) +
  annotate("text", x = tr$date, y = tr$yoy_national - 2.2,
           label = paste0("Low: ", round(tr$yoy_national, 1), "%\n(",
                          format(tr$date, "%b %Y"), ")"),
           colour = "grey30", fontface = "bold", size = 3.8, lineheight = 0.9) +
  annotate("text", x = latest + 20, y = lat$yoy_national,
           label = paste0(round(lat$yoy_national, 1), "%"), hjust = 0,
           colour = focus, fontface = "bold", size = 4.6) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months",
               expand = expansion(mult = c(0.02, 0.07))) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(-2, 28)) +
  labs(title = paste0(lead("Building Cost Inflation"), " Has Turned Up Again"),
       subtitle = paste0("Year-on-year change in the national PBCI<br>", src),
       x = NULL, y = NULL, caption = credit) +
  theme_alfred()
save_it(p1, "01_headline_yoy.png")

# =============================================================================
# Chart 2: year-on-year inflation by major group
# =============================================================================
g <- a1_long |> filter(measure == "yoy", group != "National", !is.na(value))
g_end <- g |> filter(date == latest)

p2 <- ggplot(g, aes(date, value, group = group)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  geom_line(data = filter(g, group != "Plant"), colour = ctx, linewidth = 1) +
  geom_line(data = filter(g, group == "Plant"), colour = focus, linewidth = 1.4) +
  geom_text(data = g_end,
            aes(x = latest + 20,
                label = paste0(group, " (", sprintf("%+.1f", value), "%)"),
                colour = group == "Plant"),
            hjust = 0, fontface = "bold", size = 4.2) +
  scale_colour_manual(values = c(`TRUE` = focus, `FALSE` = "grey45")) +
  scale_x_date(date_labels = "%b\n%Y",
               breaks = seq(as.Date("2024-06-01"), latest, by = "6 months"),
               expand = expansion(mult = c(0.02, 0.24))) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_cartesian(clip = "off") +
  labs(title = paste0(lead("Plant Costs"), " Are Surging While Labour Costs Fall"),
       subtitle = paste0("Year-on-year change by major group, ", latest_lab, " values in brackets<br>",
                         "Weights: materials 76.5%, labour 19.5%, plant 4.0%<br>", src),
       x = NULL, y = NULL, caption = credit) +
  theme_alfred()
save_it(p2, "02_group_yoy.png")

# =============================================================================
# Chart 3: contribution of each sub-group to headline inflation, latest month
# =============================================================================
cl <- a4 |> filter(date == latest) |>
  mutate(subgroup = reorder(subgroup, contrib), pos = contrib >= 0)
cat("Sum of contributions:", round(sum(cl$contrib), 2),
    "| Published headline:", round(lat$yoy_national, 2), "\n")

p3 <- ggplot(cl, aes(contrib, subgroup, fill = pos)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.2f", contrib),
                hjust = ifelse(pos, -0.15, 1.15), colour = pos),
            fontface = "bold", size = 3.6) +
  scale_fill_manual(values = c(`TRUE` = focus, `FALSE` = ctx)) +
  scale_colour_manual(values = c(`TRUE` = focus, `FALSE` = "grey45")) +
  scale_x_continuous(expand = expansion(mult = 0.18), labels = label_number(accuracy = 0.5)) +
  labs(title = paste0(lead("Electrical Work"), " Added Two Points to Building Cost Inflation"),
       subtitle = paste0("Contribution to the ", round(lat$yoy_national, 1),
                         "% year-on-year PBCI inflation in ", latest_lab,
                         " (percentage points)<br>Steel and cement pulled it down<br>", src),
       x = "Percentage points", y = NULL, caption = credit) +
  theme_alfred() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())
save_it(p3, "03_contributions_latest.png", h = 8)

# =============================================================================
# Chart 4: sub-group inflation, a year ago vs latest month (dumbbell)
# =============================================================================
yr_ago <- seq(latest, length.out = 2, by = "-1 year")[2]
db <- a4 |> filter(date %in% c(yr_ago, latest)) |>
  mutate(when = ifelse(date == latest, "now", "then")) |>
  select(subgroup, when, yoy) |>
  pivot_wider(names_from = when, values_from = yoy) |>
  mutate(subgroup = reorder(subgroup, now))

p4 <- ggplot(db, aes(y = subgroup)) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.4) +
  geom_segment(aes(x = then, xend = now, yend = subgroup), colour = "grey80", linewidth = 1.2) +
  geom_point(aes(x = then), colour = ctx, size = 3.2) +
  geom_point(aes(x = now), colour = focus, size = 3.6) +
  geom_text(aes(x = now, label = sprintf("%+.1f%%", now),
                hjust = ifelse(now >= then, -0.35, 1.35)),
            colour = focus, fontface = "bold", size = 3.5) +
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = 0.12)) +
  labs(title = paste0("The Price Leaders Have Swapped: ", lead(latest_lab), " vs ",
                      "<span style='color:grey55'>", format(yr_ago, "%b %Y"), "</span>"),
       subtitle = paste0("Year-on-year inflation by sub-group, grey dot a year ago, red dot now<br>",
                         "Steel went from +22.7% to -8.9%; plumbing from +4.2% to +26.1%<br>", src),
       x = NULL, y = NULL, caption = credit) +
  theme_alfred()
save_it(p4, "04_subgroup_dumbbell.png", h = 8.5)

# =============================================================================
# Chart 5: headline month-on-month change, last 24 months
# =============================================================================
m <- a1 |> filter(date > seq(latest, length.out = 2, by = "-2 years")[2]) |>
  mutate(is_latest = date == latest)

p5 <- ggplot(m, aes(date, mom_national, fill = is_latest)) +
  geom_col(width = 24) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  geom_text(aes(label = ifelse(is_latest, sprintf("%.2f", mom_national), sprintf("%.1f", mom_national)),
                vjust = ifelse(mom_national >= 0, -0.5, 1.5),
                colour = is_latest),
            fontface = "bold", size = 3.1) +
  annotate("text", x = latest - 60, y = 2.6,
           label = paste0(latest_lab, ": +", sprintf("%.2f", lat$mom_national), "%,\nnearly flat"),
           colour = focus, fontface = "bold", size = 3.9, hjust = 1, lineheight = 0.9) +
  scale_fill_manual(values = c(`TRUE` = focus, `FALSE` = ctx)) +
  scale_colour_manual(values = c(`TRUE` = focus, `FALSE` = "grey45")) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 months") +
  scale_y_continuous(labels = label_percent(scale = 1), expand = expansion(mult = 0.12)) +
  labs(title = paste0("Month-on-Month, ", lead("August Barely Moved")),
       subtitle = paste0("Monthly change in the national PBCI (%), last 24 months<br>", src),
       x = NULL, y = NULL, caption = credit) +
  theme_alfred()
save_it(p5, "05_headline_mom.png")

# =============================================================================
# Chart 6: index levels by major group since January 2023
# =============================================================================
lv <- a1_long |> filter(measure == "idx", group != "National")
lv_end <- lv |> filter(date == latest)

p6 <- ggplot(lv, aes(date, value, group = group)) +
  geom_hline(yintercept = 100, colour = "grey55", linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = as.Date("2025-01-01"), y = 98.3, label = "2023 average = 100",
           hjust = 0, colour = "grey45", fontface = "bold", size = 3.5) +
  geom_line(data = filter(lv, group != "Plant"), colour = ctx, linewidth = 1) +
  geom_line(data = filter(lv, group == "Plant"), colour = focus, linewidth = 1.4) +
  geom_text_repel(data = lv_end,
                  aes(label = paste0(group, " ", round(value, 1)), colour = group == "Plant"),
                  nudge_x = 45, hjust = 0, direction = "y", segment.colour = NA,
                  fontface = "bold", size = 4.2) +
  scale_colour_manual(values = c(`TRUE` = focus, `FALSE` = "grey45")) +
  scale_x_date(date_labels = "%b\n%Y",
               breaks = seq(as.Date("2023-06-01"), latest, by = "6 months"),
               expand = expansion(mult = c(0.02, 0.18))) +
  coord_cartesian(clip = "off") +
  labs(title = paste0(lead("Plant Costs"), " Have Caught Up With Materials in 2026"),
       subtitle = paste0("PBCI level by major group. Plant rose from ",
                         round(filter(lv, group == "Plant", date == as.Date("2025-12-01"))$value, 1),
                         " in Dec 2025 to ", round(filter(lv_end, group == "Plant")$value, 1),
                         " in ", latest_lab, "<br>", src),
       x = NULL, y = NULL, caption = credit) +
  theme_alfred()
save_it(p6, "06_group_levels.png")
