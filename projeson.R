# ============================================================
#  Turkiye Sosyal Koruma Harcamalari - 6 Grafik + Poster
#  Paketler: ggplot2, readxl, dplyr, tidyr, scales,
#            patchwork, ggtext, ggrepel
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)
library(ggtext)
library(ggrepel)

dosya_t1 <- "t1_yardim_turleri_2000_2024.xlsx"
dosya_t2 <- "t2_sartli_sartsiz_2000_2024.xlsx"

ana_kategoriler <- c(
  "Emekli/yasli",
  "Hastalik/saglik bakimi",
  "Dul/yetim",
  "Aile/cocuk",
  "Engelli/malul",
  "Issizlik",
  "Sosyal dislanma b.y.s"
)

etiketler <- c(
  "Emekli/yasli"           = "Emekli/Yasli",
  "Hastalik/saglik bakimi" = "Hastalik/Saglik",
  "Dul/yetim"              = "Dul/Yetim",
  "Aile/cocuk"             = "Aile/Cocuk",
  "Engelli/malul"          = "Engelli/Malul",
  "Issizlik"               = "Issizlik",
  "Sosyal dislanma b.y.s"  = "Sosyal Dislanma"
)

renkler <- c(
  "Emekli/Yasli"    = "#378ADD",
  "Hastalik/Saglik" = "#1D9E75",
  "Dul/Yetim"       = "#D85A30",
  "Aile/Cocuk"      = "#BA7517",
  "Engelli/Malul"   = "#7F77DD",
  "Issizlik"        = "#D4537E",
  "Sosyal Dislanma" = "#888780"
)

t1_raw <- read_excel(dosya_t1, sheet = "data")

t1_long <- t1_raw %>%
  filter(Kategori %in% ana_kategoriler) %>%
  mutate(Kategori = recode(Kategori, !!!etiketler)) %>%
  pivot_longer(
    cols      = starts_with("Y_"),
    names_to  = "Yil",
    values_to = "Harcama_milyon"
  ) %>%
  mutate(
    Yil            = as.integer(sub("Y_", "", Yil)),
    Harcama_milyar = Harcama_milyon / 1000
  )

t2_raw <- read_excel(dosya_t2, sheet = "data")

t2_2024 <- t2_raw %>%
  filter(Kategori %in% ana_kategoriler) %>%
  mutate(Kategori = recode(Kategori, !!!etiketler)) %>%
  transmute(
    Kategori,
    Sartli  = Sartli_2024  / 1000,
    Sartsiz = Sartsiz_2024 / 1000,
    Toplam  = Sartli + Sartsiz
  )

t2_long <- t2_raw %>%
  filter(Kategori %in% ana_kategoriler) %>%
  mutate(Kategori = recode(Kategori, !!!etiketler)) %>%
  pivot_longer(
    cols      = -Kategori,
    names_to  = c("Tur", "Yil"),
    names_sep = "_",
    values_to = "Harcama_milyon"
  ) %>%
  mutate(
    Yil            = as.integer(Yil),
    Harcama_milyar = Harcama_milyon / 1000
  ) %>%
  pivot_wider(
    names_from  = Tur,
    values_from = c(Harcama_milyon, Harcama_milyar)
  ) %>%
  mutate(
    Toplam_milyar = Harcama_milyar_Sartli + Harcama_milyar_Sartsiz,
    Sartli_oran   = ifelse(Toplam_milyar > 0,
                           Harcama_milyar_Sartli  / Toplam_milyar * 100, 0),
    Sartsiz_oran  = ifelse(Toplam_milyar > 0,
                           Harcama_milyar_Sartsiz / Toplam_milyar * 100, 0)
  )


# ============================================================
# GRAFIK 1 - Harcama Trendi
# ============================================================
g1 <- ggplot(t1_long, aes(x = Yil, y = Harcama_milyar,
                          color = Kategori, group = Kategori)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2, alpha = 0.6) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, suffix = " mr TL")
  ) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  scale_color_manual(values = renkler) +
  labs(
    title    = "Sosyal Yardim Turleri - Harcama Trendi (2000-2024)",
    subtitle = "Yardim kategorilerine gore yillik harcama",
    x        = NULL,
    y        = "Milyar TL",
    color    = NULL,
    caption  = "Kaynak: T\u00DC\u0130K"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank()
  )

print(g1)


# ============================================================
# GRAFIK 2 - Kategori Paylari Egim Grafigi
# ============================================================
pay_data <- t1_long %>%
  filter(Yil %in% c(2000, 2024)) %>%
  group_by(Yil) %>%
  mutate(Pay = Harcama_milyar / sum(Harcama_milyar) * 100) %>%
  ungroup() %>%
  select(Kategori, Yil, Pay) %>%
  mutate(Yil = factor(Yil))

g2 <- ggplot(pay_data, aes(x = Yil, y = Pay,
                           color = Kategori, group = Kategori)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(
    data = pay_data %>% filter(Yil == 2000),
    aes(label = paste0("%", round(Pay, 1))),
    hjust = 1.3, size = 3.2, fontface = "bold"
  ) +
  geom_text(
    data = pay_data %>% filter(Yil == 2024),
    aes(label = paste0("%", round(Pay, 1), "  ", Kategori)),
    hjust = -0.1, size = 3.2, fontface = "bold"
  ) +
  scale_color_manual(values = renkler) +
  scale_y_continuous(labels = function(x) paste0("%", x)) +
  expand_limits(x = c(0.6, 2.9)) +
  labs(
    title    = "Kategori Paylarinin 24 Yillik Degisimi",
    subtitle = "Sosyal yardimlar toplami icindeki yuzde pay (2000 -> 2024)",
    x        = NULL,
    y        = "Toplam icindeki pay (%)",
    caption  = "Kaynak: T\u00DC\u0130K"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(g2)


# ============================================================
# GRAFIK 3 - Sartli/Sartsiz Konumlanma Haritasi (Sadece 2024)
# ============================================================
t2_long_g3 <- t2_long %>%
  filter(Yil == 2024)

g3 <- ggplot(t2_long_g3,
             aes(x = Harcama_milyar_Sartli,
                 y = Harcama_milyar_Sartsiz)) +
  geom_point(aes(size = Toplam_milyar, fill = Kategori),
             shape = 21, color = "white", stroke = 0.6, alpha = 0.85) +
  scale_fill_manual(
    values = renkler,
    name   = "Kategori",
    guide  = guide_legend(
      override.aes   = list(size = 4, alpha = 1, shape = 21, color = "white"),
      title.position = "top"
    )
  ) +
  scale_size_continuous(
    range  = c(2, 18),
    name   = "Toplam harcama (mr TL)",
    breaks = c(50, 200, 500, 1000, 2000),
    guide  = guide_legend(
      override.aes   = list(fill = "grey60", color = "white", alpha = 0.7,
                            shape = 21, size = c(1, 2, 3, 4, 5)),
      title.position = "top"
    )
  ) +
  scale_x_continuous(labels = label_number(suffix = " mr TL"),
                     expand = expansion(mult = 0.15)) +
  scale_y_continuous(labels = label_number(suffix = " mr TL"),
                     expand = expansion(mult = 0.15)) +
  labs(
    title    = "Sartli - Sartsiz Konumlanma Haritasi (2024)",
    subtitle = "Balon buyuklugu toplam harcamayi gostermektedir",
    x        = "Sartli harcama (Milyar TL)",
    y        = "Sartsiz harcama (Milyar TL)",
    caption  = "Kaynak: T\u00DC\u0130K"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

print(g3)


# ============================================================
# GRAFIK 4 - Risk Gruplarina Gore Yigilmis Cubuk
# ============================================================
risk_kategoriler_etiketsiz <- c(
  "Aile/Cocuk",
  "Emekli/Yasli",
  "Hastalik/Saglik",
  "Issizlik"
)

risk_renkler <- c(
  "Aile/Cocuk"      = "#4472C4",
  "Emekli/Yasli"    = "#E8604C",
  "Hastalik/Saglik" = "#70AD47",
  "Issizlik"        = "#C39BD3"
)

risk_data <- t1_long %>%
  filter(Kategori %in% risk_kategoriler_etiketsiz) %>%
  mutate(Kategori = factor(Kategori, levels = risk_kategoriler_etiketsiz))

g4 <- ggplot(risk_data, aes(x = factor(Yil), y = Harcama_milyar, fill = Kategori)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = risk_renkler, name = NULL) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, suffix = " mr TL"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(
    title    = "Sosyal Koruma Harcamalarinin Risk Gruplarina Gore Dagilimi",
    subtitle = "Secili 4 risk grubu: Aile/Cocuk, Emekli/Yasli, Hastalik/Saglik, Issizlik",
    x        = "Yil",
    y        = "Harcama Tutari (Milyar TL)",
    caption  = "Kaynak: T\u00DC\u0130K"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(size = 9)
  )

print(g4)


# ============================================================
# GRAFIK 5 - Harcama Dagilimi Box Plot
# ============================================================
g5 <- ggplot(t1_long, aes(x = reorder(Kategori, Harcama_milyar, FUN = median),
                          y = Harcama_milyar,
                          fill = Kategori)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21,
               outlier.fill = "white", outlier.size = 2,
               outlier.stroke = 0.6) +
  geom_jitter(aes(color = Kategori), width = 0.15,
              size = 1.2, alpha = 0.45) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.1, suffix = " mr TL"),
    limits = c(-200, NA),
    breaks = c(0, 500, 1000, 1500, 2000)
  ) +
  scale_fill_manual(values  = renkler, guide = "none") +
  scale_color_manual(values = renkler, guide = "none") +
  coord_flip() +
  labs(
    title    = "Sosyal Yardim Kategorilerinin Harcama Dagilimi (2000-2024)",
    subtitle = "Her kutu 25 yillik dagilimi gostermektedir",
    x        = NULL,
    y        = "Milyar TL",
    caption  = "Kaynak: T\u00DC\u0130K"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(g5)


# ============================================================
# GRAFIK 6 - Sartli / Sartsiz Gercek Harcama (Diverging Bar)
#            X ekseni duzeltilmis: az break, egik yazi, mr TL
# ============================================================
secili_yillar <- seq(2000, 2024, by = 4)

div_data <- t2_long %>%
  filter(Yil %in% secili_yillar) %>%
  select(Kategori, Yil, Harcama_milyar_Sartli, Harcama_milyar_Sartsiz) %>%
  mutate(
    Sartli_pos  =  Harcama_milyar_Sartli,
    Sartsiz_neg = -Harcama_milyar_Sartsiz,
    Yil         = factor(Yil, levels = rev(secili_yillar))
  ) %>%
  pivot_longer(
    cols      = c(Sartli_pos, Sartsiz_neg),
    names_to  = "Tur",
    values_to = "Deger"
  ) %>%
  mutate(
    Tur_etiket = ifelse(Tur == "Sartli_pos", "Sartli", "Sartsiz")
  )

g6 <- ggplot(div_data, aes(x = Deger, y = Yil, fill = Tur_etiket)) +
  geom_col(width = 0.65, alpha = 0.88) +
  geom_vline(xintercept = 0, color = "grey30", linewidth = 0.5) +
  facet_wrap(~ Kategori, ncol = 4, scales = "free_x") +
  scale_x_continuous(
    breaks = breaks_pretty(n = 3),
    labels = function(x) paste0(abs(round(x)), " mr"),
    expand = expansion(mult = 0.05)
  ) +
  scale_fill_manual(
    values = c("Sartli" = "#E8604C", "Sartsiz" = "#378ADD"),
    name   = NULL,
    labels = c("Sartli" = "Sartli \u2192", "Sartsiz" = "\u2190 Sartsiz")
  ) +
  labs(
    title    = "Sartli / Sartsiz Harcamanin Kategoriye Gore Degisimi (2000-2024)",
    subtitle = "Sol: sartsiz tutar (Milyar TL)  |  Sag: sartli tutar (Milyar TL)  |  Bar uzadikca harcama artmistir",
    x        = NULL,
    y        = NULL,
    caption  = "Kaynak: T\u00DC\u0130K  |  T2 verisi, her 4 yilda bir kesit"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position    = "bottom",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text         = element_text(face = "bold", size = 9),
    axis.text.x        = element_text(size = 7, angle = 45,
                                      hjust = 1, vjust = 1),
    plot.title         = element_text(face = "bold"),
    panel.spacing.x    = unit(1.2, "lines")
  )

print(g6)


# ============================================================
# POSTER - A0 Dikey (84x119 cm) Akademik Poster
# ============================================================
p1 <- g1 + labs(title = "1. Harcama Trendi (2000-2024)", subtitle = "Milyar TL") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        legend.text   = element_text(size = 9),
        axis.text     = element_text(size = 9))

p2 <- g2 + labs(title    = "2. Kategori Paylarinin Degisimi",
                subtitle = "Toplam icerisindeki yuzde pay (2000 vs 2024)") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text     = element_text(size = 9))

p3 <- g3 + labs(title    = "3. Sartli - Sartsiz Konumlanma Haritasi (2024)",
                subtitle = "Balon buyuklugu toplam harcamayi gostermektedir") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text     = element_text(size = 9),
        legend.text   = element_text(size = 9))

p4 <- g4 + labs(title    = "4. Risk Gruplarina Gore Dagilim",
                subtitle = "Yigilmis cubuk grafik, Milyar TL") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text     = element_text(size = 8),
        legend.text   = element_text(size = 9))

p5 <- g5 + labs(title    = "5. Harcama Dagilimi - Box Plot",
                subtitle = "25 yillik istatistiksel dagilim, Milyar TL") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text     = element_text(size = 9))

p6 <- g6 + labs(title    = "6. Sartli/Sartsiz Harcama Degisimi (2000-2024)",
                subtitle = "Sol: sartsiz  |  Sag: sartli  |  Milyar TL") +
  theme(plot.title    = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        legend.text   = element_text(size = 9),
        axis.text     = element_text(size = 8),
        strip.text    = element_text(size = 8, face = "bold"))

poster <- (p1 | p2) / (p3 | p4) / (p5 | p6) +
  plot_annotation(
    title    = "Turkiye'de Sosyal Koruma Harcamalarinin Yapisal Donusumu: 2000-2024",
    subtitle = "Mustafa KOSE  |  Sueda DEMIRASLAN  |  Aksaray Universitesi",
    caption  = "Kaynak: T\u00DC\u0130K",
    theme = theme(
      plot.title       = element_text(size = 22, face = "bold",
                                      hjust = 0.5, margin = margin(b = 6)),
      plot.subtitle    = element_text(size = 16, hjust = 0.5, color = "grey30",
                                      margin = margin(b = 10)),
      plot.caption     = element_text(size = 10, hjust = 0.5, color = "grey50"),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )

ggsave(
  filename = "poster_sosyal_koruma_A0.pdf",
  plot     = poster,
  width    = 84,
  height   = 119,
  units    = "cm",
  dpi      = 150,
  device   = "pdf"
)

message("Poster kaydedildi: poster_sosyal_koruma_A0.pdf")