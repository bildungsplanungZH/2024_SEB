## Nicht gebrauchte Abbildungen

## {r, plot Beitrag der Schule zum digitalen Wissen}

digi_schule_t <- p2 %>%
  filter(!is.na(profil)) %>%
  select(year, starts_with("schule")) %>%
  group_by(year) %>% 
  summarise(across(starts_with("schule"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("schule"), names_to = "indicator", values_to = "mean") %>%
  mutate(
    profil = "Total",
    indicator = case_when(
      indicator == "schule_zwecke" ~ "Nutzung digitaler Medien für private Zwecke",
      indicator == "schule_aufträge" ~ "Nutzung digitaler Medien für schulische Aufträge",
      indicator == "schule_selbständig" ~ "Nutzung digitaler Medien für selbständiges Lernen",
      indicator == "schule_gefahren" ~ "Kenntnis der Auswirkungen und Gefahren digitaler Medien",
      indicator == "schule_tools" ~ "Kenntnis der technischen Seite digitaler Tools",
      TRUE ~ indicator),
    text = paste0(profil, ": ", round(mean, 2)),
    profil = fct_relevel(profil, "Total"),
    indicator = factor(indicator, levels = c(
      "Nutzung digitaler Medien für private Zwecke",
      "Kenntnis der technischen Seite digitaler Tools",
      "Nutzung digitaler Medien für selbständiges Lernen",
      "Kenntnis der Auswirkungen und Gefahren digitaler Medien",
      "Nutzung digitaler Medien für schulische Aufträge")))

digi_schule_p <- p2 %>% 
  group_by(profil)%>% 
  filter(!is.na(profil)) %>%  
  select(profil, year, starts_with("schule"))%>% 
  group_by(year, profil) %>% 
  summarise(across(starts_with("schule"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("schule"), names_to = "indicator", values_to = "mean") %>%
  mutate(indicator = case_when
         (indicator == "schule_zwecke" ~ "Nutzung digitaler Medien für private Zwecke",
           indicator == "schule_aufträge" ~ "Nutzung digitaler Medien für schulische Aufträge",
           indicator == "schule_selbständig" ~ "Nutzung digitaler Medien für selbständiges Lernen",
           indicator == "schule_gefahren" ~ "Kenntnis der Auswirkungen und Gefahren digitaler Medien",
           indicator == "schule_tools" ~ "Kenntnis der technischen Seite digitaler Tools"),
         text = paste0(profil, ": ", round(mean, 2)),
         profil = factor(profil, levels = c("Altsprachlich", "Mathematisch-Naturwissenschaftlich", "Neusprachlich", "Wirtschaftlich-Rechtlich","Musisch", "Total")),
         indicator = factor (indicator, levels = c("Nutzung digitaler Medien für private Zwecke", "Kenntnis der technischen Seite digitaler Tools", "Nutzung digitaler Medien für selbständiges Lernen", "Kenntnis der Auswirkungen und Gefahren digitaler Medien", "Nutzung digitaler Medien für schulische Aufträge"))) 

digi_schule <- bind_rows(digi_schule_p, digi_schule_t)

plot_6 <-ggplot(digi_schule, aes(x=indicator, y=mean, colour=profil, tooltip = text, data_id = profil)) +
  geom_point_interactive(size=2) +
  facet_wrap(~ year, ncol = 1) +
  coord_flip() +
  geom_jitter(width = 0.02, height = 0.02) +
  scale_y_continuous(limits = c(1, 6.1), breaks = seq(1, 6.1, 1), 
                     labels = function(y) str_wrap(c("1 sehr wenig", "2", "3", "4", "5", "6 sehr viel"), 10)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 8, angle = 90), 
        axis.text.y = element_text(size = 8),  
        axis.title = element_blank(), legend.position = "bottom", legend.justification = "right", legend.title = element_blank())+
  guides(color = guide_legend(ncol = 2))

girafe(ggobj = plot_6, height_svg = 5) %>% 
  girafe_options(opts_hover("opacity:0.7;"),opts_zoom(max = 10), opts_tooltip(use_fill =TRUE))

## Abbildung: Nutzung digitaler Medien
### nach Profil 2021 und 2024

{r, plot}

data_digi <- p2 %>% 
  select(year, profil, digi_erk, digi_ueb_anw, digi_produkte, digi_gruppen_dis, digi_plan, digi_pruef) %>% 
  group_by(profil, year) %>% 
  summarise_all(mean, na.rm=T)%>% 
  pivot_longer(cols=3:8, names_to = "indicator", values_to = "mean") %>% 
  mutate(indicator = case_when
         (indicator == "digi_erk" ~ "etwas gezeigt oder erklärt zu bekommen",
           indicator == "digi_ueb_anw" ~ "um Gelerntes zu üben und anzuwenden",
           indicator == "digi_produkte" ~ "um eigene Produkte herzustellen",
           indicator == "digi_gruppen_dis" ~ "für Gruppenarbeiten und Diskussionen",
           indicator == "digi_plan" ~ "für die Schule zu planen und zu organisieren",
           indicator == "digi_pruef" ~ "für Lernkontrollen und Prüfungen"))

data_2 <- filter(data_digi, !is.na(profil))

plot <- ggplot (data_2, aes(x=indicator, y=mean, colour=profil)) +
  geom_point(size=1.5) +
  facet_wrap(~ year, ncol = 1) +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  scale_y_continuous(limits = c(1, 6.1), breaks = seq(1, 6.1, 1), 
                     labels = function(y) str_wrap(c("1 trifft überhaupt nicht zu", "2", "3", "4", "5", "6 trifft voll und ganz zu"), 10)) +
  biplaR::getTheme(c('no_axis_title', 'no_legend_title')) +
  geom_jitter(width = 0, height = 0) +
  scale_colour_manual(values = biplaR::getColourZH(14))+
  ggtitle("Wir verwenden digitale Medien im Unterricht...")+
  theme(plot.title = element_text(size=13, hjust = 2, vjust=-1), axis.text.y = element_text(size = 8), legend.position = "bottom")


girafe(ggobj = plot, height_svg = 5) %>% 
  girafe_options(opts_hover("opacity:0.7;"),opts_zoom(max = 10), opts_tooltip(use_fill =TRUE))


## Abbildung: Nutzung digitaler Medien 2021 und 2024 nach Profil

digi_schule_t <- p2 %>%
  filter(!is.na(profil)) %>%
  select(year, starts_with("schule")) %>%
  group_by(year) %>% 
  summarise(across(starts_with("schule"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("schule"), names_to = "indicator", values_to = "mean") %>%
  mutate(
    profil = "Total",
    indicator = case_when(
      indicator == "schule_zwecke" ~ "Nutzung digitaler Medien für private Zwecke",
      indicator == "schule_aufträge" ~ "Nutzung digitaler Medien für schulische Aufträge",
      indicator == "schule_selbständig" ~ "Nutzung digitaler Medien für selbständiges Lernen",
      indicator == "schule_gefahren" ~ "Kenntnis der Auswirkungen und Gefahren digitaler Medien",
      indicator == "schule_tools" ~ "Kenntnis der technischen Seite digitaler Tools",
      TRUE ~ indicator),
    text = paste0(indicator, ": ", round(mean, 2)),
    profil = fct_relevel(profil, "Total"),
    indicator = factor(indicator, levels = c(
      "Nutzung digitaler Medien für private Zwecke",
      "Kenntnis der technischen Seite digitaler Tools",
      "Nutzung digitaler Medien für selbständiges Lernen",
      "Kenntnis der Auswirkungen und Gefahren digitaler Medien",
      "Nutzung digitaler Medien für schulische Aufträge")))

digi_schule_p <- p2 %>% 
  filter(!is.na(profil)) %>%  
  select(profil, year, starts_with("schule"))%>% 
  group_by(year, profil) %>% 
  summarise(across(starts_with("schule"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("schule"), names_to = "indicator", values_to = "mean") %>%
  mutate(indicator = case_when
         (indicator == "schule_zwecke" ~ "Nutzung digitaler Medien für private Zwecke",
           indicator == "schule_aufträge" ~ "Nutzung digitaler Medien für schulische Aufträge",
           indicator == "schule_selbständig" ~ "Nutzung digitaler Medien für selbständiges Lernen",
           indicator == "schule_gefahren" ~ "Kenntnis der Auswirkungen und Gefahren digitaler Medien",
           indicator == "schule_tools" ~ "Kenntnis der technischen Seite digitaler Tools"),
         text = paste0(indicator, ": ", round(mean, 2)),
         profil = factor(profil, levels = c("Altsprachlich", "Mathematisch-Naturwissenschaftlich", "Neusprachlich", "Wirtschaftlich-Rechtlich","Musisch", "Total")),
         indicator = factor (indicator, levels = c("Nutzung digitaler Medien für private Zwecke", "Kenntnis der technischen Seite digitaler Tools", "Nutzung digitaler Medien für selbständiges Lernen", "Kenntnis der Auswirkungen und Gefahren digitaler Medien", "Nutzung digitaler Medien für schulische Aufträge"))) 

digi_schule <- bind_rows(digi_schule_p, digi_schule_t)

plot <- ggplot(digi_schule, aes(x=year, y=mean, group=indicator, colour=indicator,
                                tooltip = text, data_id = indicator))+
  geom_line_interactive(size = 0.6) +
  geom_point_interactive(size = 1) +
  facet_wrap(~ profil, nrow=2, labeller = label_wrap_gen(width = 30)) + 
  scale_colour_manual(values = c("#3FB8B8","#d84f60","#EBD760", "#7ABF7F","#9a83a7")) +
  scale_y_continuous(limits = c(1,6), breaks = seq(1, 6, 1), labels = function(y) str_wrap(c("1 sehr unzufrieden", "2", "3", "4", "5", "6 sehr zufrieden"), 10)) +
  theme(
    panel.grid.major.y = element_line(color = "darkgrey", linewidth =  0.05),      # Optional: keep minor vertical lines
    panel.grid.major.x = element_blank(),                  # Remove horizontal grid lines
    panel.grid.minor.x = element_blank(),                  # Remove minor horizontal grid lines
    panel.background = element_rect(fill = "transparent"), # Transparent plot background
    plot.background = element_rect(fill = "transparent"),  # Transparent overall background
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 8, angle = 90), 
    axis.text.y = element_text(size = 8),
    axis.title = element_blank(),
    legend.text = element_text(size = 9),
    strip.text.x = element_text(size = 11, angle = 0, margin = margin(0.7, 0, 0.7, 0, "cm")),
    strip.background = element_rect(fill = "grey90", colour = "white") # Grey background for facet title
  ) +
  guides(color = guide_legend(nrow = 3))

girafe(ggobj= plot, pointsize = 12)

## Abbildung: Nutzungen digitaler Medien

data_digi <- p2 %>% 
  select(year, profil, digi_erk, digi_ueb_anw, digi_produkte, digi_gruppen_dis, digi_plan, digi_pruef) %>% 
  group_by(profil, year) %>% 
  summarise_all(mean, na.rm=T)%>% 
  pivot_longer(cols=3:8, names_to = "indicator", values_to = "mean") %>% 
  mutate(indicator = case_when
         (indicator == "digi_erk" ~ "etwas gezeigt oder erklärt zu bekommen",
           indicator == "digi_ueb_anw" ~ "um Gelerntes zu üben und anzuwenden",
           indicator == "digi_produkte" ~ "um eigene Produkte herzustellen",
           indicator == "digi_gruppen_dis" ~ "für Gruppenarbeiten und Diskussionen",
           indicator == "digi_plan" ~ "für die Schule zu planen und zu organisieren",
           indicator == "digi_pruef" ~ "für Lernkontrollen und Prüfungen"),
         text = paste0(indicator, ":", round(mean, )))

data_digi <- filter(data_digi, !is.na(profil))

plot <- ggplot(data_digi, aes(x=year, y=mean, group=indicator, colour=indicator,
                              tooltip = text, data_id = indicator))+
  geom_line_interactive(size = 0.6) +
  geom_point_interactive(size = 1) +
  facet_wrap(~ profil, nrow=2, labeller = label_wrap_gen(width = 30)) + 
  scale_y_continuous(limits = c(1,6), breaks = seq(1, 6, 1), labels = function(y) str_wrap(c("1 sehr unzufrieden", "2", "3", "4", "5", "6 sehr zufrieden"), 10)) +
  theme(
    panel.grid.major.y = element_line(color = "darkgrey", linewidth =  0.05),      # Optional: keep minor vertical lines
    panel.grid.major.x = element_blank(),                  # Remove horizontal grid lines
    panel.grid.minor.x = element_blank(),                  # Remove minor horizontal grid lines
    panel.background = element_rect(fill = "transparent"), # Transparent plot background
    plot.background = element_rect(fill = "transparent"),  # Transparent overall background
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 8, angle = 90), 
    axis.text.y = element_text(size = 8),
    axis.title = element_blank(),
    legend.text = element_text(size = 9),
    strip.text.x = element_text(size = 11, angle = 0, margin = margin(0.7, 0, 0.7, 0, "cm")),
    strip.background = element_rect(fill = "grey90", colour = "white") # Grey background for facet title
  ) +
  guides(color = guide_legend(nrow = 3))

girafe(ggobj = plot, height_svg = 5) %>% 
  girafe_options(opts_hover("opacity:0.7;"),opts_zoom(max = 10), opts_tooltip(use_fill =TRUE))
