## Test for differences over time

p <- p  %>% 
       filter(!profil %in% c("Lehramt", "Lehramtsschule", "Philosophie/Pädagogik/Psychologie")) %>% 
       filter(year %in% c(2021, 2024))

# Fit mixed model
p$profil <- as.factor(p$profil)
p$year <- as.factor(p$year)

model <- aov(zufrieden ~ profil *year, data = p)
summary(model)
tukey <- TukeyHSD(model)
print(tukey)

model <- aov(vorb_fach ~ profil * year, data = p)
summary(model)
tukey <- TukeyHSD(model)
print(tukey)

model <- aov(vorb_pers ~ profil *year, data = p)
summary(model)

model <- aov(vorb_selbst~ profil * year, data = p)
summary(model)

## Nur 2024

p2 <- p2 %>% 
      filter(year %in% 2024 & type %in% "Gymi")

model <- aov(zufrieden ~ profil, data = p2)
summary(model)

p %>% 
  ggplot(aes(x = profil, y = zufrieden)) +
  geom_boxplot()

## Überfachliche Kompetenzen

d2 <- select(sebpool, year, profil, personal, social, meth, it) %>% 
  filter(!is.na(profil)) %>% 
  group_by(year, profil) %>% 
  filter(year %in% c(2021, 2024))

model <- aov(personal ~ profil *year, data = d2)
summary(model)

model <- aov(social ~ profil * year, data = d2)
summary(model)

model <- aov(meth ~ profil *year, data = d2)
summary(model)

model <- aov(it ~ profil * year, data = d2)
summary(model)

## Unterstützung Studien-, Berufs- und Laufbahnberatung

seb <- sebpool_II %>%  
           filter(year %in% c(2021, 2024))

seb$year <- as.factor(seb$year)
seb$profil <- as.factor(seb$profil)

model <- aov(laufbahn ~ profil*year, data = seb)
summary(model)

tukey <- TukeyHSD(model)
tibble <- tukey

tukey_results <- as.data.frame(tukey$group)

# Add comparison names
tukey_results$Comparison <- rownames(tukey_results)

# Filter only significant results (p adj < 0.05)
significant <- filter(tukey_results, `p adj` < 0.05)

if (nrow(significant) > 0) {print(significant[, c("Comparison", "diff", "p adj")])
} else {
  cat("No significant differences.\n")}

## Digitaler Wandel

p2 <- p2 %>%
  rowwise() %>%
  mutate(mean_vor = mean(c_across(starts_with("vor_")), na.rm = TRUE),
         mean_nutzen = mean(c_across(all_of(nutzen)), na.rm = TRUE),
         mean_lehrperson = mean(c_across(starts_with("lehrp_")), na.rm = TRUE),
         mean_verwendung = mean(c_across(all_of(verwendung)), na.rm = TRUE),
         mean_beitrag = mean(c_across(starts_with("schule_")), na.rm = TRUE)) %>% 
  ungroup()

model <- aov(mean_vor ~ profil *year, data = p2)
summary(model)

model <- aov(mean_nutzen ~profil * year, data = p2)
summary(model)

model <- aov(mean_verwendung ~profil * year, data = p2)
summary(model)

model <- aov(mean_beitrag ~profil * year, data = p2)
summary(model)

model <- aov(mean_lehrperson ~ profil*year, data = p2)
summary(model)

## Namen

names(seb15)
seb09 <- get_seb(2009)
names(seb09)
