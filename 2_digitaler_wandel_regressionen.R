## Multiple Regressionen Year * Profil

library(devtools)
library(ggiraph)

devtools::load_all("../biplaRseb")

pool24 <- get_seb(2024) %>%
  dplyr::select(
    type, school, profil, lgkg,
    activity, age, sex,
    zufrieden, vorb_fach, vorb_pers, vorb_selbst,
    
    ict = stand_ict, starts_with("tis_")) %>%
  dplyr::mutate(year = 2024)

pool21 <- get_seb(2021) %>%
  dplyr::select(
    type, school, profil, lgkg,
    activity, age, sex,
    zufrieden, vorb_fach, vorb_pers, vorb_selbst,
    ict = stand_ict, starts_with("tis_")) %>%
  dplyr::mutate(year = 2021)

p2 <- rbind(pool24, pool21) %>% 
  rename(digi_effizient = tis_unt_eff,
         digi_besser_lernen = tis_unt_lern,
         digi_spannender = tis_unt_span,
         lehrp_beraten = tis_lehrp_bera,
         lehrp_passend = tis_lehrp_faecher,
         lehrp_mitarbeit = tis_lehrp_konz,
         lehrp_positiv = tis_lehrp_pos,
         lehrp_umgang = tis_lehrp_umge,
         digi_erk = tis_verw_erkl,
         digi_ueb_anw = tis_verw_lern,
         digi_produkte = tis_verw_prod,
         digi_gruppen_dis = tis_verw_disk,
         digi_plan = tis_verw_plan,
         digi_pruef = tis_verw_kont,
         schule_gefahren = tis_lern_gefahr,
         schule_zwecke = tis_lern_zweck,
         schule_aufträge = tis_lern_schul,
         schule_tools = tis_lern_tools,
         schule_selbständig = tis_lern_selbst,
         vor_andere = tis_vorr_andere,
         vor_eltern = tis_vorr_eltern,
         vor_inter = tis_vorr_inter,
         vor_software = tis_vorr_soft,
         vor_support = tis_vorr_support,
         vor_lehrperson = tis_vorr_unterstlp)

## ANOVA with post-hoc tests

anova <- function(data, response_var) {formula <- as.formula(paste(response_var, "~ year + profil"))
  model <- aov(formula, data = data)
  return(summary(model))}

results <- anova(p2, "digi_effizient")
print(results)

## ANOVA with post-hoc tests

anova <- function(data, response_var) {
  formula <- as.formula(paste(response_var, "~ year + profil"))
  model <- aov(formula, data = data)
  anova_summary <- summary(model)
  tukey <- TukeyHSD(model, which = c("year", "profil"))
  return(list(
    anova = anova_summary,
    posthoc = tukey
  ))}

anova <- function(data, response_var) {
  # Ensure year and profil are factors
  data$year <- as.factor(data$year)
  data$profil <- as.factor(data$profil)
  
  # Create the formula
  formula <- as.formula(paste(response_var, "~ year + profil"))
  
  # Fit the ANOVA model
  model <- aov(formula, data = data)
  
  # Summary of ANOVA
  anova_summary <- summary(model)
  
  # Post-hoc Tukey HSD tests
  tukey <- TukeyHSD(model, which = c("year", "profil"))
  
  # Return both
  return(list(
    anova = anova_summary,
    posthoc = tukey
  ))
}
(digi_effizient = tis_unt_eff,
  digi_besser_lernen = tis_unt_lern,
  digi_spannender = tis_unt_span,
  lehrp_beraten = tis_lehrp_bera,
  lehrp_passend = tis_lehrp_faecher,
  lehrp_mitarbeit = tis_lehrp_konz,
  lehrp_positiv = tis_lehrp_pos,
  lehrp_umgang = tis_lehrp_umge,
  digi_erk = tis_verw_erkl,
  digi_ueb_anw = tis_verw_lern,
  digi_produkte = tis_verw_prod,
  digi_gruppen_dis = tis_verw_disk,
  digi_plan = tis_verw_plan,
  digi_pruef = tis_verw_kont,
  schule_gefahren = tis_lern_gefahr,
  schule_zwecke = tis_lern_zweck,
  schule_aufträge = tis_lern_schul,
  schule_tools = tis_lern_tools,
  schule_selbständig = tis_lern_selbst,
  vor_andere = tis_vorr_andere,
  vor_eltern = tis_vorr_eltern,
  vor_inter = tis_vorr_inter,
  vor_software = tis_vorr_soft,
  vor_support = tis_vorr_support,
  vor_lehrperson = tis_vorr_unterstlp)



results <- anova(p2, "digi_effizient")
resutls <- anova(p2, "digi_besser_lernen")
results <- anova(p2, "digi_spannender")
results <- anova(p2, "lehrp_beraten")
results <- anova(p2, "digi_erk")
results <- anova(p2, "digi_ueb_anw")

# Print ANOVA results
print(results$anova)

# Print Tukey HSD post-hoc results
print(results$posthoc)

## Interaktion

anova_i <- function(data, response_var) {
  # Ensure factors
  data$year <- as.factor(data$year)
  data$profil <- as.factor(data$profil)
  
  # Model with interaction
  formula <- as.formula(paste(response_var, "~ year * profil"))
  
  # Run ANOVA
  model <- aov(formula, data = data)
  anova_summary <- summary(model)
  
  # Post-hoc Tukey HSD only works on main effects and interactions that are factors
  tukey <- TukeyHSD(model)  # Includes all factor terms if they're valid
  
  return(list(
    anova = anova_summary,
    posthoc = tukey))}

results_i <- anova(p2, "digi_ueb_anw")
print(results_i)

modell <- lm(digi_ueb_anw ~ profil + year, data = p2)
summary(modell)


## T-Tests

musisch <- filter(p2, profil %in% "Musisch")

t.test(digi_effizient ~ year, data = musisch)
t.test(digi_besser_lernen ~ year, data = musisch)

plot(uptake ~ Treatment, data=CO2)

## LME

install.packages("nlme")
library(nlme)

nutzen <- c("digi_effizient", "digi_besser_lernen", "digi_spannender")
verwendung <- c("digi_erk", "digi_ueb_anw", "digi_produkte", "digi_gruppen_dis", "digi_plan", "digi_pruef")

p2 <- p2 %>%
  rowwise() %>%
  mutate(mean_vor = mean(c_across(starts_with("vor_")), na.rm = TRUE),
         mean_nutzen = mean(c_across(all_of(nutzen)), na.rm = TRUE),
         mean_lehrperson = mean(c_across(starts_with("lehrp_")), na.rm = TRUE),
         mean_verwendung = mean(c_across(all_of(verwendung)), na.rm = TRUE),
         mean_beitrag = mean(c_across(starts_with("schule_")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(profil))

# Random effect for subjects
lme_model <- lm(mean_vor ~ year + profil, data = p2)
summary(lme_model)
anova(lme_model)

## 
df$Group <- as.factor(df$Group)
df$Time <- as.factor(df$Time)


# Results: signifikanter Unterschied: Profil, Jahr, n.s.: Interaktion
# Run two-way ANOVA
model <- aov(mean_vor ~ profil * year, data = p2)
summary(model)

## All results are significant
model <- aov(mean_nutzen~ profil * year, data = p2)
summary(model)

## Only year is significant
model <- aov(mean_lehrperson ~ profil * year, data = p2)
summary(model)

## Profil and year are significant, interaction is not
model <- aov(mean_verwendung ~ profil * year, data = p2)
summary(model)

## Profil and year are significant
model <- aov(mean_beitrag ~ profil * year, data =p2)
summary(model)

