## Skalenprüfung

## load necessary packages
library(devtools)
library(ggiraph)
library(biplaR)

## install necessary packages
library(psych)
library(RcmdrMisc)
library(openxlsx)
library(lavaan)
library(semPlot)
library(mice)
library(naniar)

## load data
devtools::load_all("../biplaRseb")
seb24 <- get_seb(2024, db = F) %>% 
  filter(type %in% "Gymi")

## 1
## dataframe with all relevant variables
fa <- select(seb24, starts_with(c("zufrieden", "vorb_", "schul_", "tis_", "stmk_", "stpk_", "stsk_", "blsb_", "flern_", "polit_", "zlern_")), laufbahnu)

# See how many missing values per variable
sort(colSums(is.na(fa)), decreasing = TRUE)
colSums(is.na(fa))  
sum(is.na(fa))  

# Visualise missingness
vis_miss(fa)

# Tabular missing pattern
tibble <- md.pattern(fa)

## Exclude columns with too many missings

fa <- fa %>% 
        select(-blsb_bes, blsb_schnu, tis_vorr_support)

# impute NA
method <- make.method(fa)
method <- rep("pmm", ncol(fa))
names(method) <- names(fa)

# Run mice imputation
imp <- mice(fa, m = 5, method = method, seed = 123)

# Get the first completed dataset
fa <- complete(imp, 1)

# Optional: check imputed values
summary(imp)

## Check requirements
KMO(cor(fa))        # KMO measure
cortest.bartlett(cor(fa), n = nrow(fa))  

## EFA
ev <- eigen(cor(fa))
ev$values
scree(fa, pc=FALSE) 
plot(ev$values, type = "b", xlab = "Factor number", ylab = "Eigenvalue",
     main = "Scree Plot")
abline(h=1, col="red", lty=2)

## Double check
fa.parallel(fa, fa="fa")

## define number of factors
Nfacs <- 15
fit <- factanal((fa), Nfacs, rotation="promax")
fit <- fa(fa, nfactors = Nfacs, rotate = "oblimin")
print(fit)
print(fit, digits=2, cutoff=0.3, sort=TRUE)

# Compare factor loadings side by side
print(fit$loadings, cutoff = 0.3)        # oblimin loadings
print(fit_promax$loadings, cutoff = 0.3) # promax loadings

# Compare factor correlations
fit$Phi        # correlations from oblimin
fit_promax$Phi # correlations from promax

# Optional: visualize loadings with a simple heatmap
library(corrplot)
corrplot(abs(fit$loadings[,]), is.corr = FALSE, title = "Oblimin Loadings")
corrplot(abs(fit_promax$loadings[,]), is.corr = FALSE, title = "Promax Loadings")

## Extract loadings as a matrix and convert to data frame
loadings_df <- as.data.frame.matrix(unclass(fit$loadings))
loadings_mat <- unclass(fit$loadings)  

primary_factor_df <- data.frame(
  Variable = rownames(loadings_mat),
  Factor = colnames(loadings_mat)[apply(abs(loadings_mat), 1, which.max)],
  Loading = apply(loadings_mat, 1, function(x) x[which.max(abs(x))]),
  row.names = NULL)

## Save results in an excel
wb <- createWorkbook()
addWorksheet(wb, "Variable-Factor Mapping")
writeData(wb, "Variable-Factor Mapping", primary_factor_df)
saveWorkbook(wb, "variable_factor_mapping.xlsx", overwrite = TRUE)

## Optimize model by excluding variables with high uniqueness

fa <- fa %>% 
       select(-tis_vorr_andere, -tis_vorr_eltern, -blsb_thema, -blsb_schnu, -blsb_gespr, -blsb_serious, -tis_vorr_support, -tis_lern_gefahr)

## results of optimized model
## EFA
ev <- eigen(cor(fa))
ev$values
scree(fa, pc=FALSE) 
plot(ev$values, type = "b", xlab = "Factor number", ylab = "Eigenvalue",
     main = "Scree Plot")
abline(h=1, col="red", lty=2)

## define number of factors
Nfacs <- 15
fit <- factanal((fa), Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

## Extract loadings as a matrix and convert to data frame
loadings_df <- as.data.frame.matrix(unclass(fit$loadings))
loadings_mat <- unclass(fit$loadings)  

primary_factor_df <- data.frame(
  Variable = rownames(loadings_mat),
  Factor = colnames(loadings_mat)[apply(abs(loadings_mat), 1, which.max)],
  Loading = apply(loadings_mat, 1, function(x) x[which.max(abs(x))]),
  row.names = NULL)

## Save results in an excel
wb <- createWorkbook()
addWorksheet(wb, "Variable-Factor Mapping")
writeData(wb, "Variable-Factor Mapping", primary_factor_df)
saveWorkbook(wb, "variable_factor_mapping.xlsx", overwrite = TRUE)

## Optimize model again

fa <- fa %>% 
  select(-zlern_org, -zlern_selbst, -tis_unt_span, -tis_lern_tools, -stpk_schw, -zlern_ideen, -schul_konfl)

## results of optimized model
## EFA
ev <- eigen(cor(fa))
ev$values
scree(fa, pc=FALSE) 
plot(ev$values, type = "b", xlab = "Factor number", ylab = "Eigenvalue",
     main = "Scree Plot")
abline(h=1, col="red", lty=2)

## define number of factors
Nfacs <- 10
fit <- factanal((fa), Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

## Extract loadings as a matrix and convert to data frame
loadings_df <- as.data.frame.matrix(unclass(fit$loadings))
loadings_mat <- unclass(fit$loadings)  

primary_factor_df <- data.frame(
  Variable = rownames(loadings_mat),
  Factor = colnames(loadings_mat)[apply(abs(loadings_mat), 1, which.max)],
  Loading = apply(loadings_mat, 1, function(x) x[which.max(abs(x))]),
  row.names = NULL)

## Save results in an excel
wb <- createWorkbook()
addWorksheet(wb, "Variable-Factor Mapping")
writeData(wb, "Variable-Factor Mapping", primary_factor_df)
saveWorkbook(wb, "variable_factor_mapping_2.xlsx", overwrite = TRUE)

# plot factor 1 and 2
# Assuming your factanal result is stored in 'fa_result'
fa_result <- factanal(x = fa, factors = 10, rotation = "promax")

# Extract loadings
loadings <- fa_result$loadings[, 1:2]  # first two factors

# Basic plot of loadings on Factor 1 vs Factor 2
plot(loadings, type = "n", xlab = "Factor 1", ylab = "Factor 2",
     main = "Factor Loadings Plot (Factors 1 & 2)")
text(loadings, labels = rownames(loadings), cex = 0.7)
abline(h = 0, v = 0, col = "gray")


## 2
## dataframe excluding variables starting with "schul_"
fa <- select(seb24, starts_with(c("zufrieden", "vorb_", "tis_", "stmk_", "stpk_", "stsk_", "blsb_", "flern_", "polit_", "zlern_")), laufbahnu)

# See how many missing values per variable
sort(colSums(is.na(fa)), decreasing = TRUE)
colSums(is.na(fa))  
sum(is.na(fa))  

## Exclude columns with too many missings
fa <- fa %>% 
  select(-blsb_bes, blsb_schnu, -blsb_hilf, -blsb_gespr, tis_vorr_support)

# impute NA
method <- make.method(fa)
method <- rep("pmm", ncol(fa))
names(method) <- names(fa)

# Run mice imputation
imp <- mice(fa, m = 5, method = method, seed = 123)

# Get the first completed dataset
fa <- complete(imp, 1)

# Optional: check imputed values
summary(imp)

## Check requirements
KMO(cor(fa))        # KMO measure
cortest.bartlett(cor(fa), n = nrow(fa))  

## EFA
ev <- eigen(cor(fa))
ev$values
scree(fa, pc=FALSE) 
plot(ev$values, type = "b", xlab = "Factor number", ylab = "Eigenvalue",
     main = "Scree Plot")
abline(h=1, col="red", lty=2)

## Double check
fa.parallel(fa, fa="fa")

## define number of factors
Nfacs <- 16
fit <- factanal((fa), Nfacs, rotation="promax")
fit <- fa(fa, nfactors = Nfacs, rotate = "oblimin")
print(fit)
print(fit, digits=2, cutoff=0.3, sort=TRUE)


fit_promax <- fa(fa, nfactors = 3, rotate = "promax")

# Compare factor loadings side by side
print(fit$loadings, cutoff = 0.3)        # oblimin loadings
print(fit_promax$loadings, cutoff = 0.3) # promax loadings

# Compare factor correlations
fit$Phi        # correlations from oblimin
fit_promax$Phi # correlations from promax

# Optional: visualize loadings with a simple heatmap
library(corrplot)
corrplot(abs(fit$loadings[,]), is.corr = FALSE, title = "Oblimin Loadings")
corrplot(abs(fit_promax$loadings[,]), is.corr = FALSE, title = "Promax Loadings")

## Extract loadings as a matrix and convert to data frame
loadings_df <- as.data.frame.matrix(unclass(fit$loadings))
loadings_mat <- unclass(fit$loadings)  

primary_factor_df <- data.frame(
  Variable = rownames(loadings_mat),
  Factor = colnames(loadings_mat)[apply(abs(loadings_mat), 1, which.max)],
  Loading = apply(loadings_mat, 1, function(x) x[which.max(abs(x))]),
  row.names = NULL)

## Save results in an excel
wb <- createWorkbook()
addWorksheet(wb, "Variable-Factor Mapping")
writeData(wb, "Variable-Factor Mapping", primary_factor_df)
saveWorkbook(wb, "variable_factor_mapping.xlsx", overwrite = TRUE)

## Build model CFA

model <- '
  Factor1 =~ flern_art + flern_atm + flern_kont + flern_mat + flern_meth + flern_pruef + flern_refle + flern_temp + flern_was + flern_zusa
  Factor2 =~ tis_lern_schul + tis_lern_selbst + tis_lern_zweck + tis_unt_eff + tis_unt_lern + tis_verw_disk + tis_verw_erkl + tis_verw_kont + tis_verw_lern + tis_verw_plan + tis_verw_prod
  Factor3 =~ blsb_ext + blsb_hilf + blsb_mittel + blsb_unt + blsb_wahl + blsb_zeit + laufbahnu
  Factor4 =~ schul_eigen + schul_frag + schul_gruppe + schul_idee + schul_info + schul_kritik + schul_prae + schul_schw + schul_selbt + schul_urteil + schul_wort +  polit_grund + polit_inter
  Factor5 =~ tis_lehrp_bera + tis_lehrp_faecher + tis_lehrp_konz + tis_lehrp_pos + tis_lehrp_umge + tis_vorr_inter + tis_vorr_soft + tis_vorr_unterstlp
  Factor6 =~ stmk_info + stmk_prae + stmk_strate + stmk_wort + stpk_frag + stpk_idee + stpk_urteil + stsk_gruppe + stsk_konfl + stsk_kritik + stsk_zusamm
  Factor7 =~ schul_achte + schul_rueck + schul_zusamm + stsk_achte + stsk_rueck 
  Factor8 =~ schul_plan + schul_strate + schul_ueber + schul_ziel + stmk_plan + stmk_ziel
  Factor9 =~ schul_time + schul_eigen + schul_selbt + stpk_time
  Factor10 =~ schul_it + stmk_it'

fit <- cfa(model, data = seb24, estimator = "MLR")  # MLR is robust maximum likelihood

## Fit indices: RMSEA and SRMR okay, CFI & TLI: not met

fitMeasures(fit, c("chisq", "df", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "cfi", "tli", "srmr"))

fit <- cfa(model, data = seb24)  
mod_indices <- modificationIndices(fit)

mod_indices %>%
  dplyr::filter(mi > 10) %>%
  dplyr::arrange(desc(mi))
