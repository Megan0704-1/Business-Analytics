library(seminr)
library(dplyr)
security_score <- read.csv("data/security_data_sem.csv")

#a.
# PLS-PM
# sec composite
sec_mm <- constructs(
  composite("TRUST", multi_items("TRST", 1:4)),
  composite("SEC", multi_items("PSEC", 1:4)),
  composite("REP", multi_items("PREP", 1:4)),
  composite("INV", multi_items("PINV", 1:3)),
  composite("POL", multi_items("PPSS", 1:3)),
  composite("FAML", single_item("FAML1")),
  interaction_term(iv="REP", moderator="POL", method=orthogonal)
  )

sec_sm <- relationships(
  paths(from=c("REP","INV","POL","FAML", "REP*POL"),
        to = "SEC"),
  paths(from="SEC",
        to = "TRUST")
)

#b. PLS-SEM model
sec_pls <- estimate_pls(
  data = security_score,
  measurement_model = sec_mm,
  structural_model = sec_sm
)

sec_pls$path_coef

sec_pls_summary <- summary(sec_pls)

# i.
plot(sec_pls)
# ii.
# weights
sec_pls_summary$weights
# adding these weights together gets the scores of the entire construct
sec_pls_summary$loadings
# the correlation between the composite and the construct
# VIF
sec_pls_summary$validity$vif_items
#iii.
sec_pls_summary$paths
# where the path fits the R^2

sec_pls_summary$descriptives$statistics$constructs
sec_pls_summary$descriptives$correlations$constructs

# iv.
boot_sec_pls <- bootstrap_model(
  seminr_model = sec_pls,
  nboot = 1000,
  seed = 42
)
# a matrix of the bootstrap path coefficients and standard deviations
boot_sec_pls$paths_descriptives
boot_sec_pls$total_paths_descriptives

boot_sec_pls_summary <- summary(boot_sec_pls)
#reports a matrix of direct paths and their standard deviation, t_values, and confidence intervals.
boot_sec_pls_summary$bootstrapped_paths
#reports a matrix of total paths and their standard deviation, t_values, and confidence intervals.
boot_sec_pls_summary$bootstrapped_total_paths

# evaluate the mean estimate, standard deviation, t_value and confidence intervals for specific paths
# from sec to trust
specific_effect_significance(boot_sec_pls, 
                             from = "SEC",
                             to = "TRUST",
                             alpha = 0.05
                             )

#2
sec_mm_reflective <- as.reflective(sec_mm)
#We often wish to conduct a CFA of our measurement model prior to CBSEM
sec_cfa <- estimate_cfa(data=security_score,
                        sec_mm_reflective)
sec_cfa_summary <- summary(sec_cfa)
sec_cfa_summary$descriptives$correlations$constructs
plot(sec_cfa)

# loading
sec_cfa_summary$loadings$significance


sec_cbsem <- estimate_cbsem(data=security_score,
                            sec_mm_reflective,
                            sec_sm)
sec_cbsem_summary <- summary(sec_cbsem)
sec_cbsem_summary$descriptives$correlations$constructs
plot(sec_cbsem)
# composite loadings
sec_cbsem_summary$loadings$significance

# regression coeff of paths between factors and p-values
sec_cbsem_summary$paths$significance
