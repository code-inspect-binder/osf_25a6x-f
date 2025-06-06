# -------------------------------------------------
# IM & SHANE (2021) SES CAUSAL BELIEFS SCALE
# RELIABILITY, CFA, & ANOVA SCRIPT
# -------------------------------------------------

library(lavaan)
library(jmv)

data = read.csv(file.choose())


#### HIGH SES SOCIETY BELIEFS RELIABILITY ####

# 3 items

#Merit
jmv::reliability(
  data = data,
  vars = vars(Q1A, Q1B, Q1J, Q1E, Q1H, Q1N),
  omegaScale = TRUE)
#a = 0.818, o = 0.821

#Opportunity
jmv::reliability(
  data = data,
  vars = vars(Q1C, Q1D, Q1M, Q1F, Q1K, Q1G),
  omegaScale = TRUE)

#Chance
jmv::reliability(
  data = data,
  vars = vars(Q1I, Q1L, Q1O, Q1P, Q1Q),
  omegaScale = TRUE)



# 2 items

#Merit
jmv::reliability(
  data = data,
  vars = vars(Q1A, Q1B, Q1H, Q1N),
  omegaScale = TRUE)
#a = 0.818, o = 0.821

#Opportunity
jmv::reliability(
  data = data,
  vars = vars(Q1C, Q1D, Q1F, Q1G),
  omegaScale = TRUE)

#Chance
jmv::reliability(
  data = data,
  vars = vars(Q1I, Q1O, Q1P, Q1Q),
  omegaScale = TRUE)


#### LOW SES SOCIETY BELIEFS RELIABILITY ####

# 3 items

#Merit
jmv::reliability(
  data = data,
  vars = vars(Q2A, Q2B, Q2J, Q2E, Q2H, Q2N),
  omegaScale = TRUE)

#Opportunity
jmv::reliability(
  data = data,
  vars = vars(Q2C, Q2D, Q2M, Q2F, Q2K, Q2G),
  omegaScale = TRUE)

#Chance
jmv::reliability(
  data = data,
  vars = vars(Q2I, Q2L, Q2O, Q2Q, Q2R),
  omegaScale = TRUE)


# 2 items

#Merit
jmv::reliability(
  data = data,
  vars = vars(Q2A, Q2B, Q2H, Q2N),
  omegaScale = TRUE)

#Opportunity
jmv::reliability(
  data = data,
  vars = vars(Q2C, Q2D, Q2F, Q2G),
  omegaScale = TRUE)

#Chance
jmv::reliability(
  data = data,
  vars = vars(Q2I, Q2O, Q2Q, Q2R),
  omegaScale = TRUE)



#### CURRENT SES AGENCY BELIEFS RELIABILITY ####

# 3 items

#Merit
jmv::reliability(
  data = data,
  vars = vars(Q3A, Q3B, Q3J, Q3E, Q3H, Q3N),
  omegaScale = TRUE)

#Opportunity
jmv::reliability(
  data = data,
  vars = vars(Q3C, Q3D, Q3M, Q3F, Q3K, Q3G),
  omegaScale = TRUE)

#Chance
jmv::reliability(
  data = data,
  vars = vars(Q3I, Q3L, Q3O, Q3P, Q3Q),
  omegaScale = TRUE)


# 2 items

#Merit
jmv::reliability(
  data = data,
  vars = vars(Q3A, Q3B, Q3H, Q3N),
  omegaScale = TRUE)

#Opportunity
jmv::reliability(
  data = data,
  vars = vars(Q3C, Q3D, Q3F, Q3G),
  omegaScale = TRUE)

#Chance
jmv::reliability(
  data = data,
  vars = vars(Q3I, Q3O, Q3P, Q3Q),
  omegaScale = TRUE)



#### BARRIERS TO SES AGENCY BELIEFS RELIABILITY ####

# 3 items

#Merit
jmv::reliability(
  data = data,
  vars = vars(Q4B, Q4C, Q4K, Q4F, Q4I, Q4N),
  omegaScale = TRUE)

#Opportunity
jmv::reliability(
  data = data,
  vars = vars(Q4D, Q4E, Q4M, Q4G, Q4A, Q4H),
  omegaScale = TRUE)

#Chance
jmv::reliability(
  data = data,
  vars = vars(Q4J, Q4L, Q4O, Q4P, Q4Q),
  omegaScale = TRUE)


# 2 items

#Merit
jmv::reliability(
  data = data,
  vars = vars(Q4B, Q4C, Q4I, Q4N),
  omegaScale = TRUE)

#Opportunity
jmv::reliability(
  data = data,
  vars = vars(Q4D, Q4E, Q4G, Q4H),
  omegaScale = TRUE)

#Chance
jmv::reliability(
  data = data,
  vars = vars(Q4J, Q4O, Q4P, Q4Q),
  omegaScale = TRUE)




#### CFA SCALES ####

#HSB 3 ITEMS

jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q1A", "Q1B", "Q1J")),
    list(
      label="Ability",
      vars=c("Q1H", "Q1N", "Q1E")),
    list(
      label="Social Connection",
      vars=c("Q1C", "Q1D", "Q1M")),
    list(
      label="Privilege",
      vars=c("Q1F", "Q1G", "Q1K")),
    list(
      label="Luck",
      vars=c("Q1I", "Q1O", "Q1L")),
    list(
      label="Fate",
      vars=c("Q1P", "Q1Q"))),
  resCov = NULL,
  stdEst = TRUE,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#HSB 2 ITEMS

jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q1A", "Q1B")),
    list(
      label="Ability",
      vars=c("Q1H", "Q1N")),
    list(
      label="Social Connection",
      vars=c("Q1C", "Q1D")),
    list(
      label="Privilege",
      vars=c("Q1F", "Q1G")),
    list(
      label="Luck",
      vars=c("Q1I", "Q1O")),
    list(
      label="Fate",
      vars=c("Q1P", "Q1Q"))),
  resCov = NULL,
  stdEst = TRUE,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"),
  duplicate = 1)

#LSB 3 ITEMS

jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q2A", "Q2B", "Q2J")),
    list(
      label="Ability",
      vars=c("Q2E", "Q2H", "Q2N")),
    list(
      label="Social Connection",
      vars=c("Q2C", "Q2D", "Q2M")),
    list(
      label="Privilege",
      vars=c("Q2F", "Q2G", "Q2K")),
    list(
      label="Luck",
      vars=c("Q2I", "Q2L", "Q2O")),
    list(
      label="Fate",
      vars=c("Q2Q", "Q2R"))),
  resCov = NULL,
  stdEst = TRUE,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#LSB 2 ITEMS

jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q2A", "Q2B")),
    list(
      label="Ability",
      vars=c("Q2H", "Q2N")),
    list(
      label="Social Connection",
      vars=c("Q2C", "Q2D")),
    list(
      label="Privilege",
      vars=c("Q2F", "Q2G")),
    list(
      label="Luck",
      vars=c("Q2I", "Q2O")),
    list(
      label="Fate",
      vars=c("Q2Q", "Q2R"))),
  resCov = NULL,
  stdEst = TRUE,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"),
  duplicate = 5)

#CAB 3 ITEMS

jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q3A", "Q3B", "Q3J")),
    list(
      label="Ability",
      vars=c("Q3E", "Q3H", "Q3N")),
    list(
      label="Social Connection",
      vars=c("Q3C", "Q3D", "Q3M")),
    list(
      label="Privilege",
      vars=c("Q3F", "Q3G", "Q3K")),
    list(
      label="Luck",
      vars=c("Q3I", "Q3L", "Q3O")),
    list(
      label="Fate",
      vars=c("Q3P", "Q3Q"))),
  resCov = NULL,
  stdEst = TRUE,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"),
  duplicate = 5)

#CAB 2 ITEMS

jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q3A", "Q3B")),
    list(
      label="Ability",
      vars=c("Q3H", "Q3N")),
    list(
      label="Social Connection",
      vars=c("Q3C", "Q3D")),
    list(
      label="Privilege",
      vars=c("Q3F", "Q3G")),
    list(
      label="Luck",
      vars=c("Q3I", "Q3O")),
    list(
      label="Fate",
      vars=c("Q3P", "Q3Q"))),
  resCov = NULL,
  stdEst = TRUE,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"),
  duplicate = 5)

#BAB 3 ITEMS

jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q4B", "Q4C", "Q4K")),
    list(
      label="Ability",
      vars=c("Q4F", "Q4I", "Q4N")),
    list(
      label="Social Connection",
      vars=c("Q4D", "Q4E", "Q4M")),
    list(
      label="Privilege",
      vars=c("Q4A", "Q4G", "Q4H")),
    list(
      label="Luck",
      vars=c("Q4J", "Q4L", "Q4O")),
    list(
      label="Fate",
      vars=c("Q4P", "Q4Q"))),
  resCov = NULL,
  stdEst = TRUE,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"),
  duplicate = 5)

#BAB 2 ITEMS

jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q4B", "Q4C")),
    list(
      label="Ability",
      vars=c("Q4I", "Q4N")),
    list(
      label="Social Connection",
      vars=c("Q4D", "Q4E")),
    list(
      label="Privilege",
      vars=c("Q4G", "Q4H")),
    list(
      label="Luck",
      vars=c("Q4J", "Q4O")),
    list(
      label="Fate",
      vars=c("Q4P", "Q4Q"))),
  resCov = NULL,
  stdEst = TRUE,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"),
  duplicate = 5)

#### CFA SUBSCALES ####

#HIGH SES SOCIETY BELIEFS

#Merit Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q1A", "Q1B", "Q1J")),
    list(
      label="Ability",
      vars=c("Q1E", "Q1H", "Q1N"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Merit Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q1A", "Q1B")),
    list(
      label="Ability",
      vars=c("Q1H", "Q1N"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Opportunity Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Social Connections",
      vars=c("Q1C", "Q1D", "Q1M")),
    list(
      label="Privilege",
      vars=c("Q1F", "Q1G", "Q1K"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Opportunity Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Social Connections",
      vars=c("Q1C", "Q1D")),
    list(
      label="Privilege",
      vars=c("Q1F", "Q1G"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))


#Chance Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Luck",
      vars=c("Q1I", "Q1L", "Q1O")),
    list(
      label="Fate",
      vars=c("Q1P", "Q1Q"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Chance Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Luck",
      vars=c("Q1I", "Q1O")),
    list(
      label="Fate",
      vars=c("Q1P", "Q1Q"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"),
  duplicate = 18)




#LOW SES SOCIETY BELIEFS

#Merit Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q2A", "Q2B", "Q2J")),
    list(
      label="Ability",
      vars=c("Q2E", "Q2H", "Q2N"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Merit Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q2A", "Q2B")),
    list(
      label="Ability",
      vars=c("Q2H", "Q2N"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Opportunity Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Social Connections",
      vars=c("Q2C", "Q2D", "Q2M")),
    list(
      label="Privilege",
      vars=c("Q2F", "Q2G", "Q2K"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Opportunity Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Social Connections",
      vars=c("Q2C", "Q2D")),
    list(
      label="Privilege",
      vars=c("Q2F", "Q2G"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))


#Chance Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Luck",
      vars=c("Q2I", "Q2L", "Q2O")),
    list(
      label="Fate",
      vars=c("Q2Q", "Q2R"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Chance Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Luck",
      vars=c("Q2I", "Q2O")),
    list(
      label="Fate",
      vars=c("Q2Q", "Q2R"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"),
  duplicate = 28)




#CURRENT SES AGENCY BELIEFS

#Merit Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q3A", "Q3B", "Q3J")),
    list(
      label="Ability",
      vars=c("Q3E", "Q3H", "Q3N"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Merit Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q3A", "Q3B")),
    list(
      label="Ability",
      vars=c("Q3H", "Q3N"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Opportunity Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Social Connections",
      vars=c("Q3C", "Q3D", "Q3M")),
    list(
      label="Privilege",
      vars=c("Q3F", "Q3G", "Q3K"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Opportunity Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Social Connections",
      vars=c("Q3C", "Q3D")),
    list(
      label="Privilege",
      vars=c("Q3F", "Q3G"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))


#Chance Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Luck",
      vars=c("Q3I", "Q3L", "Q3O")),
    list(
      label="Fate",
      vars=c("Q3P", "Q3Q"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Chance Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Luck",
      vars=c("Q3I", "Q3O")),
    list(
      label="Fate",
      vars=c("Q3P", "Q3Q"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"),
  duplicate = 38)





#BARRIERS TO SES AGENCY BELIEFS

#Merit Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q4B", "Q4C", "Q4K")),
    list(
      label="Ability",
      vars=c("Q4F", "Q4I", "Q4N"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Merit Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Effort",
      vars=c("Q4B", "Q4C")),
    list(
      label="Ability",
      vars=c("Q4I", "Q4N"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Opportunity Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Social Connections",
      vars=c("Q4D", "Q4E", "Q4M")),
    list(
      label="Privilege",
      vars=c("Q4G", "Q4A", "Q4H"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Opportunity Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Social Connections",
      vars=c("Q4D", "Q4E")),
    list(
      label="Privilege",
      vars=c("Q4G", "Q4H"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))


#Chance Three Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Luck",
      vars=c("Q4J", "Q4L", "Q4O")),
    list(
      label="Fate",
      vars=c("Q4P", "Q4Q"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"))

#Chance Two Items
jmv::cfa(
  data = data,
  factors = list(
    list(
      label="Luck",
      vars=c("Q4J", "Q4O")),
    list(
      label="Fate",
      vars=c("Q4P", "Q4Q"))),
  resCov = NULL,
  fitMeasures = c("cfi", "tli", "rmsea", "srmr"),
  duplicate = 48)




#### MEAN DIFFERENCES WITHIN BELIEF SYSTEMS ####

#WITHIN HSB
jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="HSB",
      levels=c(
        "EFFORT",
        "ABILITY",
        "SOCCON",
        "PRIVILEGE",
        "LUCK",
        "FATE"))),
  rmCells = list(
    list(
      measure="hs_efft",
      cell="EFFORT"),
    list(
      measure="hs_abil",
      cell="ABILITY"),
    list(
      measure="hs_socn",
      cell="SOCCON"),
    list(
      measure="hs_priv",
      cell="PRIVILEGE"),
    list(
      measure="hs_luck",
      cell="LUCK"),
    list(
      measure="hs_fate",
      cell="FATE")),
  effectSize = c("eta", "partEta"),
  rmTerms = ~ HSB,
  postHoc = list(
    "HSB"))

#WITHIN LSB
jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="LSB",
      levels=c(
        "EFFORT",
        "ABILITY",
        "SOCCON",
        "PRIVILEGE",
        "LUCK",
        "FATE"))),
  rmCells = list(
    list(
      measure="ls_efft",
      cell="EFFORT"),
    list(
      measure="ls_abil",
      cell="ABILITY"),
    list(
      measure="ls_socn",
      cell="SOCCON"),
    list(
      measure="ls_priv",
      cell="PRIVILEGE"),
    list(
      measure="hs_luck",
      cell="LUCK"),
    list(
      measure="ls_fate",
      cell="FATE")),
  effectSize = c("eta", "partEta"),
  rmTerms = ~ LSB,
  postHoc = list(
    "LSB"))

#WITHIN CAB
jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="CAB",
      levels=c(
        "EFFORT",
        "ABILITY",
        "SOCCON",
        "PRIVILEGE",
        "LUCK",
        "FATE"))),
  rmCells = list(
    list(
      measure="ha_efft",
      cell="EFFORT"),
    list(
      measure="ha_abil",
      cell="ABILITY"),
    list(
      measure="ha_socn",
      cell="SOCCON"),
    list(
      measure="ha_priv",
      cell="PRIVILEGE"),
    list(
      measure="ha_luck",
      cell="LUCK"),
    list(
      measure="ha_fate",
      cell="FATE")),
  effectSize = c("eta", "partEta"),
  rmTerms = ~ CAB,
  postHoc = list(
    "CAB"))

#WITHIN BAB
jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="BAB",
      levels=c(
        "EFFORT",
        "ABILITY",
        "SOCCON",
        "PRIVILEGE",
        "LUCK",
        "FATE"))),
  rmCells = list(
    list(
      measure="la_efft",
      cell="EFFORT"),
    list(
      measure="la_abil",
      cell="ABILITY"),
    list(
      measure="la_socn",
      cell="SOCCON"),
    list(
      measure="la_priv",
      cell="PRIVILEGE"),
    list(
      measure="la_luck",
      cell="LUCK"),
    list(
      measure="la_fate",
      cell="FATE")),
  effectSize = c("eta", "partEta"),
  rmTerms = ~ BAB,
  postHoc = list(
    "BAB"))

#### MEAN DIFFERENCES BETWEEN BELIEF SYSTEMS ####

#MERIT
jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="MERIT",
      levels=c("HSB", "LSB", "CAB", "BAB"))),
  rmCells = list(
    list(
      measure="hs_merit",
      cell="HSB"),
    list(
      measure="ls_merit",
      cell="LSB"),
    list(
      measure="ha_merit",
      cell="CAB"),
    list(
      measure="la_merit",
      cell="BAB")),
  effectSize = c("partEta", "eta"),
  rmTerms = ~ MERIT,
  postHoc = list(
    "MERIT"))

#EFFORT

jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="EFFORT",
      levels=c("HSB", "LSB", "CAB", "BAB"))),
  rmCells = list(
    list(
      measure="hs_efft",
      cell="HSB"),
    list(
      measure="ls_efft",
      cell="LSB"),
    list(
      measure="ha_efft",
      cell="CAB"),
    list(
      measure="la_efft",
      cell="BAB")),
  effectSize = c("partEta", "eta"),
  rmTerms = ~ EFFORT,
  postHoc = list(
    "EFFORT"))

#ABILITY

jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="ABILITY",
      levels=c("HSB", "LSB", "CAB", "BAB"))),
  rmCells = list(
    list(
      measure="hs_abil",
      cell="HSB"),
    list(
      measure="ls_abil",
      cell="LSB"),
    list(
      measure="ha_abil",
      cell="CAB"),
    list(
      measure="la_abil",
      cell="BAB")),
  effectSize = c("partEta", "eta"),
  rmTerms = ~ ABILITY,
  postHoc = list(
    "ABILITY"))

#OPPORTUNITY

jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="OPPORTUNITY",
      levels=c("HSB", "LSB", "CAB", "BAB"))),
  rmCells = list(
    list(
      measure="hs_opportunity",
      cell="HSB"),
    list(
      measure="ls_opportunity",
      cell="LSB"),
    list(
      measure="ha_opportunity",
      cell="CAB"),
    list(
      measure="la_opportunity",
      cell="BAB")),
  effectSize = c("partEta", "eta"),
  rmTerms = ~ OPPORTUNITY,
  postHoc = list(
    "OPPORTUNITY"))

#SOCIAL CONNECTIONS

jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="SOCIAL_CONNECTIONS",
      levels=c("HSB", "LSB", "CAB", "BAB"))),
  rmCells = list(
    list(
      measure="hs_socn",
      cell="HSB"),
    list(
      measure="ls_socn",
      cell="LSB"),
    list(
      measure="ha_socn",
      cell="CAB"),
    list(
      measure="la_socn",
      cell="BAB")),
  effectSize = c("partEta", "eta"),
  rmTerms = ~ SOCIAL_CONNECTIONS,
  postHoc = list(
    "SOCIAL_CONNECTIONS"))

#PRIVILEGE

jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="PRIVILEGE",
      levels=c("HSB", "LSB", "CAB", "BAB"))),
  rmCells = list(
    list(
      measure="hs_priv",
      cell="HSB"),
    list(
      measure="ls_priv",
      cell="LSB"),
    list(
      measure="ha_priv",
      cell="CAB"),
    list(
      measure="la_priv",
      cell="BAB")),
  effectSize = c("partEta", "eta"),
  rmTerms = ~ PRIVILEGE,
  postHoc = list(
    "PRIVILEGE"))

#CHANCE

jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="CHANCE",
      levels=c("HSB", "LSB", "CAB", "BAB"))),
  rmCells = list(
    list(
      measure="hs_chance",
      cell="HSB"),
    list(
      measure="ls_chance",
      cell="LSB"),
    list(
      measure="ha_chance",
      cell="CAB"),
    list(
      measure="la_chance",
      cell="BAB")),
  effectSize = c("partEta", "eta"),
  rmTerms = ~ CHANCE,
  postHoc = list(
    "CHANCE"))

#LUCK

jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="LUCK",
      levels=c("HSB", "LSB", "CAB", "BAB"))),
  rmCells = list(
    list(
      measure="hs_luck",
      cell="HSB"),
    list(
      measure="ls_luck",
      cell="LSB"),
    list(
      measure="ha_luck",
      cell="CAB"),
    list(
      measure="la_luck",
      cell="BAB")),
  effectSize = c("partEta", "eta"),
  rmTerms = ~ LUCK,
  postHoc = list(
    "LUCK"))

#FATE

jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="FATE",
      levels=c("HSB", "LSB", "CAB", "BAB"))),
  rmCells = list(
    list(
      measure="hs_fate",
      cell="HSB"),
    list(
      measure="ls_fate",
      cell="LSB"),
    list(
      measure="ha_fate",
      cell="CAB"),
    list(
      measure="la_fate",
      cell="BAB")),
  effectSize = c("partEta", "eta"),
  rmTerms = ~ FATE,
  postHoc = list(
    "FATE"))
