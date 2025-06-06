# -------------------------------------------------
# IM & SHANE (2021) SES CAUSAL BELIEFS SCALE
# DATA CLEANUP SCRIPT
# -------------------------------------------------

library(dplyr)
library(car)

data = read.csv(file.choose())

#Reverse Code
data = data %>%
  mutate(
    Q8D_r = recode(Q8D, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q8E_r = recode(Q8E, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q8F_r = recode(Q8F, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q13C_r = recode(Q13C, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q13D_r = recode(Q13D, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q13E_r = recode(Q13E, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q13G_r = recode(Q13G, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q13F_r = recode(Q13F, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q13H_r = recode(Q13H, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q13I_r = recode(Q13I, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q14O_r = recode(Q14O, '1=5; 2=4; 3=3; 4=2; 5=1'),
    Q14P_r = recode(Q14P, '1=5; 2=4; 3=3; 4=2; 5=1'),
    D3_mod = ifelse(D3 == 2, 4, NA),
    D5_con = recode(D5, '1=7; 2=6; 3=5')
  )

data = data %>%
  mutate(conservative = rowMeans(x = select(.data = ., D3_mod, D4, D5_con), na.rm = TRUE),
         hs_efft = rowMeans(x = select(.data = ., Q1A, Q1B), na.rm = TRUE),
         hs_abil = rowMeans(x = select(.data = ., Q1H, Q1N), na.rm = TRUE),
         hs_socn = rowMeans(x = select(.data = ., Q1C, Q1D), na.rm = TRUE),
         hs_priv = rowMeans(x = select(.data = ., Q1F, Q1G), na.rm = TRUE),
         hs_luck = rowMeans(x = select(.data = ., Q1I, Q1O), na.rm = TRUE),
         hs_fate = rowMeans(x = select(.data = ., Q1P, Q1Q), na.rm = TRUE),
         hs_merit = rowMeans(x = select(.data = ., Q1A, Q1B, Q1H, Q1N), na.rm = TRUE),
         hs_opportunity = rowMeans(x = select(.data = ., Q1C, Q1D, Q1F, Q1G), na.rm = TRUE),
         hs_chance = rowMeans(x = select(.data = ., Q1I, Q1O, Q1P, Q1Q), na.rm = TRUE),
         ls_efft = rowMeans(x = select(.data = ., Q2A, Q2B), na.rm = TRUE),
         ls_abil = rowMeans(x = select(.data = ., Q2H, Q2N), na.rm = TRUE),
         ls_socn = rowMeans(x = select(.data = ., Q2C, Q2D), na.rm = TRUE),
         ls_priv = rowMeans(x = select(.data = ., Q2F, Q2G), na.rm = TRUE),
         ls_luck = rowMeans(x = select(.data = ., Q2I, Q2O), na.rm = TRUE),
         ls_fate = rowMeans(x = select(.data = ., Q2Q, Q2R), na.rm = TRUE),
         ls_merit = rowMeans(x = select(.data = ., Q2A, Q2B, Q2H, Q2N), na.rm = TRUE),
         ls_opportunity = rowMeans(x = select(.data = ., Q2C, Q2D, Q2F, Q2G), na.rm = TRUE),
         ls_chance = rowMeans(x = select(.data = ., Q2I, Q2O, Q2Q, Q2R), na.rm = TRUE),
         ha_efft = rowMeans(x = select(.data = ., Q3A, Q3B), na.rm = TRUE),
         ha_abil = rowMeans(x = select(.data = ., Q3H, Q3N), na.rm = TRUE),
         ha_socn = rowMeans(x = select(.data = ., Q3C, Q3D), na.rm = TRUE),
         ha_priv = rowMeans(x = select(.data = ., Q3F, Q3G), na.rm = TRUE),
         ha_luck = rowMeans(x = select(.data = ., Q3I, Q3O), na.rm = TRUE),
         ha_fate = rowMeans(x = select(.data = ., Q3P, Q3Q), na.rm = TRUE),
         ha_merit = rowMeans(x = select(.data = ., Q3A, Q3B, Q3H, Q3N), na.rm = TRUE),
         ha_opportunity = rowMeans(x = select(.data = ., Q3C, Q3D, Q3F, Q3G), na.rm = TRUE),
         ha_chance = rowMeans(x = select(.data = ., Q3I, Q3O, Q3P, Q3Q), na.rm = TRUE),
         la_efft = rowMeans(x = select(.data = ., Q4B, Q4C), na.rm = TRUE),
         la_abil = rowMeans(x = select(.data = ., Q4I, Q4N), na.rm = TRUE),
         la_socn = rowMeans(x = select(.data = ., Q4D, Q4E), na.rm = TRUE),
         la_priv = rowMeans(x = select(.data = ., Q4G, Q4H), na.rm = TRUE),
         la_luck = rowMeans(x = select(.data = ., Q4J, Q4O), na.rm = TRUE),
         la_fate = rowMeans(x = select(.data = ., Q4P, Q4Q), na.rm = TRUE),
         la_merit = rowMeans(x = select(.data = ., Q4B, Q4C, Q4I, Q4N), na.rm = TRUE),
         la_opportunity = rowMeans(x = select(.data = ., Q4D, Q4E, Q4G, Q4H), na.rm = TRUE),
         la_chance = rowMeans(x = select(.data = ., Q4J, Q4O, Q4P, Q4Q), na.rm = TRUE),
         ops_spc = rowMeans(x = select(.data = ., Q7A, Q7B, Q7C), na.rm = TRUE),
         ops_cpc = rowMeans(x = select(.data = ., Q7D, Q7E, Q7F), na.rm = TRUE),
         ops_ssc = rowMeans(x = select(.data = ., Q7G, Q7H, Q7I), na.rm = TRUE),
         ops_csc = rowMeans(x = select(.data = ., Q7J, Q7K, Q7L, Q7M, Q7N, Q7O), na.rm = TRUE),
         ops_csc_diseng = rowMeans(x = select(.data = ., Q7J, Q7K, Q7L, Q7N), na.rm = TRUE),
         ops_csc_selfprt = rowMeans(x = select(.data = ., Q7M), na.rm = TRUE),
         ops_csc_adjgoal = rowMeans(x = select(.data = ., Q7O), na.rm = TRUE),
         ops_engage = rowMeans(x = select(.data = ., Q7A, Q7B, Q7C, Q7D, Q7E, Q7F, Q7G, Q7H, Q7I), na.rm = TRUE),
         soc_persmast = rowMeans(x = select(.data = ., Q8A, Q8B, Q8C), na.rm = TRUE),
         soc_perconst = rowMeans(x = select(.data = ., Q8D, Q8E, Q8F), na.rm = TRUE),
         sensecontrol = rowMeans(x = select(.data = ., Q8A, Q8B, Q8C, Q8D_r, Q8E_r, Q8F_r), na.rm = TRUE),
         sesfam = rowMeans(x = select(.data = ., Q10), na.rm = TRUE),
         sesown = rowMeans(x = select(.data = ., Q11), na.rm = TRUE),
         sesfut = rowMeans(x = select(.data = ., Q12), na.rm = TRUE),
         sesown_fam = Q11-Q10,
         sesfut_own = Q12-Q11,
         sestotal = rowMeans(x = select(.data = ., Q10, Q11, Q12), na.rm = TRUE),
         fw_free = rowMeans(x = select(.data = ., Q13A, Q13B, Q13C_r), na.rm = TRUE),
         fw_rand = rowMeans(x = select(.data = ., Q13D, Q13E, Q13F_r), na.rm = TRUE),
         fw_fate = rowMeans(x = select(.data = ., Q13G, Q13H, Q13I), na.rm = TRUE),
         freewill = rowMeans(x = select(.data = ., Q13A, Q13B, Q13C_r, Q13D_r, Q13E_r, Q13F, Q13G_r, Q13H_r, Q13I_r), na.rm = TRUE),
         wb_lifsat = rowMeans(x = select(.data = ., Q14A, Q14B, Q14C), na.rm = TRUE),
         wb_flouri = rowMeans(x = select(.data = ., Q14D, Q14E, Q14F, Q14G, Q14H, Q14I, Q14L, Q14M), na.rm = TRUE),
         wb_socact = rowMeans(x = select(.data = ., Q14N, Q14O_r, Q14P_r), na.rm = TRUE),
         wellbeing = rowMeans(x = select(.data = ., Q14A, Q14B, Q14C,
                                         Q14D, Q14E, Q14F, Q14G, Q14H, Q14I, Q14L, Q14M,
                                         Q14N, Q14O_r, Q14P_r), na.rm = TRUE)
  )

write.csv(data, file.choose())