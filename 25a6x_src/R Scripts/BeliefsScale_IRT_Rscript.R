# -------------------------------------------------
# IM & SHANE (2021) SES CAUSAL BELIEFS SCALE
# IRT ANALYSES SCRIPT
# -------------------------------------------------


library(lordif)
library(ltm)
library(mirt)

data = read.csv(file.choose())



##### IRT HIGH SES SOCIETY BELIEFS#####

#HIGH SES SOCIETAL - EFFORT
data_effo1 = data[c("Q1A", "Q1B", "Q1J")]
data_effo1 = na.omit(data_effo1)
effort1 = mirt(data = data_effo1,
                  model = 1,
                  itemtype = "gpcm")
plot(effort1, type = "trace")
plot(effort1, type = "info",
     main = "High SES Effort")

effo1co = coef(effort1, IRTpars = T)
write.csv(effo1co, file.choose())


#HIGH SES SOCIETAL - ABILITY
data_abil1 = data[c("Q1H", "Q1N", "Q1E")]
data_abil1 = na.omit(data_abil1)
ability1 = mirt(data = data_abil1,
               model = 1,
               itemtype = "gpcm")
plot(ability1, type = "trace")
abil1co = coef(ability1, IRTpars = T)
write.csv(abil1co, file.choose())


#HIGH SES SOCIETAL - MERIT SUBSCALE
data_merit1 = data[c("Q1A", "Q1B", "Q1H", "Q1N")]
data_merit1 = na.omit(data_merit1)
merit1 = mirt(data = data_merit1,
              model = 1,
              itemtype = "gpcm")
plot(merit1, type = "info",
     main = "MERIT")


#HIGH SES SOCIETAL - SOCIAL CONNECTIONS
data_socn1 = data[c("Q1C", "Q1D", "Q1M")]
data_socn1 = na.omit(data_socn1)
soccon1 = mirt(data = data_socn1,
                model = 1,
                itemtype = "gpcm")
plot(soccon1, type = "trace")
socn1co = coef(soccon1, IRTpars = T)
write.csv(socn1co, file.choose())


#HIGH SES SOCIETAL - PRIVILEGE
data_priv1 = data[c("Q1F", "Q1G", "Q1K")]
data_priv1 = na.omit(data_priv1)
privilege1 = mirt(data = data_priv1,
               model = 1,
               itemtype = "gpcm")
plot(privilege1, type = "trace")
priv1co = coef(privilege1, IRTpars = T)
write.csv(priv1co, file.choose())


#HIGH SES SOCIETAL - LUCK
data_luck1 = data[c("Q1I", "Q1O", "Q1L")]
data_luck1 = na.omit(data_luck1)
luck1 = mirt(data = data_luck1,
                  model = 1,
                  itemtype = "gpcm")
plot(luck1, type = "trace")
luck1co = coef(luck1, IRTpars = T)
write.csv(luck1co, file.choose())


#HIGH SES SOCIETAL - FATE
data_fate1 = data[c("Q1P", "Q1Q")]
data_fate1 = na.omit(data_fate1)
fate1 = mirt(data = data_fate1,
             model = 1,
             itemtype = "gpcm")
plot(fate1, type = "trace")
fate1co = coef(fate1, IRTpars = T)
write.csv(fate1co, file.choose())




##### IRT LOW SES SOCIETY BELIEFS#####

#LOW SES SOCIETAL - EFFORT
data_effo2 = data[c("Q2A", "Q2B", "Q2J")]
data_effo2 = na.omit(data_effo2)
effort2 = mirt(data = data_effo2,
               model = 1,
               itemtype = "gpcm")
plot(effort2, type = "trace")
effo2co = coef(effort2, IRTpars = T)
write.csv(effo2co, file.choose())


#LOW SES SOCIETAL - ABILITY
data_abil2 = data[c("Q2H", "Q2N", "Q2E")]
data_abil2 = na.omit(data_abil2)
ability2 = mirt(data = data_abil2,
                model = 1,
                itemtype = "gpcm")
plot(ability2, type = "trace")
abil2co = coef(ability2, IRTpars = T)
write.csv(abil2co, file.choose())


#LOW SES SOCIETAL - SOCIAL CONNECTIONS
data_socn2 = data[c("Q2C", "Q2D", "Q2M")]
data_socn2 = na.omit(data_socn2)
soccon2 = mirt(data = data_socn2,
               model = 1,
               itemtype = "gpcm")
plot(soccon2, type = "trace")
socn2co = coef(soccon2, IRTpars = T)
write.csv(socn2co, file.choose())


#LOW SES SOCIETAL - PRIVILEGE
data_priv2 = data[c("Q2F", "Q2G", "Q2K")]
data_priv2 = na.omit(data_priv2)
privilege2 = mirt(data = data_priv2,
                  model = 1,
                  itemtype = "gpcm")
plot(privilege2, type = "trace")
priv2co = coef(privilege2, IRTpars = T)
write.csv(priv2co, file.choose())


#LOW SES SOCIETAL - LUCK
data_luck2 = data[c("Q2I", "Q2O", "Q2L")]
data_luck2 = na.omit(data_luck2)
luck2 = mirt(data = data_luck2,
             model = 1,
             itemtype = "gpcm")
plot(luck2, type = "trace")
luck2co = coef(luck2, IRTpars = T)
write.csv(luck2co, file.choose())


#LOW SES SOCIETAL - FATE
data_fate2 = data[c("Q2Q", "Q2R")]
data_fate2 = na.omit(data_fate2)
fate2 = mirt(data = data_fate2,
             model = 1,
             itemtype = "gpcm")
plot(fate2, type = "trace")
fate2co = coef(fate2, IRTpars = T)
write.csv(fate2co, file.choose())




##### IRT CURRENT SES AGENCY BELIEFS#####


#CURRENT SES AGENCY - EFFORT
data_effo3 = data[c("Q3A", "Q3B", "Q3J")]
data_effo3 = na.omit(data_effo3)
effort3 = mirt(data = data_effo3,
               model = 1,
               itemtype = "gpcm")
plot(effort3, type = "trace")
effo3co = coef(effort3, IRTpars = T)
write.csv(effo3co, file.choose())


#CURRENT SES AGENCY - ABILITY
data_abil3 = data[c("Q3H", "Q3N", "Q3E")]
data_abil3 = na.omit(data_abil3)
ability3 = mirt(data = data_abil3,
                model = 1,
                itemtype = "gpcm")
plot(ability3, type = "trace")
abil3co = coef(ability3, IRTpars = T)
write.csv(abil3co, file.choose())


#CURRENT SES AGENCY - SOCIAL CONNECTIONS
data_socn3 = data[c("Q3C", "Q3D", "Q3M")]
data_socn3 = na.omit(data_socn3)
soccon3 = mirt(data = data_socn3,
               model = 1,
               itemtype = "gpcm")
plot(soccon3, type = "trace")
socn3co = coef(soccon3, IRTpars = T)
write.csv(socn3co, file.choose())


#CURRENT SES AGENCY - PRIVILEGE
data_priv3 = data[c("Q3F", "Q3G", "Q3K")]
data_priv3 = na.omit(data_priv3)
privilege3 = mirt(data = data_priv3,
                  model = 1,
                  itemtype = "gpcm")
plot(privilege3, type = "trace")
priv3co = coef(privilege3, IRTpars = T)
write.csv(priv3co, file.choose())


#CURRENT SES AGENCY - LUCK
data_luck3 = data[c("Q3I", "Q3O", "Q3L")]
data_luck3 = na.omit(data_luck3)
luck3 = mirt(data = data_luck3,
             model = 1,
             itemtype = "gpcm")
plot(luck3, type = "trace")
luck3co = coef(luck3, IRTpars = T)
write.csv(luck3co, file.choose())


#CURRENT SES AGENCY - FATE
data_fate3 = data[c("Q3P", "Q3Q")]
data_fate3 = na.omit(data_fate3)
fate3 = mirt(data = data_fate3,
             model = 1,
             itemtype = "gpcm")
plot(fate3, type = "trace")
fate3co = coef(fate3, IRTpars = T)
write.csv(fate3co, file.choose())




##### IRT BARRIER SES AGENCY BELIEFS#####


#BARRIER SES AGENCY - EFFORT
data_effo4 = data[c("Q4B", "Q4C", "Q4K")]
data_effo4 = na.omit(data_effo4)
effort4 = mirt(data = data_effo4,
               model = 1,
               itemtype = "gpcm")
plot(effort4, type = "trace")
effo4co = coef(effort4, IRTpars = T)
write.csv(effo4co, file.choose())


#BARRIER SES AGENCY - ABILITY
data_abil4 = data[c("Q4I", "Q4N", "Q4F")]
data_abil4 = na.omit(data_abil4)
ability4 = mirt(data = data_abil4,
                model = 1,
                itemtype = "gpcm")
plot(ability4, type = "trace")
abil4co = coef(ability4, IRTpars = T)
write.csv(abil4co, file.choose())


#BARRIER SES AGENCY - SOCIAL CONNECTIONS
data_socn4 = data[c("Q4D", "Q4E", "Q4M")]
data_socn4 = na.omit(data_socn4)
soccon4 = mirt(data = data_socn4,
               model = 1,
               itemtype = "gpcm")
plot(soccon4, type = "trace")
socn4co = coef(soccon4, IRTpars = T)
write.csv(socn4co, file.choose())


#BARRIER SES AGENCY - PRIVILEGE
data_priv4 = data[c("Q4G", "Q4H", "Q4A")]
data_priv4 = na.omit(data_priv4)
privilege4 = mirt(data = data_priv4,
                  model = 1,
                  itemtype = "gpcm")
plot(privilege4, type = "trace")
priv4co = coef(privilege4, IRTpars = T)
write.csv(priv4co, file.choose())


#BARRIER SES AGENCY - LUCK
data_luck4 = data[c("Q4J", "Q4O", "Q4L")]
data_luck4 = na.omit(data_luck4)
luck4 = mirt(data = data_luck4,
             model = 1,
             itemtype = "gpcm")
plot(luck4, type = "trace")
luck4co = coef(luck4, IRTpars = T)
write.csv(luck4co, file.choose())


#BARRIER SES AGENCY - FATE
data_fate4 = data[c("Q4P", "Q4Q")]
data_fate4 = na.omit(data_fate4)
fate4 = mirt(data = data_fate4,
             model = 1,
             itemtype = "gpcm")
plot(fate4, type = "trace")
fate4co = coef(fate4, IRTpars = T)
write.csv(fate4co, file.choose())




##### DIFFERENTIAL ITEM FUNCTIONING#####

#Create trichotomous income variable
data = data %>% mutate(
  income3 = recode(income, '1=1; 2=1; 3=1; 4=1; 5=1; 6=1; 7=1; 8=1;
                            9=2; 10=2; 11=2; 12=2; 13=2;
                            14=3; 15=3; 16=3; 17=3; 18=3'))

q1merit = data[c("Q1A", "Q1B", "Q1H", "Q1N")]
q1opportunity = data[c("Q1C", "Q1D", "Q1F", "Q1G")]
q1chance = data[c("Q1I", "Q1O", "Q1P", "Q1Q")]

q2merit = data[c("Q2A", "Q2B", "Q2H", "Q2N")]
q2opportunity = data[c("Q2C", "Q2D", "Q2F", "Q2G")]
q2chance = data[c("Q2I", "Q2O", "Q2Q", "Q2R")]

q3merit = data[c("Q3A", "Q3B", "Q3H", "Q3N")]
q3opportunity = data[c("Q3C", "Q3D", "Q3F", "Q3G")]
q3chance = data[c("Q3I", "Q3O", "Q3P", "Q3Q")]

q4merit = data[c("Q4B", "Q4C", "Q4I", "Q4N")]
q4opportunity = data[c("Q4D", "Q4E", "Q4G", "Q4H")]
q4chance = data[c("Q4J", "Q4O", "Q4P", "Q4Q")]

sesmerit = data[c("hs_merit", "ls_merit", "ha_merit", "la_merit")]
sesopportunity = data[c("hs_opportunity", "ls_opportunity", "ha_opportunity", "la_opportunity")]
seschance = data[c("hs_chance", "ls_chance", "ha_chance", "la_chance")]

merit_full = data[c("hs_efft", "hs_abil", "ls_efft", "ls_abil", "ha_efft", "la_efft")]

gender = data$gender
age = data$AGE4
politic = data$D3
income = data$income3
educ = data$EDUC4

#Q1 Merit
q1mg = lordif(q1merit, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q1ma = lordif(q1merit, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 2, 4
q1mp = lordif(q1merit, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q1me = lordif(q1merit, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q1mi = lordif(q1merit, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF

summary(q1ma) #pseudo R^2 item 2 = 0.0136; i4 = 0.0080
plot(q1ma)

#Q1 Opportunity
q1og = lordif(q1opportunity, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 4
q1oa = lordif(q1opportunity, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 4
q1op = lordif(q1opportunity, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 4
q1oe = lordif(q1opportunity, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q1oi = lordif(q1opportunity, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 3

summary(q1og) #pseudo R^2 i4 = 0.0052
summary(q1oa) #pseudo R^2 i4 = 0.0094
summary(q1op) #pseudo R^2 i4 = 0.0120
summary(q1oi) #pseudo R^2 i3 = 0.0091

#Q1 Chance
q1cg = lordif(q1chance, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 4
q1ca = lordif(q1chance, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q1cp = lordif(q1chance, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 1, 2
q1ce = lordif(q1chance, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 2
q1ci = lordif(q1chance, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 4

summary(q1cg) #pseudo R^2 i4 = 0.0048
summary(q1cp) #pseudo R^2 i1 = 0.0116; i2 = 0.0072
summary(q1ce) #pseudo R^2 i2 = 0.0059
summary(q1ci) #pseudo R^2 i4 = 0.0084




#Q2 Merit
q2mg = lordif(q2merit, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q2ma = lordif(q2merit, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 2, 3
q2mp = lordif(q2merit, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 3, 4
q2me = lordif(q2merit, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q2mi = lordif(q2merit, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF

summary(q2ma) #pseudo R^2 i2 = 0.0173; i3 = 0.0057
summary(q2mp) #pseudo R^2 i3 = 0.0086; i4 = 0.0175

#Q2 Opportunity
q2og = lordif(q2opportunity, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q2oa = lordif(q2opportunity, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 4
q2op = lordif(q2opportunity, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 4
q2oe = lordif(q2opportunity, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 4
q2oi = lordif(q2opportunity, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 2

summary(q2oa) #pseudo R^2 i4 = 0.0080
summary(q2op) #pseudo R^2 i4 = 0.0191
summary(q2oe) #pseudo R^2 i4 = 0.0128
summary(q2oi) #pseudo R^2 i2 = 0.0084

#Q2 Chance
q2cg = lordif(q2chance, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q2ca = lordif(q2chance, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 1
q2cp = lordif(q2chance, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 1
q2ce = lordif(q2chance, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q2ci = lordif(q2chance, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF

summary(q2ca) #pseudo R^2 i1 = 0.0057
summary(q2cp) #pseudo R^2 i1 = 0.0094





#Q3 Merit
q3mg = lordif(q3merit, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q3ma = lordif(q3merit, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q3mp = lordif(q3merit, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 3, 4
q3me = lordif(q3merit, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 2, 3, 4
q3mi = lordif(q3merit, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 2

summary(q3mp) #pseudo R^2 i3 = 0.0116; i4 = 0.0135
summary(q3me) #pseudo R^2 i2 = 0.0217; i3 = 0.0182; i4 = 0.0102
summary(q3mi) #pseudo R^2 i2 = 0.0158

#Q3 Opportunity
q3og = lordif(q3opportunity, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 4
q3oa = lordif(q3opportunity, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 4
q3op = lordif(q3opportunity, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 3, 4
q3oe = lordif(q3opportunity, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 1, 4
q3oi = lordif(q3opportunity, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF

summary(q3og) #pseudo R^2 i4 = 0.0042
summary(q3oa) #pseudo R^2 i4 = 0.0092
summary(q3op) #pseudo R^2 i3 = 0.0109; i4 = 0.0061
summary(q3oe) #pseudo R^2 i1 = 0.0105; i4 = 0.0127

#Q3 Chance
q3cg = lordif(q3chance, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q3ca = lordif(q3chance, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q3cp = lordif(q3chance, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 1
q3ce = lordif(q3chance, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q3ci = lordif(q3chance, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF

summary(q3cp) #pseudo R^2 i1 = 0.0058





#Q4 Merit
q4mg = lordif(q4merit, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q4ma = lordif(q4merit, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q4mp = lordif(q4merit, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q4me = lordif(q4merit, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 1
q4mi = lordif(q4merit, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF

summary(q4me) #pseudo R^2 i1 = 0.0122

#Q4 Opportunity
q4og = lordif(q4opportunity, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 3
q4oa = lordif(q4opportunity, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q4op = lordif(q4opportunity, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q4oe = lordif(q4opportunity, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 1, 3, 4
q4oi = lordif(q4opportunity, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF items 3, 4

summary(q4og) #pseudo R^2 i3 = 0.0059
summary(q4oe) #pseudo R^2 i1 = 0.0060; i3 = 0.0080; i4 = 0.0263
summary(q4oi) #pseudo R^2 i3 = 0.0147; i4 = 0.0192

#Q4 Chance
q4cg = lordif(q4chance, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 2
q4ca = lordif(q4chance, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 1
q4cp = lordif(q4chance, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 2
q4ce = lordif(q4chance, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
q4ci = lordif(q4chance, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF

summary(q4cg) #pseudo R^2 i2 = 0.0045
summary(q4ca) #pseudo R^2 i1 = 0.0125
summary(q4cp) #pseudo R^2 i2 = 0.0062


fullg = lordif(merit_full, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5)
fulla = lordif(merit_full, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 1
fullp = lordif(merit_full, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
fulle = lordif(merit_full, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
fulli = lordif(merit_full, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF

meritg = lordif(sesmerit, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 2
merita = lordif(sesmerit, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 1
meritp = lordif(sesmerit, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
merite = lordif(sesmerit, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
meriti = lordif(sesmerit, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF

plot(meritg, labels = c('M', 'F'))
plot(merita)

opporg = lordif(sesopportunity, gender, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 2
oppora = lordif(sesopportunity, age, criterion = "Chisqr", alpha = 0.01, minCell = 5) #DIF item 1
opporp = lordif(sesopportunity, politic, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
oppore = lordif(sesopportunity, educ, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF
oppori = lordif(sesopportunity, income, criterion = "Chisqr", alpha = 0.01, minCell = 5) #no DIF

summary(oppori)
summary(oppore)

##### IRT ITEM INFORMATION#####

data_merit1 = data[c("Q1A", "Q1B", "Q1H", "Q1N")]
data_merit2 = data[c("Q2A", "Q2B", "Q2H", "Q2N")]
data_merit3 = data[c("Q3A", "Q3B", "Q3H", "Q3N")]
data_merit4 = data[c("Q4B", "Q4C", "Q4I", "Q4N")]

#Graded model for each subscale
merit1gm = grm(data_merit1)
merit2gm = grm(data_merit2)
merit3gm = grm(data_merit3)
merit4gm = grm(data_merit4)

#graded subscale information for Merit
meri1 = information(merit1gm, c(-4,4))
meri2 = information(merit2gm, c(-4,4))
meri3 = information(merit3gm, c(-4,4))
meri4 = information(merit4gm, c(-4,4))

#high SES societal merit
hsbm1 = information(merit1gm, c(-4, 4), items = 1)
hsbm2 = information(merit1gm, c(-4, 4), items = 2)
hsbm3 = information(merit1gm, c(-4, 4), items = 3)
hsbm4 = information(merit1gm, c(-4, 4), items = 4)

#high SES societal merit
lsbm1 = information(merit2gm, c(-4, 4), items = 1)
lsbm2 = information(merit2gm, c(-4, 4), items = 2)
lsbm3 = information(merit2gm, c(-4, 4), items = 3)
lsbm4 = information(merit2gm, c(-4, 4), items = 4)

#current SES agentic merit
cabm1 = information(merit3gm, c(-4, 4), items = 1)
cabm2 = information(merit3gm, c(-4, 4), items = 2)
cabm3 = information(merit3gm, c(-4, 4), items = 3)
cabm4 = information(merit3gm, c(-4, 4), items = 4)

#barrier SES agentic merit
babm1 = information(merit4gm, c(-4, 4), items = 1)
babm2 = information(merit4gm, c(-4, 4), items = 2)
babm3 = information(merit4gm, c(-4, 4), items = 3)
babm4 = information(merit4gm, c(-4, 4), items = 4)


kable(coef.grm(merit1gm, prob = T))
kable(coef.grm(merit2gm, prob = T))
kable(coef.grm(merit3gm, prob = T))
kable(coef.grm(merit4gm, prob = T))



data_oppor1 = data[c("Q1C", "Q1D", "Q1F", "Q1G")]
data_oppor2 = data[c("Q2C", "Q2D", "Q2F", "Q2G")]
data_oppor3 = data[c("Q3C", "Q3D", "Q3F", "Q3G")]
data_oppor4 = data[c("Q4D", "Q4E", "Q4G", "Q4H")]

#Graded model for each subscale
oppor1gm = grm(data_oppor1)
oppor2gm = grm(data_oppor2)
oppor3gm = grm(data_oppor3)
oppor4gm = grm(data_oppor4)

#graded subscale information for Merit
oppo1 = information(oppor1gm, c(-4,4))
oppo2 = information(oppor2gm, c(-4,4))
oppo3 = information(oppor3gm, c(-4,4))
oppo4 = information(oppor4gm, c(-4,4))

#high SES societal merit
hsbo1 = information(oppor1gm, c(-4, 4), items = 1)
hsbo2 = information(oppor1gm, c(-4, 4), items = 2)
hsbo3 = information(oppor1gm, c(-4, 4), items = 3)
hsbo4 = information(oppor1gm, c(-4, 4), items = 4)

#high SES societal merit
lsbo1 = information(oppor2gm, c(-4, 4), items = 1)
lsbo2 = information(oppor2gm, c(-4, 4), items = 2)
lsbo3 = information(oppor2gm, c(-4, 4), items = 3)
lsbo4 = information(oppor2gm, c(-4, 4), items = 4)

#current SES agentic merit
cabo1 = information(oppor3gm, c(-4, 4), items = 1)
cabo2 = information(oppor3gm, c(-4, 4), items = 2)
cabo3 = information(oppor3gm, c(-4, 4), items = 3)
cabo4 = information(oppor3gm, c(-4, 4), items = 4)

#barrier SES agentic merit
babo1 = information(oppor4gm, c(-4, 4), items = 1)
babo2 = information(oppor4gm, c(-4, 4), items = 2)
babo3 = information(oppor4gm, c(-4, 4), items = 3)
babo4 = information(oppor4gm, c(-4, 4), items = 4)




data_chanc1 = data[c("Q1I", "Q1O", "Q1P", "Q1Q")]
data_chanc2 = data[c("Q2I", "Q2O", "Q2Q", "Q2R")]
data_chanc3 = data[c("Q3I", "Q3O", "Q3P", "Q3Q")]
data_chanc4 = data[c("Q4J", "Q4O", "Q4P", "Q4Q")] #causes error due to NAs
data_chanc4 = data_chanc4[rowSums(is.na(data_chanc4)) < 4, ] #delete 9 cases with complete missing data

#Graded model for each subscale
chanc1gm = grm(data_chanc1)
chanc2gm = grm(data_chanc2)
chanc3gm = grm(data_chanc3)
chanc4gm = grm(data_chanc4)

#graded subscale information for Merit
chan1 = information(chanc1gm, c(-4,4))
chan2 = information(chanc2gm, c(-4,4))
chan3 = information(chanc3gm, c(-4,4))
chan4 = information(chanc4gm, c(-4,4))

#high SES societal merit
hsbc1 = information(chanc1gm, c(-4, 4), items = 1)
hsbc2 = information(chanc1gm, c(-4, 4), items = 2)
hsbc3 = information(chanc1gm, c(-4, 4), items = 3)
hsbc4 = information(chanc1gm, c(-4, 4), items = 4)

#high SES societal merit
lsbc1 = information(chanc2gm, c(-4, 4), items = 1)
lsbc2 = information(chanc2gm, c(-4, 4), items = 2)
lsbc3 = information(chanc2gm, c(-4, 4), items = 3)
lsbc4 = information(chanc2gm, c(-4, 4), items = 4)

#current SES agentic merit
cabc1 = information(chanc3gm, c(-4, 4), items = 1)
cabc2 = information(chanc3gm, c(-4, 4), items = 2)
cabc3 = information(chanc3gm, c(-4, 4), items = 3)
cabc4 = information(chanc3gm, c(-4, 4), items = 4)

#barrier SES agentic merit
babc1 = information(chanc4gm, c(-4, 4), items = 1)
babc2 = information(chanc4gm, c(-4, 4), items = 2)
babc3 = information(chanc4gm, c(-4, 4), items = 3)
babc4 = information(chanc4gm, c(-4, 4), items = 4)



socbef = matrix(NA, nrow = 15, ncol = 7)
colnames(socbef) = c("Item", "Total", "Range", "Range %", "Total", "Range", "Range %")
socbef[1, ] = c("MERIT", round(meri1$InfoTotal, 3), round(meri1$InfoRange, 3), round(meri1$PropRange, 3),
               round(meri2$InfoTotal, 3), round(meri2$InfoRange, 3), round(meri2$PropRange, 3))
socbef[2, ] = c("Q1A/2A", round(hsbm1$InfoTotal, 3), round(hsbm1$InfoRange, 3), round(hsbm1$PropRange, 3),
               round(lsbm1$InfoTotal, 3), round(lsbm1$InfoRange, 3), round(lsbm1$PropRange, 3))
socbef[3, ] = c("Q1B/2B", round(hsbm2$InfoTotal, 3), round(hsbm2$InfoRange, 3), round(hsbm2$PropRange, 3),
               round(lsbm2$InfoTotal, 3), round(lsbm2$InfoRange, 3), round(lsbm2$PropRange, 3))
socbef[4, ] = c("Q1H/2H", round(hsbm3$InfoTotal, 3), round(hsbm3$InfoRange, 3), round(hsbm3$PropRange, 3),
               round(lsbm3$InfoTotal, 3), round(lsbm3$InfoRange, 3), round(lsbm3$PropRange, 3))
socbef[5, ] = c("Q1N/2N", round(hsbm4$InfoTotal, 3), round(hsbm4$InfoRange, 3), round(hsbm4$PropRange, 3),
               round(lsbm4$InfoTotal, 3), round(lsbm4$InfoRange, 3), round(lsbm4$PropRange, 3))
socbef[6, ] = c("OPPORTUNITY", round(oppo1$InfoTotal, 3), round(oppo1$InfoRange, 3), round(oppo1$PropRange, 3),
                round(oppo2$InfoTotal, 3), round(oppo2$InfoRange, 3), round(oppo2$PropRange, 3))
socbef[7, ] = c("Q1C/2C", round(hsbo1$InfoTotal, 3), round(hsbo1$InfoRange, 3), round(hsbo1$PropRange, 3),
                round(lsbo1$InfoTotal, 3), round(lsbo1$InfoRange, 3), round(lsbo1$PropRange, 3))
socbef[8, ] = c("Q1D/2D", round(hsbo2$InfoTotal, 3), round(hsbo2$InfoRange, 3), round(hsbo2$PropRange, 3),
                round(lsbo2$InfoTotal, 3), round(lsbo2$InfoRange, 3), round(lsbo2$PropRange, 3))
socbef[9, ] = c("Q1F/2F", round(hsbo3$InfoTotal, 3), round(hsbo3$InfoRange, 3), round(hsbo3$PropRange, 3),
                round(lsbo3$InfoTotal, 3), round(lsbo3$InfoRange, 3), round(lsbo3$PropRange, 3))
socbef[10, ] = c("Q1G/2G", round(hsbo4$InfoTotal, 3), round(hsbo4$InfoRange, 3), round(hsbo4$PropRange, 3),
                round(lsbo4$InfoTotal, 3), round(lsbo4$InfoRange, 3), round(lsbo4$PropRange, 3))
socbef[11, ] = c("CHANCE", round(chan1$InfoTotal, 3), round(chan1$InfoRange, 3), round(chan1$PropRange, 3),
                round(chan2$InfoTotal, 3), round(chan2$InfoRange, 3), round(chan2$PropRange, 3))
socbef[12, ] = c("Q1I/2I", round(hsbc1$InfoTotal, 3), round(hsbc1$InfoRange, 3), round(hsbc1$PropRange, 3),
                round(lsbc1$InfoTotal, 3), round(lsbc1$InfoRange, 3), round(lsbc1$PropRange, 3))
socbef[13, ] = c("Q1O/2O", round(hsbc2$InfoTotal, 3), round(hsbc2$InfoRange, 3), round(hsbc2$PropRange, 3),
                round(lsbc2$InfoTotal, 3), round(lsbc2$InfoRange, 3), round(lsbc2$PropRange, 3))
socbef[14, ] = c("Q1P/2Q", round(hsbc3$InfoTotal, 3), round(hsbc3$InfoRange, 3), round(hsbc3$PropRange, 3),
                round(lsbc3$InfoTotal, 3), round(lsbc3$InfoRange, 3), round(lsbc3$PropRange, 3))
socbef[15, ] = c("Q1Q/2R", round(hsbc4$InfoTotal, 3), round(hsbc4$InfoRange, 3), round(hsbc4$PropRange, 3),
                 round(lsbc4$InfoTotal, 3), round(lsbc4$InfoRange, 3), round(lsbc4$PropRange, 3))


agebef = matrix(NA, nrow = 15, ncol = 7)
colnames(agebef) = c("Item", "Total", "Range", "Range %", "Total", "Range", "Range %")
agebef[1, ] = c("MERIT", round(meri3$InfoTotal, 3), round(meri3$InfoRange, 3), round(meri3$PropRange, 3),
                round(meri4$InfoTotal, 3), round(meri4$InfoRange, 3), round(meri4$PropRange, 3))
agebef[2, ] = c("Q3A/4B", round(cabm1$InfoTotal, 3), round(cabm1$InfoRange, 3), round(cabm1$PropRange, 3),
                round(babm1$InfoTotal, 3), round(babm1$InfoRange, 3), round(babm1$PropRange, 3))
agebef[3, ] = c("Q3B/4C", round(cabm2$InfoTotal, 3), round(cabm2$InfoRange, 3), round(cabm2$PropRange, 3),
                round(babm2$InfoTotal, 3), round(babm2$InfoRange, 3), round(babm2$PropRange, 3))
agebef[4, ] = c("Q3H/4I", round(cabm3$InfoTotal, 3), round(cabm3$InfoRange, 3), round(cabm3$PropRange, 3),
                round(babm3$InfoTotal, 3), round(babm3$InfoRange, 3), round(babm3$PropRange, 3))
agebef[5, ] = c("Q3N/4N", round(cabm4$InfoTotal, 3), round(cabm4$InfoRange, 3), round(cabm4$PropRange, 3),
                round(babm4$InfoTotal, 3), round(babm4$InfoRange, 3), round(babm4$PropRange, 3))
agebef[6, ] = c("OPPORTUNITY", round(oppo3$InfoTotal, 3), round(oppo3$InfoRange, 3), round(oppo3$PropRange, 3),
                round(oppo4$InfoTotal, 3), round(oppo4$InfoRange, 3), round(oppo4$PropRange, 3))
agebef[7, ] = c("Q3C/4D", round(cabo1$InfoTotal, 3), round(cabo1$InfoRange, 3), round(cabo1$PropRange, 3),
                round(babo1$InfoTotal, 3), round(babo1$InfoRange, 3), round(babo1$PropRange, 3))
agebef[8, ] = c("Q3D/4E", round(cabo2$InfoTotal, 3), round(cabo2$InfoRange, 3), round(cabo2$PropRange, 3),
                round(babo2$InfoTotal, 3), round(babo2$InfoRange, 3), round(babo2$PropRange, 3))
agebef[9, ] = c("Q3F/4G", round(cabo3$InfoTotal, 3), round(cabo3$InfoRange, 3), round(cabo3$PropRange, 3),
                round(babo3$InfoTotal, 3), round(babo3$InfoRange, 3), round(babo3$PropRange, 3))
agebef[10, ] = c("Q3G/4H", round(cabo4$InfoTotal, 3), round(cabo4$InfoRange, 3), round(cabo4$PropRange, 3),
                 round(babo4$InfoTotal, 3), round(babo4$InfoRange, 3), round(babo4$PropRange, 3))
agebef[11, ] = c("CHANCE", round(chan3$InfoTotal, 3), round(chan3$InfoRange, 3), round(chan3$PropRange, 3),
                 round(chan4$InfoTotal, 3), round(chan4$InfoRange, 3), round(chan4$PropRange, 3))
agebef[12, ] = c("Q3I/4J", round(cabc1$InfoTotal, 3), round(cabc1$InfoRange, 3), round(cabc1$PropRange, 3),
                 round(babc1$InfoTotal, 3), round(babc1$InfoRange, 3), round(babc1$PropRange, 3))
agebef[13, ] = c("Q3O/4O", round(cabc2$InfoTotal, 3), round(cabc2$InfoRange, 3), round(cabc2$PropRange, 3),
                 round(babc2$InfoTotal, 3), round(babc2$InfoRange, 3), round(babc2$PropRange, 3))
agebef[14, ] = c("Q3P/4P", round(cabc3$InfoTotal, 3), round(cabc3$InfoRange, 3), round(cabc3$PropRange, 3),
                 round(babc3$InfoTotal, 3), round(babc3$InfoRange, 3), round(babc3$PropRange, 3))
agebef[15, ] = c("Q3Q/4Q", round(cabc4$InfoTotal, 3), round(cabc4$InfoRange, 3), round(cabc4$PropRange, 3),
                 round(babc4$InfoTotal, 3), round(babc4$InfoRange, 3), round(babc4$PropRange, 3))

kable(socbef)
kable(agebef)


write.csv(socbef, file.choose())
write.csv(agebef, file.choose())


plot(merit1gm, type = "IIC", items = 0, main = "MERIT", lwd = 2)
plot(merit2gm, type = "IIC", items = 0, main = "MERIT", lwd = 2)
plot(merit3gm, type = "IIC", items = 0, main = "MERIT", lwd = 2)
plot(merit4gm, type = "IIC", items = 0, main = "MERIT", lwd = 2)

plot(oppor1gm, type = "IIC", items = 0, main = "OPPORTUNITY", lwd = 2)
plot(oppor2gm, type = "IIC", items = 0, main = "OPPORTUNITY", lwd = 2)
plot(oppor3gm, type = "IIC", items = 0, main = "OPPORTUNITY", lwd = 2)
plot(oppor4gm, type = "IIC", items = 0, main = "OPPORTUNITY", lwd = 2)

plot(chanc1gm, type = "IIC", items = 0, main = "CHANCE", lwd = 2)
plot(chanc2gm, type = "IIC", items = 0, main = "CHANCE", lwd = 2)
plot(chanc3gm, type = "IIC", items = 0, main = "CHANCE", lwd = 2)
plot(chanc4gm, type = "IIC", items = 0, main = "CHANCE", lwd = 2)
