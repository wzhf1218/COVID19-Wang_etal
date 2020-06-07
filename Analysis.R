library(lme4)
library(xlsx)

exp_data = read.xlsx("Data_with_S6.xlsx",1)  # change to "Data_without_S6.xlsx" for CD4+ and CD8+ T cells
str(exp_data)

# generate the values for the variable P in the models
s1=14
for (i in 1:length(exp_data$days)){
  if (exp_data$days[i]<=s1){exp_data$period[i] = 1}
  else {exp_data$period[i] = 0}
}

# perform fitting and ANOVA
fit1 <- lmer(Sydecan1 ~  days + period + (1 | Patient_ID), data=exp_data);
fit2 <- lmer(Sydecan1 ~  days + period + group_ID + (1 | Patient_ID), data=exp_data); 
anova(fit1, fit2)

