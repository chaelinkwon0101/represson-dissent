######Cross-country analysis#####
mydt <- read.csv("vdem+mmd.csv")
dt_f <- read.csv("vdem+mmd+fariss.csv")
dt_srcs <- read.csv("vdem+mmd+fariss+srcs.csv")

#packages
library(MASS)
library(lmtest)
library(car)
library(ggeffects)
library(margins)
library(performance)
library(sandwich)
library(fixest)
library(dplyr)
library(sjPlot)
library(tidyverse)
library(performance)
library(see)
library(qqplotr)
library(psych)
library(interplot)
library(ggplot2)
library(ggthemes)
library(stargazer)

unique(mydt$country_name)
summary(mydt$year)

#####V-DEM#####
#negative binomial: DV-MMD
library(MASS)
library(lmtest)
library(sandwich)
mod1 <- glm.nb(freq.pt ~ lag.rev.phyv*lag.rev.csoee + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec, mydt)
coeftest(mod1, vcov = vcovCL, cluster=~COWcode)

cov1 <- vcovCL(mod1, cluster = ~ COWcode, type = "HC1")
robust_se1 <- sqrt(diag(cov1))

fix1 <- glm.nb(freq.pt ~ lag.rev.phyv*lag.rev.csoee + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec +
                 factor(year) - 1, data = mydt)
coeftest(fix1, vcov = vcovCL, cluster=~COWcode)
cov2 <- vcovCL(fix1, cluster = ~ COWcode, type = "HC1")
robust_se2 <- sqrt(diag(cov2))

#latex output1
stargazer(mod1, fix1,
          keep= c("lag.rev.phyv:lag.rev.csoee", "lag.rev.phyv", "lag.rev.csoee",
                  "lag.pt", "lag.dist", "lag.intg", "lag.row", "yrsoffc", "exelec", "Constant"),
          order = c("lag.rev.phyv:lag.rev.csoee", "lag.rev.phyv", "lag.rev.csoee"),
          no.space = T, title = "Interaction effect between physical violence and restriction on CSO entry & exit: V-dem", 
          dep.var.labels = "Annual Count of Anti-government Protests (MMD data)",
          column.labels = c("negative binomial", "year fixed"),
          covariate.labels = c("physical violence * CSO entry/exit", 
                               "physical violence", "CSO entry/exit",
                               "protest in previous year",
                               "equal distribution of resources",
                               "regime interregnum",
                               "regime type (RoW)",
                               "years in office",
                               "executive election", "Constant"),
          se=list(robust_se1, robust_se2), 
          notes = "Robust standard errors clustered on country are in parentheses.",
          type = "latex", out = "vdem1.tex")

#interaction plot
library(interplot)
fixed1 <- glm(freq.pt ~ lag.rev.phyv*lag.rev.csoee + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec +
                factor(year) - 1, mydt, family = negative.binomial(fix1$theta))

interplot(m=fixed1, var1 = "lag.rev.phyv", var2="lag.rev.csoee",  point=T, ci=.95) +
  labs(title = "", x="\nRestriction on CSO entry & exit", 
       y="Estimated effect of physical violence\n on protest frequencey",
       caption = "Note: Indicators for direct and indirect repression are derived from V-dem data. Both indicators are rescaled to make 
       higher values indicating greater repression. Therefore, the positive (negative) effect indicates more (less) dissent 
       as level of repression increases.") +
  scale_x_continuous(labels=c("0"="Unconstrained", "1"="Minimal", "2"="Moderate", 
                              "3"="Substantial", "4"="Monopolitstic")) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line=element_line(),
        axis.text.x = element_text(size = 8)) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_stata()

interplot(m=fixed1, var2 = "lag.rev.phyv", var1="lag.rev.csoee",  point=T, ci=.95) +
  labs(title = "", x="\nPhysical violence", 
       y="Estimated effect of CSO entry/exit\n on protest frequencey",
       caption = "Note: Indicators for direct and indirect repression are derived from V-dem data. Both indicators are rescaled to make 
       higher values indicating greater repression. Therefore, the positive (negative) effect indicates more (less) dissent 
       as level of repression increases.") +
  scale_x_continuous(labels=c()) + 
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line=element_line(),
        axis.text.x = element_text(size = 8)) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_stata()


#####Fariss (2020) dataset: change of physical integrity right#####

mod2 <-  glm.nb(freq.pt ~ fariss_ch*lag.rev.csoee + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec, dt_f)
coeftest(mod2, vcov = vcovCL, cluster=~COWcode)
cov21 <- vcovCL(mod2, cluster = ~ COWcode, type = "HC1")
robust_se21 <- sqrt(diag(cov21))

fix2 <- glm.nb(freq.pt ~ fariss_ch*lag.rev.csoee + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec +
                 factor(year) - 1, data = dt_f)
coeftest(fix2, vcov = vcovCL, cluster=~COWcode)
cov22 <- vcovCL(fix2, cluster = ~ COWcode, type = "HC1")
robust_se22 <- sqrt(diag(cov22))

stargazer(mod2, fix2,
          keep= c("fariss_ch*lag.rev.csoee", "fariss_ch", "lag.rev.csoee",
                  "lag.pt", "lag.dist", "lag.intg", "lag.row", "yrsoffc", "exelec", "Constant"),
          order = c("fariss_ch*lag.rev.csoee", "fariss_ch", "lag.rev.csoee"),
          no.space = T, title = "Interaction effect between physical integrity rights (Fariss 2020) and restriction on CSO entry & exit (V-dem)", 
          dep.var.labels = "Annual Count of Anti-government Protests (MMD data)",
          column.labels = c("negative binomial", "year fixed"),
          covariate.labels = c("physical integrity right * CSO entry/exit", 
                               "physical integrity right", "CSO entry/exit",
                               "protest in previous year",
                               "equal distribution of resources",
                               "regime interregnum",
                               "regime type (RoW)",
                               "years in office",
                               "executive election", "Constant"),
          se=list(robust_se21, robust_se22), 
          notes = "Robust standard errors clustered on country are in parentheses.",
          type = "latex", out = "fariss1.tex")


#plot
p2 <- glm(freq.pt ~ fariss_ch*lag.rev.csoee + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec +
               factor(year) - 1, dt_f,
             family = negative.binomial(fix2$theta))

interplot(m=p2, var1 = "fariss_ch", var2="lag.rev.csoee",  point=T, ci=.95) +
  labs(title = "", x="\nRestriction on CSO entry & exit", 
       y="Estimated effect of change of physical integrity rights protection\n (Fariss 2020) on protest frequencey",
       caption = "Note: Positive value of the change of physical integrity rights protection (Fariss 2020) indicates 
       better physical integrity rights protection at year t-1 compared to year t-2. Thus, the positive 
       coefficient indicates better physical integrity rights, more dissent (worse physical intergrity 
       rights, less dissent). The deterrent effect of direct repression (physical integrity right violation) 
       on anti-government protest exists after the restriction on CSO entry and exit is more than 
       moderate.") +
  scale_x_continuous(labels=c("0"="Unconstrained", "1"="Minimal", "2"="Moderate", 
                              "3"="Substantial", "4"="Monopolitstic")) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line=element_line(),
        axis.text.x = element_text(size = 8)) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_stata()
#As civil society organization becomes more restrictive, better physical integrity rights improvement leads to more dissent.
#More repression, less dissent.

interplot(m=p2, var1="lag.rev.csoee", var2 = "fariss_ch",  point=T, ci=.95) +
  labs(title = "", x="\nChange of physical integrity rights protection\n (Fariss 2020)", 
       y="Estimated effect of restriction on CSO entry/exit\n on protest frequencey",
       caption = "Note: Positive value of the change of physical integrity rights protection (Fariss 2020) indicates 
       better physical integrity rights protection at year t-1 compared to year t-2. The positive coef
       ficient in this plot indicates more restriction on CSO entry and exit, more dissent. The effect 
       of indirect repression (restriction on CSO entry and exit) on anti-government protest becomes 
       positive (backlash effect) as physical integrity rights protection becomes better at year t-1 
       than year t-2.") +
  scale_x_continuous(labels=c()) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line=element_line(),
        axis.text.x = element_text(size = 8)) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_stata()


######SRCS#####
#activities_restrict
names(dt_srcs)
mod3 <-  glm.nb(freq.pt ~ fariss_ch*lag.hro_restriction + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec, dt_srcs)
coeftest(mod3, vcov = vcovCL, cluster=~COWcode)
cov3 <- vcovCL(mod3, cluster = ~ COWcode)
robust_se3 <- sqrt(diag(cov3))


fix3 <- glm.nb(freq.pt ~ fariss_ch*lag.hro_restriction + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec +
                 factor(year) - 1, data = dt_srcs)
coeftest(fix3, vcov = vcovCL, cluster=~COWcode)
cov33 <- vcovCL(fix3, cluster = ~ COWcode)
robust_se33 <- sqrt(diag(cov33))


stargazer(mod3, fix3,
          keep= c("fariss_ch*lag.hro_restriction", "fariss_ch", "lag.hro_restriction",
                  "lag.pt", "lag.dist", "lag.intg", "lag.row", "yrsoffc", "exelec", "Constant"),
          order = c("fariss_ch:lag.hro_restriction", "fariss_ch", "lag.hro_restriction"),
          no.space = T, title = "Interaction effect between physical integrity rights (Fariss 2020) and restriction on HRO activities (SRCS)", 
          dep.var.labels = "Annual Count of Anti-government Protests (MMD data)",
          column.labels = c("negative binomial", "year fixed"),
          covariate.labels = c("change of physical integrity right * HRO activity restrictions", 
                               "change of physical integrity right", "HRO activity restrictions",
                               "protest in previous year",
                               "equal distribution of resources",
                               "regime interregnum",
                               "regime type (RoW)",
                               "years in office",
                               "executive election", "Constant"),
          se=list(robust_se3, robust_se33), 
          notes = "Robust standard errors clustered on country are in parentheses.",
          type = "latex", out = "srcs.tex")


#plot
p3 <- glm(freq.pt ~ fariss_ch*lag.hro_restriction + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec + factor(year) - 1, dt_srcs,
          family = negative.binomial(mod3$theta))

interplot(m=p3, var1 = "fariss_ch", var2="lag.hro_restriction",  point=T, ci=.95) +
  labs(title = "",
       y="Estimated effect of change of physical integrity rights\n (Fariss 2020) on protest frequencey",
       caption = "Note: Positive value of the change of physical integrity rights protection (Fariss 2020) indicates 
       better physical integrity rights protection at year t-1 compared to year t-2. The positive coef
       ficient in this plot indicates physical integrity rights protection improvement, more dissent.
       When there is no restriction on HROs activities, improvement of physical integrity rights 
       has no significant effect on the level of dissent whereas improvement of physical integrity 
       rights yields more dissent when HRO activities are restricted.") +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line=element_line(),
        axis.text.x = element_text(size = 8)) + theme_stata() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name = "Restriction on Human Rights Organizations Activities", 
                     breaks = c(0, 1), 
                     labels = c("   Unrestricted", "Restricted"))



###=====================below this line is incomplete=============================
#####CIRI#####
names(dt_srcs)
summary(dt_srcs$srcs_act)
lm1 <- lm(lag.fariss_phy ~ lag.rev.phyv, dt_fs)
summary(lm1)

names(dt_srcs)
test <-  glm.nb(freq.pt ~ fariss_ch*lag.srcs_act + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec, dt_srcs)
coeftest(test, vcov = vcovCL, cluster=~COWcode)

plot_model(mod2, type = "pred", terms = c("lag.fariss_phy", "csoee"))

t1 <- glm(freq.pt ~ fariss_ch*lag.srcs_act + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec, dt_srcs,
             family = negative.binomial(test$theta))

interplot(m=t1, var1 = "fariss_ch", var2="lag.srcs_act",  point=T, ci=.95) +
  labs(title = "",
       y="Estimated effect of change of physical integrity rights\n (Fariss 2020) on protest frequencey",
       caption = " ") +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line=element_line(),
        axis.text.x = element_text(size = 8)) + theme_stata() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name = "Restriction on Human Rights Organizations", 
                     breaks = c(0, 1), 
                     labels = c("Unrestricted", "Restricted"))

fix_nb2 <- glm.nb(freq.pt ~ rev.lag.cirip*lag.censor + lag.pt + lag.corr + lag.dist + lag.reint + yrsoffc + exelec + 
                    factor(year) - 1 + factor(COWcode) - 1, data = dt2)
coeftest(fix_nb2, vcov = vcovHC(fix_nb2, type = "HC1"))
cov2 <- vcovHC(fix_nb2, type = "HC1")
robust_se2 <- sqrt(diag(cov2))

stargazer(mod2, fix_nb2,
          keep= c("lag.cirip:lag.HRr", "lag.cirip", "lag.HRr",
                  "lag.pt", "lag.corr", "lag.dist", "lag.reint", "yrsoffc", "exelec", "Constant"),
          order = c("lag.cirip:lag.HRr", "lag.cirip", "lag.HRr"),
          no.space = T, title = "Interaction effect between physical violence and restriction on CSOs", 
          dep.var.labels = "Annual Count of Anti-government Protests",
          column.labels = c("negative binomial", "year fixed", "country-year fixed"),
          covariate.labels = c("physical violence * CSO repression", 
                               "physical violence", "CSO repression",
                               "protest in previous year", "equal distribution of resources",
                               "legislative competitiveness",
                               "executive competitiveness",
                               "legislative election",
                               "executive election", "Constant"),
          se=list(robust_se1, robust_se2), type = "latex", out= "vdem.tex")


#interaction plot
fixed2 <- glm(freq.pt ~ rev.lag.cirip*lag.HRb + lag.pt + lag.corr + lag.dist + lag.reint + yrsoffc + exelec + 
               factor(year)-1 + factor(COWcode)-1, dt2, family = negative.binomial(fix_nb2$theta))

interplot(m=fixed2, var1 = "rev.lag.cirip", var2="lag.HRb",  point=T, ci=.95) +
  labs(title = "", x="\nState restirction on civil society organizations", 
       y="Estimated effect of physical violence\n on protest frequencey") +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line=element_line(),
        axis.text.x = element_text(size = 8)) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_stata()

