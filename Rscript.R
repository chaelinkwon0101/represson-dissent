######Interaction model exercise#####
#data
mydt <- read.csv("vdem+mmd.csv")
dt_f <- read.csv("vdem+mmd+fariss.csv")
dt_srcs <- read.csv("vdem+mmd+fariss+srcs.csv")


#negative binomial: DV = protest frequency from MMD data
library(MASS)
library(lmtest)
library(sandwich)
mod1 <- glm.nb(freq.pt ~ lag.rev.phyv*lag.rev.csoee + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec, mydt)
coeftest(mod1, vcov = vcovCL, cluster=~COWcode)

cov1 <- vcovCL(mod1, cluster = ~ COWcode, type = "HC1")
robust_se1 <- sqrt(diag(cov1))

library(fixest)
fix1 <- glm.nb(freq.pt ~ lag.rev.phyv*lag.rev.csoee + lag.pt + lag.dist + lag.intg + lag.row + yrsoffc + exelec +
                 factor(year) - 1, data = mydt)
coeftest(fix1, vcov = vcovCL, cluster=~COWcode)
cov2 <- vcovCL(fix1, cluster = ~ COWcode, type = "HC1")
robust_se2 <- sqrt(diag(cov2))

#latex output1
library(stargazer)
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

#visualization: interaction plot
library(interplot)
library(ggplot2)
library(ggthemes)
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

