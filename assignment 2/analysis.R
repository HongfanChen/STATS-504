library(tidyverse)
library(survival)
path = "./"
retino_file = sprintf("%s/diabeticVision.csv", path)
retino = read_csv(file = retino_file) %>%
  select(-X1) %>%
  mutate(treatment = ifelse(trt == 0,
                            paste(laser, "control"),
                            paste(laser, "treated")
  ) )

survobj = with(retino, Surv (futime, status))
fit0 = survfit(survobj~1, data = retino)
# summary(fit0)
plot(fit0, xlab = "Time to loss of vision in Days", 
     ylab = "% not blinded", yscale = 100,
     main = "Survival Distribution (Overall)")
fit1 = survfit(survobj~treatment, data = retino)
plot(fit1, xlab = "Time to loss of vision in Days", 
     ylab = "% not blinded", yscale = 100,
     main = "Figure 2: Survival Distribution by Treatment (Overall)",
     col = c('tomato3', 'green', 'cyan', 'blue'))
legend('bottomleft',
       legend = levels(as.factor(retino$treatment)),
       col = c('tomato3', 'green', 'cyan', 'blue'),
       lty = 1)

trt_fit = coxph (survobj ~ trt, data = retino)
indep_fit = coxph(survobj~laser + trt + age + risk, data = retino)
trt_fit = coxph(survobj ~ laser + trt + frailty (id), data = retino)
results = summary(trt_fit)

results$conf.int %>%
  as_tibble() %>%
  mutate(Variable = c("LaserXenon", "Treatment"),
         `Rates(95% CI)` = sprintf("%.3f(%.3f, %.3f)",
                                   `exp(coef)`, `lower .95`, `upper .95`)) %>%
  select(Variable, `Rates(95% CI)`) %>%
  add_row(Variable = "Frailty(id)", `Rates(95% CI)` = "-") %>%
  mutate(`P Value` = sprintf("%.3f", results$coefficients[,6]))

results$coefficients[,6]

cluster_fit = coxph(survobj ~ laser*trt + age + risk + frailty (id),
                    data = retino)
results2 = summary(cluster_fit)
results2$conf.int %>%
  as_tibble() %>%
  mutate(Variable = c("LaserXenon vs. LaserArgon", "Treatment: treated eyes vs. controlled eyes", "Age", "Clinical Risk",
                      "Interaction:Xenon vs. Argon ~ Treatment"),
         `Rates(95% CI)` = sprintf("%.3f(%.3f, %.3f)",
                                   `exp(coef)`, `lower .95`, `upper .95`)) %>%
  select(Variable, `Rates(95% CI)`) %>%
  add_row(Variable = "Frailty(id)", `Rates(95% CI)` = "-") %>%
  mutate(`P Value` = sprintf("%.3f", results2$coefficients[,6][c(1,2,3,4,6,5)])) %>%
  knitr::kable(caption = cap_tab3)

