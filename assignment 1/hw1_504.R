library(tidyverse)
library(pscl)
library(MASS)
library(doParallel)
path = "./"
derogatory_file = sprintf("%s/derogatory.csv", path)
derogatory = read_csv(file = derogatory_file)
CreditData = derogatory %>%
  filter(age >= 18) %>%
  mutate(reports = as.integer(reports)) %>%
  mutate_if(sapply(derogatory, is.character), as.factor) %>%
  dplyr::select(-card, -share)
CreditData %>%
  summarize(mu = mean(reports), v = var(reports))

hist_data = table(CreditData$reports) %>%
  data.frame()

hist_data %>%
  rename(reports = Var1, freq = Freq) %>%
  as_tibble() %>%
  mutate(reports = as.integer(reports)) %>%
  ggplot(aes(x = reports, y = freq)) +
  geom_col() +
  theme_bw() +
  lims(y = c(0, 1200)) + 
  geom_line() + 
  labs(x = "Number of Derogatory Credit Reports ", y = "Frequency") +
  geom_text(size = 5,
            aes(x = reports, y = freq, label = freq, vjust = -1))

stats_zero = CreditData %>%
  filter(reports == 0) %>%
  dplyr::select(-owner, -selfemp, -majorcards, -reports) %>%
  summary() %>%
  as.data.frame() %>%
  separate(col = Freq, into = c("stats", "value"), sep = ":") %>%
  dplyr::select(-Var1) %>%
  as_tibble() %>%
  mutate(Var2 = as.character(str_trim(Var2, "left")),
         stats = as.character(str_trim(stats, "right")),
         value = as.numeric(value)) %>%
  pivot_wider(id_cols = Var2,
              names_from = stats,
              values_from = value) %>%
  rename(Variables = Var2) %>%
  mutate(type = "zero")

stats_nonzero = CreditData %>%
  filter(reports != 0) %>%
  dplyr::select(-owner, -selfemp, -majorcards, -reports) %>%
  summary() %>%
  as.data.frame() %>%
  separate(col = Freq, into = c("stats", "value"), sep = ":") %>%
  dplyr::select(-Var1) %>%
  as_tibble() %>%
  mutate(Var2 = as.character(str_trim(Var2, "left")),
         stats = as.character(str_trim(stats, "right")),
         value = as.numeric(value)) %>%
  pivot_wider(id_cols = Var2,
              names_from = stats,
              values_from = value) %>%
  rename(Variables = Var2) %>%
  mutate(type = "nonzero")

stats_zero %>%
  bind_rows(stats_nonzero) %>%
  filter(Variables %in% c("income", "dependents", "active")) %>%
  ggplot( aes(x = Mean, y = Variables, color = Variables, group = type)
  ) +
  geom_point(
    position = position_dodge2(width = 0.5)
  ) +
  geom_errorbar( 
    aes(xmin = `1st Qu.`, xmax = `3rd Qu.`),
    position = position_dodge(width = 0.5),
    alpha = 0.75
  ) + 
  facet_wrap(~type, ncol = 2) +
  theme_bw()

stats_zero %>%
  bind_rows(stats_nonzero) %>%
  filter(!Variables %in% c("income", "dependents", "active")) %>%
  ggplot( aes(x = Mean, y = Variables, color = Variables, group = type)
  ) +
  geom_point(
    position = position_dodge2(width = 0.5)
  ) +
  geom_errorbar( 
    aes(xmin = `1st Qu.`, xmax = `3rd Qu.`),
    position = position_dodge(width = 0.5),
    alpha = 0.75
  ) + 
  facet_wrap(~type, ncol = 2) +
  theme_bw()

table_zero = CreditData %>%
  filter(reports == 0) %>%
  dplyr::select(owner, selfemp, majorcards) %>%
  summary() %>% as.data.frame() %>%
  separate(col = Freq, into = c("stats", "value"), sep = ":") %>%
  dplyr::select(-Var1) %>% as_tibble() %>% 
  mutate(Var2 = as.character(str_trim(Var2, "left")),
         stats = as.character(str_trim(stats, "right")),
         value = as.numeric(value)) %>%
  pivot_wider(id_cols = Var2,
              names_from = stats,
              values_from = value) %>%
  rename(Variables = Var2) %>%
  mutate(type = "zero")

table_nonzero = CreditData %>%
  filter(reports != 0) %>%
  dplyr::select(owner, selfemp, majorcards) %>%
  summary() %>% as.data.frame() %>%
  separate(col = Freq, into = c("stats", "value"), sep = ":") %>%
  dplyr::select(-Var1) %>% as_tibble() %>% 
  mutate(Var2 = as.character(str_trim(Var2, "left")),
         stats = as.character(str_trim(stats, "right")),
         value = as.numeric(value)) %>%
  pivot_wider(id_cols = Var2,
              names_from = stats,
              values_from = value) %>%
  rename(Variables = Var2) %>%
  mutate(type = "nonzero")
table_zero %>%
  bind_rows(table_nonzero) %>%
  mutate(`RATIO(YES/NO)` = sprintf("%.3f", yes / no)) %>%
  rename(Type = type, No = no, Yes = yes) %>%
  arrange(Variables) %>%
  dplyr::select(-Variables) %>%
  relocate(Type, No, Yes, `RATIO(YES/NO)`) %>%
  knitr::kable() %>%
  kableExtra::kable_styling("striped", full_width = TRUE) %>%
  kableExtra::add_header_above(
    header = c(' ' = 1, 'Categorical value' = 2, ' ' = 1)) %>%
  kableExtra::group_rows("Majorcards", start_row = 1, end_row = 2) %>%
  kableExtra::group_rows("Owner", start_row = 3, end_row = 4) %>%
  kableExtra::group_rows("Selfemp", start_row = 5, end_row = 6)
## ZINB model
predictors = colnames(CreditData)[2:10]
count_var = setdiff(predictors, c("expenditure", "owner", "active"))
zero_var = setdiff(predictors, c("months", "active", "expenditure"))
enum_choose <- function(x, k) {
  if(k > length(x)) stop('k > length(x)')
  if(choose(length(x), k)==1){
    list(as.vector(combn(x, k)))
  } else {
    cbn <- combn(x, k)
    lapply(seq(ncol(cbn)), function(i) cbn[,i])
  }
}
for (i in 1:6){
  if(i == 1){
    count_list = enum_choose(count_var, 1)
    } else{
    count_list= append(count_list, enum_choose(count_var, i))
    }
}
for (i in 1:6){
  if(i == 1){
    zero_list = enum_choose(zero_var, 1)
  } else{
    zero_list= append(zero_list, enum_choose(zero_var, i))
  }
}

cl = makeCluster(12)
registerDoParallel(cl)
AIC_df = foreach(i = 1:63,
                 .packages = c("tidyverse", "pscl"),
                 .combine = bind_rows
                  ) %dopar% {
  for(j in 1:63){
    str_formula = paste("reports ~ expenditure + owner + active +",
                        paste(count_list[[i]], collapse = " + "),
                        "| months + active + expenditure + ",
                        paste(zero_list[[j]], collapse = " + "))
    ZINB_mod = zeroinfl(formula = as.formula(str_formula),
                        data = CreditData, dist = "negbin")
    if(j == 1){
      AIC_table = tibble(formula = str_formula, AIC = AIC(ZINB_mod),
                         loglik = as.numeric(logLik(ZINB_mod)),
                         SE = ZINB_mod$SE.logtheta)
      } else{
      AIC_table  = AIC_table %>%
        bind_rows(tibble(formula = str_formula,
                         AIC = AIC(ZINB_mod),
                         loglik = as.numeric(logLik(ZINB_mod)),
                         SE = ZINB_mod$SE.logtheta)
        )
      }
  }
                    AIC_table
                    }
stopCluster(cl)

AIC_df %>%
  arrange(AIC, loglik) %>%
  .[26:30, ] %>%
  dplyr::select(formula, AIC) %>%
  separate(col = formula, into = c("Count", "Zero"), sep = " \\| ") %>%
  separate(col = Count, into = c("Response", "Count"), sep = "reports ~ ") %>%
  dplyr::select(-Response) %>%
  bind_cols(ID = c(1:5)) %>%
  pivot_longer(cols = c(1:2),
               names_to = "Parts",
               values_to = "Variables") %>%
  relocate(ID, Parts, Variables, AIC) %>%
  mutate(AIC = sprintf("%.3f", AIC)) %>%
  mutate(AIC = ifelse(Parts == "Zero", "-", AIC)) %>%
  knitr::kable()
min_formula = paste("reports ~ expenditure + owner + active + age + income +",
"dependents + months | months + active + expenditure + dependents +",
"majorcards")
ZINB_mod = zeroinfl(formula = as.formula(min_formula), data = CreditData, dist = "negbin")
## The mean and variance actually deviate a lot, so the assumption that the 
## derogatory reports satisfy a poisson distribution may not be appropriate
## here.
pois_mod = glm(formula = reports ~ income + expenditure + owner + dependents + months + active,
               data = CreditData,
               family = "poisson")
ZIP_formula = paste("reports ~ expenditure + owner + dependents + active |",
                    "owner + months + active")
ZIP_mod = zeroinfl(formula = as.formula(ZIP_formula), data = CreditData,
                   dist = "poisson")
NB_mod = glm.nb(formula = reports ~ ., data = CreditData)
Model_vec = c("Poisson", "ZIP", "NB", "ZINB")
AIC_vec = c(AIC(pois_mod), AIC(ZIP_mod), AIC(NB_mod), AIC(ZINB_mod))

tibble(Model = Model_vec, AIC = AIC_vec)



Observed = CreditData %>%
  group_by(reports) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count)) %>%
  mutate(p_value = count / total)
true_p = append(Observed$p_value[1:8], 0) %>%
  append(Observed$p_value[9:12]) %>%
  append(0) %>%
  append(Observed$p_value[13])

p_Pois = predprob(pois_mod) %>% colMeans
p_ZIP = predprob(ZIP_mod) %>% colMeans
p_ZINB = predprob(ZINB_mod) %>% colMeans
p_NB = predprob(NB_mod) %>% colMeans
prob_wide = data.frame(x = 0:max(Observed$reports), Poisson = p_Pois, 
                       NegBin = p_NB, ZIP = p_ZIP, ZINB = p_ZINB,
                       Obs = true_p)
prob_long = prob_wide %>%
  pivot_longer(cols = c(2:6),
               names_to = "Model",
               values_to = "prob")

ZINB_sum = summary(ZINB_mod)

count_coef = ZINB_sum$coefficients["count"] %>%
  as.data.frame() %>%
  as_tibble() %>%
  dplyr::select(Estimate = count.Estimate, P_value = count.Pr...z..) %>%
  .[1:8,]
  

zero_coef = ZINB_sum$coefficients["zero"] %>%
  as.data.frame() %>%
  as_tibble() %>%
  dplyr::select(Estimate = zero.Estimate, P_value = zero.Pr...z..)

coef_matrix = count_coef %>%
  bind_rows(zero_coef) %>%
  as.matrix() %>%
  cbind(confint(ZINB_mod))
coef_name = dimnames(coef_matrix)
coef_mtx = coef_matrix %>%
  as_tibble() %>%
  mutate(P_value = sprintf("%.3f", P_value),
         `Estimate(2.5%, 97.5%)` = sprintf("%.3f(%.3f, %.3f)",
                                           Estimate, `2.5 %`, `97.5 %`),
         Signal = ifelse(P_value <= 0.05, "*", " ")) %>%
  dplyr::select(`Estimate(2.5%, 97.5%)`, P_value, Signal) %>%
  as.matrix()
dimnames(coef_mtx)[[1]] = coef_name[[1]]
prob_long %>%
  ggplot(aes(x = x, y = prob, group = Model, col = Model)) +
  geom_line(aes(lty = Model), lwd = 1) +
  theme_bw() +
  labs(x = "Number of Derogatory reports", y = 'Probability',
       title = "Models for number of derogatory reports") +
  scale_color_manual(values = c('blue', 'black', 'purple', 'red', 'green')) +
  scale_linetype_manual(values = c('dotted', 'solid', 'dotted', 'dotted', 'dotted')) +
  theme(legend.position=c(.75, .65), axis.title.y = element_text(angle = 0))
