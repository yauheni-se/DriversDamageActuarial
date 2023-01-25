# PRESETS ----
library(CASdatasets)
library(tidyverse)
library(MASS)
library(lsr)
library(xts)
library(zoo)
library(sp)
library(rpart)
library(rpart.plot)
library(Hmisc)
library(caret)
rm(list = ls())
set.seed(42)


# DATA PREPARATION ----
data(freMPL9)
df <- freMPL9
rm(freMPL9)
str(df)


df_sel <- df %>% 
  as_tibble() %>% 
  dplyr::filter(DrivAge>=18, Exposure>0, ClaimAmount>0) %>% 
  mutate(ClaimNb = ClaimNbResp+ClaimNbNonResp+ClaimNbParking+ClaimNbFireTheft+ClaimNbWindscreen+OutUseNb) %>% 
  dplyr::filter(!(ClaimAmount !=0 & ClaimNb == 0)) %>%    # filtrujemy zle wpisy z liczb¹ szkód = 0 ale wartoœci¹ szkód != 0
  dplyr::select(ClaimAmount, ClaimNb, Exposure, Gender, DrivAge, BonusMalus, VehUsage, RiskArea) %>%    # wybieramy interesuj¹ce zmienne
  mutate(
    RiskArea = as.factor(RiskArea),
    ClaimAmountLog = log(ClaimAmount)#,
    #ClaimAmountSingle = ClaimAmount/ClaimNb,
    #ClaimAmountSingleLog = log(ClaimAmountSingle)
  )
rm(df)

# EDA ----
# * Functions ----
make_point <- function(variable, df=df_sel, variable_y = "ClaimAmount") {
  df %>% 
    ggplot(aes_string(variable, variable_y)) +
    geom_point(color = "#29d9d9") +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca")
    )
}

make_box <- function(variable, df=df_sel, variable_y = "ClaimAmount") {
  df %>% 
    ggplot(aes_string(variable, variable_y)) +
    geom_boxplot(fill = "#29d9d9") +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca")
    )
}

make_density <- function(variable, df, plot_type = "density") {
  df %>% 
    ggplot(aes_string(x = variable)) +
    do.call(paste0("geom_", plot_type), list(fill = "#29d9d9", alpha = 0.6, color = "#131313")) +
    labs(y = "") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca")
    )
}

make_qq <- function(variable, df) {
  ggplot(df_sel, aes_string(sample = variable)) + 
    stat_qq(color = "#29d9d9") + 
    stat_qq_line(size = 1.2)+
    labs(y = "", x = variable) +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca")
    )
}

make_density_cat <- function(variable, df) {
  df %>% 
    ggplot(aes_string(variable)) +
    geom_bar(color = "#131313", fill = "#29d9d9", alpha = 0.6) +
    geom_text(stat='count', aes(label=..count..), vjust=-1) +
    labs(y = "") +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank()
    )
}

# * Explainable variables ----
variables_main <- c("ClaimAmount", "ClaimAmountLog")

lapply(variables_main, make_density, df_sel)
lapply(variables_main, make_density, df_sel, "histogram")
lapply(variables_main, make_qq, df_sel)

lapply("ClaimNb", make_density, df_sel)
lapply("ClaimNb", make_density, df_sel, "histogram")
df_sel$ClaimNb %>% table()

# * Numeric variables ----
vars_numeric <- c("DrivAge", "Exposure", "BonusMalus")

lapply(vars_numeric, make_density, df_sel)

df_sel <- df_sel %>% 
  mutate(BonusMalusLog = log(BonusMalus))

lapply("BonusMalusLog", make_point, df_sel, "ClaimAmountLog")
lapply(c("DrivAge", "Exposure"), make_point, df_sel, "ClaimAmountLog")

df_sel %>% 
  dplyr::select(ClaimAmountLog, BonusMalusLog, DrivAge, Exposure) %>% 
  cor() %>%
  as_tibble(rownames = "variable") %>% 
  mutate_if(is.numeric, round, 2)


# * Categorical Variables ----
vars_categorical <- c("Gender", "RiskArea", "VehUsage")

lapply(vars_categorical, make_density_cat, df_sel)
lapply(vars_categorical, make_box, df_sel, "ClaimAmountLog")

df_sel <- df_sel %>% 
  mutate(
    RiskAreaGroup = ifelse(RiskArea %in% c(1, 2, 3), "1-3", ifelse(RiskArea %in% c(11, 12, 13), "11+", as.character(RiskArea))),
    RiskAreaGroup = as.factor(RiskAreaGroup),
    VehUsageGroup = ifelse(as.character(VehUsage) == "Professional run", "Professional", as.character(VehUsage)),
    VehUsageGroup = as.factor(VehUsageGroup),
    .keep = "unused"
  ) %>%
  dplyr::select(-BonusMalus) %>% 
  rename(BonusMalus = BonusMalusLog, VehUsage = VehUsageGroup)


# GLM MODEL ----
data_set <- df_sel
rm(df_sel)
data_set

# Model
model_gamma <- glm(ClaimAmount/ClaimNb ~ Exposure+Gender+DrivAge+BonusMalus+RiskAreaGroup+VehUsage, family = Gamma(link = "log"), weights = ClaimNb, data_set)
summary(model_gamma)
exp(-0.689114)*100

# Prediction
prediction_var <- predict(model_gamma, newdata = data_set[1:2, ])    # predicted
real_var <- data_set[1:2, ] %>% mutate(tmp = log(ClaimAmount)/ClaimNb) %>% pull(tmp)    # real
round(((prediction_var-real_var))/real_var, 4)*100

# FEATURE SELECTION ----
model_full <- glm(ClaimAmount/ClaimNb ~ Exposure+Gender+DrivAge+BonusMalus+RiskAreaGroup+VehUsage, family = Gamma(link = "log"), weights = ClaimNb, data_set)
model_sel <- stepAIC(model_full)
summary(model_sel)

drop1(model_full, test = "F")
model_gamma_reduced <- glm(ClaimAmount/ClaimNb ~ Exposure+Gender+DrivAge+RiskAreaGroup+VehUsage, family = Gamma(link = "log"), weights = ClaimNb, data_set)
drop1(model_gamma_reduced, test = "F")
model_gamma_reduced <- glm(ClaimAmount/ClaimNb ~ Exposure+Gender+RiskAreaGroup+VehUsage, family = Gamma(link = "log"), weights = ClaimNb, data_set)
drop1(model_gamma_reduced, test = "F")


# JAKOSC DOPASOWANIA MODELU ----
model_gamma_reduced <- glm(ClaimAmount/ClaimNb ~ Exposure+Gender+DrivAge+BonusMalus+VehUsage, family = Gamma(link = "log"), weights = ClaimNb, data_set)
summary(model_gamma_reduced)
model_prediction_reduced <- predict(model_gamma_reduced, type = "response")


# wykres reszt
tibble(
  res = residuals(model_gamma_reduced,type="deviance"),
  pred = predict(model_gamma_reduced,type="link")
) %>% 
  ggplot() +
  geom_point(aes(pred, res), color = "#29d9d9") +
  labs(x = "Linear predictor", y = "Residuals") +
  theme(
    panel.background = element_rect(fill = "#e4dcdc"),
    plot.background = element_rect(fill = "#e4dcdc"),
    panel.grid.major.y = element_line(color = "#cacaca"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#cacaca")
  ) +
  geom_abline(slope = 0, intercept = 0)

tibble(
  obs = data_set$ClaimAmount/data_set$ClaimNb,
  mu_pred = model_prediction_reduced
) %>% 
  ggplot() +
  geom_point(aes(mu_pred, obs), color = "#29d9d9") +
  labs(x = "Predictions mu", y = "Observations") +
  theme(
    panel.background = element_rect(fill = "#e4dcdc"),
    plot.background = element_rect(fill = "#e4dcdc"),
    panel.grid.major.y = element_line(color = "#cacaca"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#cacaca")
  ) +
  geom_abline(slope = 1, intercept = 0)


#QQ plot
beta <- data_set$ClaimNb/model_prediction_reduced/summary(model_gamma_reduced)$dispersion
alpha <- data_set$ClaimNb/summary(model_gamma_reduced)$dispersion
quantile_residual <- pgamma(data_set$ClaimAmount/data_set$ClaimNb, shape = alpha, rate = beta)
quantile_residual <- qnorm(quantile_residual, 0, 1)
par(bg = "#e4dcdc")
qqnorm(quantile_residual, col = "#29d9d9", pch = 20, xlim = c(-4, 4), ylim = c(-4, 4), bty = "n")
abline(0, 1, col = "black")


# SECOND GLM MODEL ----
summary(model_sel)
model_full2 <- glm(log(ClaimAmount)/ClaimNb~VehUsage+BonusMalus+log(Exposure)+DrivAge+RiskAreaGroup+Gender, family = inverse.gaussian(link = "1/mu^2"), data_set)
model_sel2 <- stepAIC(model_full2)
summary(model_sel2)


# COMPARE GLMS WITH CV ----
mean(boot::cv.glm(data_set, model_gamma_reduced, cost = function(y, yhat) {Metrics::mse(actual = y, predicted = yhat)}, K=10)$delta)
mean(boot::cv.glm(data_set, model_sel2, cost = function(y, yhat) {Metrics::mse(actual = y, predicted = yhat)}, K=10)$delta)
model_glm_best <- model_sel2


# TREE-BASED MODEL ----
model_tree <- rpart(log(ClaimAmount)/ClaimNb ~ VehUsage+BonusMalus+DrivAge, data = data_set)
summary(model_tree)
rpart.rules(model_tree)
rpart.plot(model_tree,tweak=1.5)

printcp(model_tree)
plotcp(model_tree)


# wykres reszt
plot(predict(model_tree),residuals(model_tree))
tibble(
  res = residuals(model_tree),
  pred = predict(model_tree, type = "vector")
) %>% 
  ggplot() +
  geom_point(aes(pred, res), color = "#29d9d9") +
  labs(x = "Linear predictor", y = "Residuals") +
  theme(
    panel.background = element_rect(fill = "#e4dcdc"),
    plot.background = element_rect(fill = "#e4dcdc"),
    panel.grid.major.y = element_line(color = "#cacaca"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#cacaca")
  ) +
  geom_abline(slope = 0, intercept = 0)


# COMPARE TREE WITH GLM ----
#mean(boot::cv.glm(data_set, model_sel2, cost = function(y, yhat) {Metrics::mse(actual = y, predicted = yhat)}, K=5)$delta)    # GLM

mse_metric <- function(data, lev = NULL, model = NULL) {
  mse_val <- Metrics::mse(actual = data$obs, predicted = data$pred)
  c(mse = mse_val)
}

validated_glm <- train(log(ClaimAmount)/ClaimNb ~ VehUsage+BonusMalus+log(Exposure)+DrivAge+RiskAreaGroup,
                       data = data_set,
                       method = "glm",
                       family = inverse.gaussian(link = "1/mu^2"),
                       metric = "mse",
                       trControl= trainControl(method = "cv", number = 10, summaryFunction = mse_metric)
)
validated_tree <- train(log(ClaimAmount)/ClaimNb ~ VehUsage+BonusMalus+log(Exposure)+DrivAge+RiskAreaGroup,
                        data = data_set,
                        method = "rpart",
                        metric = "mse",
                        trControl= trainControl(method = "cv", number = 10, summaryFunction = mse_metric)
                        )
tibble(
  `MSE - tree` = validated_tree$resample %>% arrange(Resample) %>% pull(mse) %>% mean(),
  `MSE - glm` = validated_glm$resample %>% arrange(Resample) %>% pull(mse) %>% mean()
)


tibble(
  tree = validated_tree$resample %>% arrange(Resample) %>% pull(mse),
  glm = validated_glm$resample %>% arrange(Resample) %>% pull(mse),
) %>% 
  mutate(index = row_number()) %>% 
  pivot_longer(cols = c(tree, glm)) %>% 
  rename(model = name) %>% 
  ggplot(aes(index, value, color = model)) +
  geom_point(color = "black", alpha = 0.4, size = 2) +
  geom_line(size = 2) +
  labs(y = "mse value", x = "CV fold index") +
  theme(
    panel.background = element_rect(fill = "#e4dcdc"),
    plot.background = element_rect(fill = "#e4dcdc"),
    legend.background = element_rect(fill = "#e4dcdc"),
    panel.grid.major.y = element_line(color = "#cacaca"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#cacaca")
  )
