#' ---
#' title: "Update on Seizure Detection Analysis"
#' author: "Andrew"
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 3
#' ---
#'
#+ global_options, include = FALSE
# rmarkdown::render("test.R", output_file = paste0("report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html"))
# knitr::spin("test.R", knit = FALSE)
knitr::opts_chunk$set(echo = FALSE,
                      # results = "hide",
                      warning = FALSE,
                      message = FALSE)

rm(list = ls())
#setwd("O:/_other/andrew/research")

#'
#'
#'
#+ include = FALSE
# Packages. ----
library("dplyr")
library("readr")
library("stringr")
library("caret")
# library("doParallel")

# Paramters. ----
seed <- 42
remove_tempvars <- TRUE
do_preprocessing <- FALSE
run_long_processes <- FALSE
try_regression <- TRUE

num_cores <- parallel::detectCores()
num_cores

# Import. ----
data_0 <- read_csv("tidy_data.csv", col_names = FALSE)
names(data_0)


# Clean. ----
num_cols <- ncol(data_0) - 1
num_freqs <- 50
freqs <- seq(1, num_freqs, by = 1)
suffixes <- letters[1:(num_cols / num_freqs)]
suffixes_rep <- sort(rep(suffixes, num_freqs))
freqs_rep <-
  rep(str_c("f", sprintf("%02d", freqs)), num_cols / num_freqs)
freqs_colnames <- str_c(freqs_rep, suffixes_rep)
head(freqs_colnames)
colname_y <- "y_binary"
names(data_0) <- c(freqs_colnames, colname_y)

data_0 <-
  data_0 %>% mutate(y_factor = as.factor(ifelse(y_binary == 1, "positive", "negative")))
names(data_0)

# Prepare. ----
colnames_x_1 <- freqs_colnames
colname_y_1rgs <- colname_y
colname_y_1cls <- "y_factor"

# This is necessary if variable names start with numbers.
add_tickmarks <- function(x) {
  ifelse(grepl("^[0-9]", x) == TRUE, paste0("`", x, "`"), x)
}

get_fmla <- function(var_y, vars_x) {
  vars_x <- add_tickmarks(vars_x)
  var_y <- add_tickmarks(var_y)
  paste0(var_y, " ~ ", paste(vars_x, collapse = " + ")) %>%
    as.formula()
}

fmla_1rgs <- get_fmla(colname_y_1rgs, colnames_x_1)
fmla_1cls <- get_fmla(colname_y_1cls, colnames_x_1)

data_0 %>% count(y_binary)
data_0 %>% count(y_factor)

# Doing this in case I decide to create a different base data_0 set later.
# select() conflicts with a package loaded when running caret models.
data_1 <-
  data_0 %>%
  dplyr::select(c(one_of(
    colname_y_1rgs, colname_y_1cls, colnames_x_1
  )))

if (remove_tempvars == TRUE) {
  rm(list = c("data_0"))
}

# These matrices are used for pre-processing.
# Similar variables created later for train/test splits are used for modeling.
get_mat_data <- function(d, colnames) {
  d %>%
    dplyr::select(one_of(colnames)) %>%
    as.matrix() %>%
    na.exclude()
}

x_1 <- get_mat_data(data_1, colnames_x_1)
y_1rgs <- get_mat_data(data_1, colname_y_1rgs)
y_1cls <- get_mat_data(data_1, colname_y_1cls)

# Pre-Process. ----
# caret: "3.2 Identifying Zero- and Near Zero-Variance Predictors. ----
if (do_preprocessing == TRUE) {
  nzv_1_metrics <- caret::nearZeroVar(x_1, saveMetrics = TRUE)
  nzv_1_metrics
  
  nzv_1_idx <- caret::nearZeroVar(x_1, saveMetrics = FALSE)
  if (length(nzv_1_idx) > 0) {
    x_1_nonzv <- x_1[, -nzv_1_idx]
  }
  
  # caret: "3.3 Identifying Correlated Predictors" ----
  x_1_cor <- cor(x_1)
  summary(x_1_cor[upper.tri(x_1_cor)])
  
  x_1_corhigh_idx <-
    caret::findCorrelation(cor(x_1), cutoff = .75)
  x_1_corhigh_idx
  
  if (length(x_1_corhigh_idx) > 0) {
    x_1_nocorhigh <- x_1[, -x_1_corhigh_idx]
    x_1_nocorhigh_cor <- cor(x_1_nocorhigh)
    summary(x_1_nocorhigh_cor[upper.tri(x_1_nocorhigh_cor)])
  }
  
  # # caret: "3.4 Linear Dependencies" ----
  lcombo_info_1 <- caret::findLinearCombos(x_1)
  lcombo_info_1
  
  if (!is.null(lcombo_info_1$remove)) {
    x_1_nolcombo <- x_1[, -lcombo_info_1$remove]
  }
}
# Model. ----
# UPDATE: DON'T NECESSARILY NEED THIS IF USING CARET'S RE-SAMPLING.
# Create train/test splits.
# set.seed(seed)
# trn_1_idx <-
#   createDataPartition(y_1rgs,
#                       p = 0.8,
#                       list = FALSE,
#                       times = 1)
#
# data_1_trn <- data_1[trn_1_idx, ]
# data_1_tst <- data_1[-trn_1_idx, ]
#
# x_1_trn <- get_mat_data(data_1_trn, colnames_x_1)
# y_1rgs_trn <- get_mat_data(data_1_trn, colname_y_1rgs)
# y_1cls_trn <- get_mat_data(data_1_trn, colname_y_1cls)

#'
#' # Linear Regression
#'
#'  (This is not really an appropriate method because the response variable is categorical.)
#'
# registerDoParallel(num_cores - 2)
# cl <- makeCluster(num_cores - 2)
# registerDoParallel(cl)

# if (try_regression == TRUE) {
trn_control_cv_rgs <-
  caret::trainControl(method = "cv", number = 10)
# trn_control_rcv_rgs <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(seed)
lm_cv_1 <-
  caret::train(
    fmla_1rgs,
    data = data_1,
    method = "lm",
    trControl = trn_control_cv_rgs,
    metric = "RMSE"
  )
lm_cv_1
# lm_cv_1$finalModel
lm_cv_1_pred <- predict(lm_cv_1, data_1)
postResample(pred = lm_cv_1_pred, obs = y_1rgs)


if (run_long_processes == TRUE) {
  set.seed(seed)
  lm_step_cv_1 <-
    caret::train(
      fmla_1rgs,
      data = data_1 %>% dplyr::select(-one_of(colname_y_1cls)),
      method = "lmStepAIC",
      trControl = trn_control_cv,
      trace = FALSE
    )
  lm_step_cv_1
}
# }
#'
#' # Logistic Regression
#'
# Note that the Default re-sampling method is bootstrapping.
trn_control_cv_cls <-
  caret::trainControl(method = "cv",
                      number = 10,
                      classProbs = TRUE)

set.seed(seed)
glm_cv_1 <-
  caret::train(
    fmla_1cls,
    data = data_1,
    method = "glm",
    trControl = trn_control_cv_cls,
    metric = "Kappa"
  )
glm_cv_1
glm_cv_1$results
confusionMatrix(data = glm_cv_1, reference = y_1cls)

glm_cv_1_pred <- predict(glm_cv_1, data_1)
table(glm_cv_1_pred, y_1cls) / nrow(y_1cls)

#'
#'
#'
#+ include = FALSE
# twoClassSummary(dplyr::select(data_1, y_factor), lev = levels(data_1$y_factor))

#'
#'
#'
#+ include = FALSE
# # This has worse accuracy.
# set.seed(seed)
# glm_cv_1b <-
#   caret::train(
#     fmla_1cls,
#     data = data_1,
#     method = "glm",
#     trControl = trn_control_cv_cls,
#     metric = "ROC"
#   )
# glm_cv_1b
# glm_cv_1b$results
#'
#'
#'
#+ include = FALSE
# if (remove_tempvars == TRUE) {
#   rm(list = c("glm_cv_1b"))
# }

#'
#' # Logistic Regression with Penalty
#'
#' (Still needs work...)
#'
#+ include = FALSE
if (run_long_processes == TRUE) {
  set.seed(seed)
  glmnet_cv_basic_1 <-
    glmnet::cv.glmnet(x = data.matrix(x_1),
                      y = data.matrix(y_1cls),
                      family = "binomial")
  glmnet_cv_basic_1$lambda.min
  
  plot(glmnet_cv_basic_1)
  str(glmnet_cv_basic_1)
  summary(glmnet_cv_basic_1)
  str(glmnet_cv_basic_1$glmnet.fit)
  
  glmnet_cv_basic_1_pred <-
    # predict(glmnet_cv_basic_1, as(y_1cls, "dgCMatrix"), s = glmnet_cv_basic_1$lambda.min)
    predict(glmnet_cv_basic_1, as.vector(y_1cls), s = 0.001)
  dim(as(y_1cls, "dgCMatrix"))
  head(as.vector(y_1cls))
  
  set.seed(seed)
  glmnet_cv_default_1 <-
    caret::train(
      x = x_1,
      y = y_1cls,
      method = "glmnet",
      trControl = trn_control_cv_cls
    )
  glmnet_cv_default_1
  str(glmnet_cv_default_1)
  glmnet_cv_default_1$bestTune
  head(glmnet_cv_default_1$finalModel)
}

if (run_long_processes == TRUE) {
  grid_rdg_1 <-
    expand.grid(alpha = 0, lambda = seq(0.00, 1, by = 0.1))
  grid_lso_1 <-
    expand.grid(alpha = 1, lambda = seq(0.00, 1, by = 0.1))
  grid_elnet_1 <-
    expand.grid(alpha = seq(0, 1, by = 0.25),
                lambda = seq(0.1, 10, by = (10 - 0.1) / 5))
  set.seed(seed)
  glmnet_lso_1 <-
    caret::train(
      x = x_1,
      y = y_1cls,
      # preProcess = c("center", "scale")
      method = "glmnet",
      tuneGrid = grid_lso_1
    )
  glmnet_lso_1
}
#'
#' # Random Forest
#'
set.seed(seed)
rf_basic_1 <-
  randomForest::randomForest(fmla_1cls,
                             data = data_1 %>% dplyr::select(-one_of(colname_y_1rgs)),
                             importance = TRUE)
rf_basic_1

# rf_basic_1 %>%
#   randomForest::importance() %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column() %>%
#   as_tibble() %>%
#   arrange(MeanDecreaseAccuracy) %>%
#   slice(1:10)
# randomForest::varImpPlot(rf_basic_1)
table(rf_basic_1$predicted, y_1cls) / nrow(y_1cls)

#'
#'
#'
#+ include = FALSE
if (run_long_processes == TRUE) {
  set.seed(seed)
  rf_default_1 <-
    caret::train(x = x_1,
                 y = y_1cls,
                 method = "rf")
}

if (run_long_processes == TRUE) {
  grid_rf_1 <- data.frame(mtry = seq(2, 10, by = 2))
  set.seed(seed)
  rf_specific_1 <-
    caret::train(
      x = x_1,
      y = y_1cls,
      method = "rf",
      tuneGrid = grid_rf_1
    )
}

#'
#'
#+ include = FALSE

# model_list <-
#   list(
#     lm = lm_cv_1,
#     lm_step = lm_step_cv_1,
#     glm = glm_cv_1,
#     glmnet = glmnet_cv_default_1,
#     glmnet = glmnet_cv_1,
#     rf = rf_default_1
#   )
# model_resamples <- caret::resample(model_list)
# summary(model_resamples)
# compare_models(glmnet_cv_1, glmnet_lso_1)
#'
#'
#'
