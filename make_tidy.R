logistic_lasso <- function(mode = "classification", penalty) {
  
  ## logistic_lasso(mode, penalty) generates a specification 
  ## of a model before fitting logistic lasso
  ##
  ## Input:
  ## - mode: a single character string for the type of model
  ## - penalty: a non-negative number representing the total
  ##          amount of regulation
  ##
  ## Output:
  ## - No output 
  ##
  ## Example:
  ##   library(tidymodels)
  ##   library(tidyverse)
  ##   spec <- ridge_reg(penalty = 0.001) %>% set_engine("fit_logistic_lasso")
  
  args <- list(penalty = rlang::enquo(penalty))
  new_model_spec("logistic_lasso",
                 args = args,
                 mode = mode,
                 eng_args = NULL,
                 method = NULL,
                 engine = NULL)
}

set_new_model("logistic_lasso")
set_model_mode(model = "logistic_lasso", mode = "classification") 
set_model_engine("logistic_lasso",
                 mode = "classification",
                 eng = "fit_logistic_lasso")

set_dependency("logistic_lasso", eng = "fit_logistic_lasso", pkg = "base")

set_model_arg(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  parsnip = "penalty", ## what parsnip will call it
  original = "lambda", ## what we call it!
  func = list(pkg = "dials", fun = "penalty"), ## Use dials::penalty() to set 
  has_submodel = FALSE # If you don't know, don't worry.
)

set_encoding(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification", 
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  ) 
)

show_model_info("logistic_lasso")

set_fit(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification", 
  value = list(
    interface = "matrix", 
    protect = c("x", "y"),
    func = c(fun = "fit_logistic_lasso"), 
    defaults = list()
  )
)

set_pred(
  model = "logistic_lasso",
  eng = "fit_logistic_lasso",
  mode = "classification", 
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict_logistic_lasso"), 
    args = list(
      object = expr(object$fit),
      new_x = expr(as.matrix(new_data[, names(object$fit$beta)]))
    )
  )
)

update.logistic_lasso <- function(object, penalty = NULL, ...){

  ## update.logistic_lasso(object, penalty) updates the logistic 
  ## lasso and generates a specification of a model
  ##
  ## Input:
  ## - object: an existing fit from a model function
  ## - penalty: a non-negative number representing the total
  ##          amount of regulation
  ##
  ## Output:
  ## - No output 
  ##
  ## Example:
  ##   library(tidymodels)
  ##   library(tidyverse)
  ##   spec <- update.logistic_lasso(object, penalty = 0.05)
  
  if(!is.null(penalty)) {
    object$args <- list(penalty = enquo(penalty))
  }
  
  new_model_spec("logistic_lasso", 
                 args = object$args, 
                 eng_args = NULL,
                 mode = "classification", 
                 method = NULL, 
                 engine = object$engine)
}