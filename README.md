# Logistic-Lasso
## Logistic Regression Model based on Lasso (Least absolute shrinkage and selection operator)

### Logistic-Lasso
- I made functions to compute the logistic Lasso using coordinate descent on the penalized iteratively reweighted least squares algorithm. 
- I built the helper functions required to add my algorithm as a `parsnip` model so that it can be used in a `tidymodels` workflow. 
- I wrote a short vignette that explains how to use my function within a `tidymodels` workflow to perform classification. This includes how to tune the model. 

### Files
- `functions.R` contains all of the R functions I wrote.
- `make_tidy.R` contains a script that defines and registers the `parsnip` engine for the functions defined in `functions.R`.
