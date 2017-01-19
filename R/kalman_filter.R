kalman_filter <- function(pp = 1, hh = 1,
                          prior_constant_variance = 10,
                          gg = .1, kk = 1, ll = 1) { # function(yraw, pp, hh, prior_constant_variance, gg, kk, ll)

# generate the data ----------------------------------------
yraw <- cbind(1:3, 11:13)

# initialize the model -------------------------------------
model <- model_initialize(yraw, pp, hh,
  prior_constant_variance, gg, kk, ll)

# assign each object in the model list to a matrix
dd <- model$dd
hh <- model$hh
pp <- model$pp
tt <- model$tt
yy <- model$yy
yraw <- model$yraw
zz <- model$zz
zz_cols_number <- model$zz_cols_number
zz_rows_number <- model$zz_rows_number
beta_predict_expectation <- model$beta_predict_expectation
beta_predict_variance <- model$beta_predict_variance
beta_update_expectation <- model$beta_update_expectation
beta_update_variance <- model$beta_update_variance
yy_predict_expectation <- model$yy_predict_expectation
yy_predict_variance <- model$yy_predict_variance
yy_update_variance <- model$yy_update_variance

for (t in 1:tt) {

}

return(model)

# # build model_probability_predict and
# # model_probability_update
# model_probability_predict <- array(0, dim = c(tt,
#   gg_length * kk_length *
#     ll_length * ff_length))
# model_probability_update <- model_probability_predict
}