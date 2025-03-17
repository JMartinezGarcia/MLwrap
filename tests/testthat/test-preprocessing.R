formula = "y ~ x1 + x2 + x3 + x4"
formula = as.formula(formula)

formula_multiclass = "y2 ~ x1 + x2 + x3 + x4"
formula_multiclass = as.formula(formula_multiclass)

df <- data.frame(
  y = as.factor(c(1,0)),
  y2 = factor(c(0,2), levels = c(0,1,2)),
  x1 = c(2,3),
  x2 = c(4,6),
  x3 = as.factor(c(0,1)),
  x4 = factor(c(0,2), levels = c(0,1,2))
)

test_that("create_recipe produces recipe object", {

  rec = create_recipe(formula = formula, data = df)

  expect_equal(class(rec), 'recipe')

})

test_that("standarize works correctly",{

  rec = create_recipe(formula = formula, data = df)

  # For "all":

  rec_all <- standarize_predictors(rec = rec, norm_num_vars = "all")

  rec_all_prep <- recipes::prep(rec_all, training = df)

  rec_all_bake <- recipes::bake(rec_all_prep, new_data = df)

  expect_equal(rec_all_bake$x1, c(-0.5*sqrt(2), 0.5*sqrt(2)), tolerance=1e-3)

  expect_equal(rec_all_bake$x2, c(-1/sqrt(2), 1/sqrt(2)), tolerance=1e-3)

  # For x1:

  rec_x1 <- standarize_predictors(rec = rec, norm_num_vars = c("x1"))

  rec_x1_prep <- recipes::prep(rec_x1, training = df)

  rec_x1_bake <- recipes::bake(rec_x1_prep, new_data = df)

  expect_equal(rec_x1_bake$x1, c(-0.5*sqrt(2), 0.5*sqrt(2)), tolerance=1e-3)

  expect_equal(rec_x1_bake$x2, df$x2, tolerance=1e-3)

})

test_that("one_hot_predictors works correctly",{

  rec <- create_recipe(formula = formula, data = df)

  # With "all":

  rec_all <- one_hot_predictors(rec = rec, encode_cat_vars = "all")

  rec_all_prep <- recipes::prep(rec_all, training = df)

  rec_all_bake <- recipes::bake(rec_all_prep, new_data = df)

  expect_equal(rec_all_bake$x3_X0, c(1,0))

  expect_equal(rec_all_bake$x3_X1, c(0,1))

  expect_equal(rec_all_bake$x4_X0, c(1,0))

  expect_equal(rec_all_bake$x4_X1, c(0,0))

  expect_equal(rec_all_bake$x4_X2, c(0,1))

  # With only x4:

  rec_all_x4 <- one_hot_predictors(rec = rec, encode_cat_vars = c("x4"))

  rec_all_prep_x4 <- recipes::prep(rec_all_x4, training = df)

  rec_all_bake_x4 <- recipes::bake(rec_all_prep_x4, new_data = df)

  expect_equal(rec_all_bake_x4$x3, df$x3)

  expect_equal(rec_all_bake_x4$x4_X0, c(1,0))

  expect_equal(rec_all_bake_x4$x4_X1, c(0,0))

  expect_equal(rec_all_bake_x4$x4_X2, c(0,1))

})

# test_that("encode_target works correctly",{
#
#   # For binary:
#
#   rec_binary <- create_recipe(formula = formula, data = df)
#
#   rec_binary <- encode_target(rec = rec_binary, type_task = "binary", dep_var = "y")
#
#   rec_binary_prep <- recipes::prep(rec_binary, training = df)
#
#   rec_binary_bake <- recipes::bake(rec_binary_prep, new_data = df)
#
#   expect_equal(rec_binary_bake$y_X1, c(1,0))
#
#   # For multiclass:
#
#   rec_multiclass <- create_recipe(formula = formula_multiclass, data = df)
#
#   rec_multiclass <- encode_target(rec = rec_multiclass, type_task = "multiclass", dep_var = "y2")
#
#   rec_multiclass_prep <- recipes::prep(rec_multiclass, training = df)
#
#   rec_multiclass_bake <- recipes::bake(rec_multiclass_prep, new_data = df)
#
#   expect_equal(rec_multiclass_bake$y2_X0, c(1,0))
#
#   expect_equal(rec_multiclass_bake$y2_X1, c(0,0))
#
#   expect_equal(rec_multiclass_bake$y2_X2, c(0,1))
# })
