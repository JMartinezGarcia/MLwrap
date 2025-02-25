test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

formula = "y ~ x1 + x2"
formula = as.formula(formula)
df <- data.frame(
  y   = c(1,0),
  x1   = c(2,3),
  x2   = c(4,5)
)

test_that("create_recipe produces recipe object", {

  rec = create_recipe(formula, data = df)

  expect_equal(class(rec), "recipe")

})
