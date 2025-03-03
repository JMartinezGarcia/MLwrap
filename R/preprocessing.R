create_recipe <- function(formula, data){

  rec = recipes::recipe(formula = formula, data = data)

  return(rec)
}

standarize_predictors <- function(rec, norm_num_vars){

  if (norm_num_vars == "all"){

    rec <- recipes::step_normalize(rec, recipes::all_numeric_predictors())

  } else{

    rec <- recipes::step_normalize(rec, all_of(norm_num_vars))

  }

  return(rec)

}

one_hot_predictors <- function(rec, encode_cat_vars, one_hot = T){

  if(encode_cat_vars == "all"){

    rec <- recipes::step_dummy(rec, recipes::all_factor_predictors(), one_hot = one_hot)

  } else{

    rec <- recipes::step_dummy(rec, all_of(encode_cat_vars), one_hot = one_hot)

  }

  return(rec)

}

encode_target <- function(rec, type_task, dep_var){

  if (type_task == "binary"){

    rec <- recipes::step_dummy(rec, all_of(dep_var))

  } else if (type_task == "multiclass"){

    rec <- recipes::step_dummy(rec, all_of(dep_var), one_hot = T)

  }

  return(rec)

}

transformer <- function(df, formula, dep_var, num_vars, cat_vars, norm_num_vars,
                       encode_cat_vars, encode_dep_var){

          rec = create_recipe(formula = formula, data = df)

          # Check numerical variables are numeric

          if (!is.null(num_var)) {

            ##### TODO: num_vars or norm_num_vars,
            #### standarize_predictors no usa norm_num_vars

              rec <- rec %>% recipes::step_mutate_at(all_of(num_vars),
                                            fn = as.numeric)
          }

          # Check categorical variables are factor

          if (!is.null(cat_var)) {

              rec <- rec %>% recipes::step_mutate_at(all_of(cat_vars),
                                            fn = as.factor)
          }

          # Normalize selected numerical columns

          if (!is.null(norm_num_vars)) {

            rec <- standarize(rec = rec, norm_num_vars = norm_num_vars, num_vars = num_vars)

          }

          # Encode selected categorical columns

          if (!is.null(encode_cat_vars)){

              rec <- one_hot_predictors(rec = rec, encode_cat_vars = encode_cat_vars)

          }

          # Encode target variable

          if (!is.null(encode_dep_var)){

              rec <- encode_target(rec = rec, type_task = encode_dep_var, dep_var = dep_var)

          }

          set.seed(123)

          train_test_split <- rsample::initial_split(
            data   = df,
            prop   = 0.8
          )

          train_data <- rsample::training(train_test_split)
          test_data  <- rsample::testing(train_test_split)

          tidy_object <- TidyMLObject$new(dataframe = df, train_data = train_data, test_data = test_data, transformer = rec)

          return(tidy_object)

}


