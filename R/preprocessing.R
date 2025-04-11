create_recipe <- function(formula, data){

  rec = recipes::recipe(formula = formula, data = data)

  return(rec)
}

standarize_predictors <- function(rec, norm_num_vars){

  if (any(norm_num_vars == "all")){

    rec <- recipes::step_normalize(rec, recipes::all_numeric_predictors())

  } else{

    rec <- recipes::step_normalize(rec, all_of(norm_num_vars))

  }

  return(rec)

}

one_hot_predictors <- function(rec, encode_cat_vars, one_hot = T){

  if (any(encode_cat_vars == "all")){

    rec <- recipes::step_dummy(rec, recipes::all_factor_predictors(), one_hot = one_hot)

  } else{

    rec <- recipes::step_dummy(rec, all_of(encode_cat_vars), one_hot = one_hot)

  }

  return(rec)

}



transformer <- function(df, formula, num_vars = NULL, cat_vars = NULL, norm_num_vars = "all", encode_cat_vars = "all"){

          formula = as.formula(formula)

          if (all.vars(formula)[2] != "."){
            df = df[all.vars(formula)]
          }

          rec = create_recipe(formula = formula, data = df)


          # Check numerical variables are numeric

          if (!is.null(num_vars)) {

              rec <- rec %>% recipes::step_mutate_at(all_of(num_vars),
                                            fn = as.numeric)
          }

          # Check categorical variables are factor

          if (!is.null(cat_vars)) {

              rec <- rec %>% recipes::step_mutate_at(all_of(cat_vars),
                                            fn = as.factor)
          }

          # Normalize selected numerical columns

          if (!is.null(norm_num_vars)) {

            rec <- standarize_predictors(rec = rec, norm_num_vars = norm_num_vars)

          }

          # Encode selected categorical columns

          if (!is.null(encode_cat_vars)){

              rec <- one_hot_predictors(rec = rec, encode_cat_vars = encode_cat_vars)

          }

          tidy_object <- TidyMLObject$new(full_data = df, transformer = rec)

          tidy_object$modify("formula", formula)

          return(tidy_object)

}


