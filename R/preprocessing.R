create_recipe <- function(formula, data){

  rec = recipes::recipe(formula = formula, data = data)

  return(rec)
}


preprocessing <- function(df, formula, dep_var, num_vars, cat_vars, norm_num_vars,
                       encode_cat_vars, encode_dep_var){

          rec = recipes::recipe(formula, data = df)

          # Check numerical variables are numeric

          if (!is.null(num_var)) {
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
            if (norm_num_vars == "all"){

              rec <- rec %>% recipes::step_normalize(all_of(num_vars))

            } else {

              rec <- rec %>% recipes::step_normalize(all_of(norm_num_vars))

            }

          }

          # Encode selected categorical columns

          if (!is.null(encode_cat_vars)){
            if(encode_cat_vars == "all"){

              rec <- rec %>% recipes::step_dummy(all_of(cat_vars), one_hot = T)

            } else{

              rec <- rec %>% recipes::step_dummy(all_of(encode_cat_vars), one_hot = T)

            }
          }

          # Encode target variable

          if (!is.null(encode_dep_var)){

            if (encode_dep_var == "binary"){

              rec <- rec %>% recipes::step_dummy(dep_var)

            } else if (encode_dep_var == "multiclass"){

              rec <- rec %>% recipes::step_dummy(dep_var, one_hot = T)

            }
          }

          return(rec)

    }
