IntGrad_calc <- function(model, train, test, y, task){

    test <- test[which(names(test) != y)]


    intgrads <- IntGrad_reg(model, train, test, y)


    return(intgrads)

}

IntGrad_reg <- function(model, train, test, y){

  torch_model = torch::torch_load(model$fit$model_obj)

  converted_model <-
    innsight::convert(torch_model$model,
                      input_dim = model$fit$dims$p
    )

  intgrads <- innsight::run_intgrad(converted_model, data = test)

  intgrads = as.data.frame(intgrads$get_result())

  names(intgrads) = names(test)

  intgrads = dplyr::select(intgrads, names(test))

  return(intgrads)

}

IntGrad_bin <- function(model, train, test, y){

    torch_model = torch::torch_load(model$fit$model_obj)

    converted_model <-
      innsight::convert(torch_model$model,
              input_dim = model$fit$dims$p
      )

    intgrads <- innsight::run_intgrad(converted_model, data = test, verbose = F)

    intgrads = as.data.frame(intgrads$get_result())

    names(intgrads) = names(test)

    intgrads = dplyr::select(intgrads, names(test))

    return(intgrads)

}





