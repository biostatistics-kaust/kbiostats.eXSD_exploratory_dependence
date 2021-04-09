var_coefficients <- function(data, var_order, var_type=NULL){
    T <- nrow(data)
    k <- ncol(data)
    if(is.null(var_type) || var_type == "olsVARS"){
        model_results <- vars::VAR(
            data, 
            p=var_order, 
            type="const")
        Phi <- Bcoef(model_results)[,1:(k * var_order)]
        intercepts <- Bcoef(model_results)[, (k * var_order + 1)]
        Sigma <- summary(model_results)$covres
    }else if(var_type == "LassoBigVAR"){
        model <- BigVAR::constructModel(
            as.matrix(data),
            p=var_order,
            struct="Basic",
            gran=c(150,10),
            RVAR=FALSE,
            h=1,
            cv="Rolling",
            MN=FALSE,
            verbose=FALSE,
            IC=TRUE)
        model_results <- cv.BigVAR(model)
        intercepts <- model_results@betaPred[, 1]
        Phi <- model_results@betaPred[, 2:(k * var_order + 1)]
        Sigma <- var(model_results@resids)
        #Bias?# Sigma <- Sigma * (T - k * var_order - 1)  / (T + var_order - 1)
    }else if(var_type == "RawLSE"){
        Z <- expand_lag(as.matrix(data), lags=var_order)
        fitted <- VAR(Z$X, Z$Y, LS.estimator=LSE)
        intercepts <- fitted$beta_intercept
        Phi <- fitted$beta
        Sigma <- fitted$sigma
    }else if(var_type == "LASSLE"){
		Z <- expand_lag(as.matrix(data), lags=var_order)
		fitted <- VAR(Z$X, Z$Y, LS.estimator=LASSLE)
		intercepts <- fitted$beta_intercept
		Phi <- fitted$beta
        Sigma <- fitted$sigma
    }else{
        stop(paste("VAR estimation method:", var_type, "not defined!"))
    }
	model <- list(
        intercepts=intercepts,
        Phi=Phi,
        Sigma=Sigma
    )
	class(model) <- "modelVAR"
	model
}






