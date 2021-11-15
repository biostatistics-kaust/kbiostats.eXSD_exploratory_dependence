VALID_WINDOWS <- c("bartlett", "blackman", "boxcar", "gausswin", "hamming", "hanning", "triang")

spectrum_model <- function(data, estimation_type=NULL, var_order, number_freq_points=200, variance_window_C_T=11, n_trials=10, win_spans=seq(2, 20, by=2), win="gausswin"){
    freqs <- (0:(number_freq_points-1)) / ( 2 *number_freq_points)
	if(estimation_type %in% c(NULL, "olsVARS", "LassoBigVAR", "RawLSE", "LASSLE")){
		model <- var_coefficients(data, var_order, estimation_type)
	}else if(estimation_type == "GSE"){
		if(!(win %in% VALID_WINDOWS)) print(paste("Invalid window", win))
		model <- general_shrinkage_estimator(data, var_lag=var_order, C_T=variance_window_C_T, n_trials=n_trials, win_spans=win_spans, win=match.fun(win))
	}else if(estimation_type == "FFT"){
		if(!(win %in% VALID_WINDOWS)) print(paste("Invalid window", win))
		model <- non_parametric_spectrum_FFT(data, win_span=win_spans[1], win=match.fun(win))
	}else if(estimation_type == "PURE"){
		if(!(win %in% VALID_WINDOWS)) print(paste("Invalid window", win))
		model <- non_parametric_spectrum_PURE(data, n_trials=n_trials, win_spans=win_spans, win=match.fun(win))
	}else{
        stop(paste("Estimation method:", estimation_type, "not defined!"))
    }
	model
}


