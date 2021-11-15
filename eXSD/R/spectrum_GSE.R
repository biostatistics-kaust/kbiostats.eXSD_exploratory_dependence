#  > matrix(1:16, ncol=4)
#       [,1] [,2] [,3] [,4]
#  [1,]    1    5    9   13
#  [2,]    2    6   10   14
#  [3,]    3    7   11   15
#  [4,]    4    8   12   16
#  
#  > matrix(matrix(1:16, ncol=4), ncol=4 ^ 2)
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#  [1,]    1    2    3    4    5    6    7    8    9    10    11    12    13    14
#       [,15] [,16]
#  [1,]    15    16
#  

default_frequency_range <- function(n_freqs){
	n_positive_freqs <- floor(n_freqs/2) + 1
	freqs <-  c(1:n_positive_freqs-1, (-n_positive_freqs - (n_freqs %% 2) + 2):-1) / n_freqs
	freqs
}

HS_abs <- function(v) sum(abs(v))

# X: rows x cols dimmension
raw_periodogram <- function(X){
	d <- apply(X, 2, fft)
	#I <- (d %*% Conj(t(d))) / nrow(d)
	# I <- matrix(apply(d, 1, function(y) abs(y %*% Conj(t(y)))), ncol=ncol(d) ^ 2) / nrow(d)
	I <- t( matrix(c(apply(d, 1, function(y) abs(y %*% Conj(t(y))))), nrow=ncol(d)^2) ) / nrow(d) * sqrt(2*pi)
	I
}

#  > G = matrix(1:40, ncol=4)
#  > G
#        [,1] [,2] [,3] [,4]
#   [1,]    1   11   21   31
#   [2,]    2   12   22   32
#   [3,]    3   13   23   33
#   [4,]    4   14   24   34
#   [5,]    5   15   25   35
#   [6,]    6   16   26   36
#   [7,]    7   17   27   37
#   [8,]    8   18   28   38
#   [9,]    9   19   29   39
#  [10,]   10   20   30   40
#  > stats::filter(G, signal::triang(3), circular=TRUE)
#  Time Series:
#  Start = 1
#  End = 10
#  Frequency = 1
#     [,1] [,2] [,3] [,4]
#   1    7   27   47   67
#   2    4   24   44   64
#   3    6   26   46   66
#   4    8   28   48   68
#   5   10   30   50   70
#   6   12   32   52   72
#   7   14   34   54   74
#   8   16   36   56   76
#   9   18   38   58   78
#  10   15   35   55   75
#  > stats::filter(G[,1], signal::triang(3), circular=TRUE)
#   [1]  7  4  6  8 10 12 14 16 18 15
smoothed_periodogram <- function(X, win_span=nrow(X)%/%10, win=signal::hanning){
	T <- nrow(X)
	win <- win(win_span)
	win_factor <- sum(abs(win))
	#print(win); print(paste("W=", win_factor))
	I_w <- raw_periodogram(X)
	f_tilde <- matrix(stats::filter(I_w, win, circular=TRUE), ncol=ncol(I_w)) / win_factor
	#f_tilde <- matrix(stats::filter(I_w, win, circular=TRUE), nrow=nrow(I_w))
	f_tilde
}


smoothed_periodogram_trial <- function(X, i0, i1, win_span=101, win=signal::hanning, return_periodogram=FALSE){
	T <- nrow(X)
	index_test <- i0: i1
	index_train <- c()
	if(i0 > 1){
		index_train <- 1:(i0 - 1)
	}
	if(i1 < T){
		index_train <- c(index_train, (i1 + 1): T)
	}
	#
	#X_test <- matrix(X[index_test,], ncol=ncol(X))
	#X_train <- matrix(X[index_train,], ncol=ncol(X))
	# X test is filled with zeros to perform a sinc interpolation in freq. domain
	X_train <- matrix(rep(0, length(X)), ncol=ncol(X))
	X_train[1:length(index_train), ] <- X[index_train,]
	X_test <- matrix(rep(0, length(X_train)), ncol=ncol(X_train))
	X_test[1:length(index_test), ] <- X[index_test,]
	#
	I_test <- smoothed_periodogram(X_test, win_span=win_span, win=win)
	I_train <- smoothed_periodogram(X_train, win_span=win_span, win=win)
	#
	#### plotX = function(X){ plot(X[,1], type="l"); sapply(2:ncol(X), function(i) {plot(X[,i], col=i, type="l", main=paste("col =", i)); i}) }
	#### par(mfrow=c(2,2))
	#### plotX(I_test)
	#### par(mfrow=c(2,2))
	#### plotX(I_train)
	#print(dim(I_test))
	#print(dim(I_train))
	#print(c(-1, -1, -1))
	#print(sum(colSums(abs(I_test - I_train) ^2)))
	if(return_periodogram){
		I_train
	}else{
		sum(colSums(abs(I_test - I_train) ^ 2) / nrow(I_train))
	}
}


PURE_procedure_auto_span <- function(X, n_trials=10, win_spans=c(2, nrow(X)%/%5, nrow(X)%/%10), win=signal::gausswin){
	N <- nrow(X)
	n_span <- length(win_spans)
	R_n <- matrix(rep(0, n_trials * n_span), ncol=n_span)
	for(i in 1:n_trials){
		i0 <- 1 + ceiling(N / n_trials * (i-1))
		i1 <- ceiling(N / n_trials * i)
		#print(c(i, i0, i1))
		for(k in 1:n_span){
			R_n[i, k] <- smoothed_periodogram_trial(X, i0, i1, win_span=win_spans[k], win=win)
		}
	}
	print("R_n:")
	print(round(R_n, 2))
	R_n <- colSums(R_n) / n_trials
	#print(paste0("R_n:", paste(R_n, collapse=",")))
	min_R <- which.min(R_n)
	M_T <- win_spans[min_R]
	print(paste0("Selected M_T:", M_T))
	M_T
}

PURE_procedure_for_trial <- function(X, index_trial=1, n_trials=10, win_spans=c(2, nrow(X)%/%5, nrow(X)%/%10), win=signal::gausswin){
	N <- nrow(X)
	n_span <- length(win_spans)
	R_n <- rep(0, n_span)
	i0 <- 1 + ceiling(N / n_trials * (index_trial-1))
	i1 <- ceiling(N / n_trials * index_trial)
	#print(c(index_trial, i0, i1))
	for(k in 1:n_span){
		R_n[k] <- smoothed_periodogram_trial(X, i0, i1, win_span=win_spans[k], win=win)
	}
	print(paste0("R_n:", paste(round(R_n, 2), collapse=",")))
	min_R <- which.min(R_n)
	M_T <- win_spans[min_R]
	print(paste0("M_T:", M_T))
	#M_T
	f_tilde_trial <- smoothed_periodogram_trial(X, i0, i1, win_span=M_T, win=win, return_periodogram=TRUE)
	f_tilde_trial
}

#@Deprecated???
PURE_procedure_full <- function(X, n_trials=10, win_spans=c(2, nrow(X)%/%5, nrow(X)%/%10), win=signal::gausswin){
	M_T <- PURE_procedure_auto_span(X=X, n_trials=n_trials, win_spans=win_spans, win=win)
	f_tilde <- smoothed_periodogram(X, win_span=M_T, win=win)
	f_tilde
}

PURE_procedure <- function(X, n_trials=10, win_spans=c(2, nrow(X)%/%5, nrow(X)%/%10), win=signal::gausswin){
	f_tilde <- 0
	for(i in 1:n_trials){
		f_tilde_trial <- PURE_procedure_for_trial(X, index_trial=i, n_trials=n_trials, win_spans=win_spans, win=win)
		f_tilde <- f_tilde_trial / n_trials
	}
	f_tilde
}

general_shrinkage_estimator <- function(X, var_lag=4, C_T=11, n_trials=10, win_spans=seq(2, 20, by=2), win=signal::gausswin, apply_max_normalization=TRUE){
	#
	delta_C <- function(C_T) (C_T %/% 2 - 1)
	Hilbert_Schmidt_norm <- function(Y) mean(abs(rowSums(Y) ^ 2) / ncol(Y))
	sum_over_variance <- function(f_estimator, f_0, C_T) sapply(1:nrow(f_estimator), function(idx) Hilbert_Schmidt_norm(
		f_estimator[idx,] - 
		f_0[seq(max(1, idx - delta_C(C_T) ), min(nrow(f_0), idx + delta_C(C_T) )), ]
	))
	#
	normalize_to_max <- if(apply_max_normalization) (function(X) X/max(X)) else (function(X) X)
	#
	N_x <- ceiling(nrow(X)/2)
	#
	f_0 <- normalize_to_max(raw_periodogram(X)[1:(N_x+1),])
	f_tilde <- normalize_to_max(PURE_procedure(X, n_trials=n_trials, win_spans=win_spans, win=win)[1:(N_x+1),])
	#
	var_X <- var_coefficients(X, var_lag)
	V_tilde <- normalize_to_max(t(sapply(seq(0, N_x) / nrow(X), function(w) c(abs(phi_spectrum(var_X$Phi, w=w))^2))))
	#
	spectrum_max <- max(max(f_0), max(f_tilde), max(V_tilde))
	beta2_w <- sum_over_variance(f_tilde, f_0, C_T=C_T)
	alpha2_w <- sum_over_variance(V_tilde, f_0, C_T=C_T)
	delta2_w <- 0.5 * (sum_over_variance(V_tilde, f_tilde, C_T=C_T) + sum_over_variance(f_tilde, V_tilde, C_T=C_T))
	#
	hat_W <- (beta2_w - 0.5 * (alpha2_w + beta2_w - delta2_w) ) / (delta2_w)
	hat_W[hat_W < 0] <- 0
	hat_W[hat_W > 1] <- 1
	#
	f_star <- hat_W * V_tilde + (1 - hat_W) * f_tilde
	#
	freqs <- default_frequency_range(nrow(f_star))
	#
	spectr <- list(spectrum=f_star, freqs=freqs)
	#
	class(spectr) <- "modelNonParametric"
	spectr
}

non_parametric_spectrum_FFT <- function(X, win_span=nrow(X)%/%10, win=signal::hanning){
	f_star <- smoothed_periodogram(X, win_span=win_span, win=win)
	freqs <- default_frequency_range(nrow(f_star))
	spectr <- list(spectrum=f_star, freqs=freqs)
	class(spectr) <- "modelNonParametric"
	spectr
}

non_parametric_spectrum_PURE <- function(X, n_trials=10, win_spans=c(2, nrow(X)%/%5, nrow(X)%/%10), win=signal::gausswin){
	f_star <- PURE_procedure_full(X, n_trials=n_trials, win_spans=win_spans, win=win)
	freqs <- default_frequency_range(nrow(f_star))
	spectr <- list(spectrum=f_star, freqs=freqs)
	class(spectr) <- "modelNonParametric"
	spectr
}

estimate_spectrum.modelNonParametric <- function(model, w){
	index <- which.min(abs(model$freqs-w))
	freq <- model$freqs[index]
	spectrum_coeff <- model$spectrum[index,]
	spectrum_nrow <- ceiling(sqrt(length(spectrum_coeff)))
	spectrum_coeff <- matrix(spectrum_coeff, nrow=spectrum_nrow)
	spectrum_coeff
}


estimate_SPCA <- function(model, X, n_components=2){
	UseMethod("estimate_SPCA", model)
}

estimate_SPCA.modelNonParametric <- function(model, X, n_components=2){
	d <- apply(X, 2, fft)
	#
	n_freqs <- length(model$freqs)
	n_positive_freqs <- floor(n_freqs/2) + 1
	spectrum_nrow <- ncol(X)#ceiling(sqrt((spectrum_coeff)))
	explained_variance <- matrix(rep(0, n_freqs * n_components), nrow=n_freqs)
	lambda_factors <- matrix(rep(0, n_freqs * n_components), nrow=n_freqs)
	C_factors <- list()
	for(index_factor in 1:n_components){
		C_factors[[index_factor]] <- matrix(rep(0, spectrum_nrow * n_freqs), nrow=n_freqs)
	}
	for(index in 1:n_freqs){
		negative_index <- if (index <= 1 ) -1 else (n_freqs - index + 2) # index should be > 1
		# Apply PCA on each frequency
		freq <- model$freqs[index]
		spectrum_coeff <- model$spectrum[index,]
		spectrum_coeff <- matrix(spectrum_coeff, nrow=spectrum_nrow)
		spectrum_eig <- eigen(spectrum_coeff)
		ordered_eigvals <- sort(spectrum_eig$values, decreasing=TRUE, index.return=TRUE)
		spectrum_eig$values <- ordered_eigvals$x
		spectrum_eig$vectors <- spectrum_eig$vectors[, ordered_eigvals$ix]
		explained_variance[index,] <- cumsum(spectrum_eig$values[1:n_components]) / sum(spectrum_eig$values)
		lambda_factors[index,] <- spectrum_eig$values
		
		for(index_factor in 1:n_components){
			C_factors[[index_factor]][index, ] <- spectrum_eig$vectors[, index_factor]
			if(negative_index > 0){ # Get the equivalent in the negative domain
				C_factors[[index_factor]][negative_index, ] <- Conj(spectrum_eig$vectors[, index_factor])
			}
		}
		if(negative_index > 0){ # Get the equivalent in the negative domain
			explained_variance[negative_index,] <- explained_variance[index,]
			lambda_factors[negative_index,] <- lambda_factors[index,]
		}
	}
	#Obtain factors:
	time_factors <- matrix(rep(0, n_freqs * n_components), nrow=n_freqs)
	for(index_factor in 1:n_components){
		time_factors[, index_factor] <- Re(fft(rowSums(C_factors[[index_factor]] * d), inverse=TRUE)) / n_freqs
	}
	
	list(
		freqs=model$freqs,
		spectrum=model$spectrum,
		factors=time_factors,
		explained_variance=explained_variance,
		lambda_factors=lambda_factors,
		C_factors=C_factors
	)
}



if(!TRUE){
	x.2 = sin(2*3.1416*(1:200)/100*40)
	x.1 = sin(2*3.1416*(1:200)/100*1)
	X=cbind(x.1, x.2)
	X=X + 0.1 * matrix(rnorm(length(X)), ncol=ncol(X))
	f_star <- general_shrinkage_estimator(X, var_lag=4, C_T=11, apply_max_normalization=TRUE)
	par(mfrow=c(2,2)); for(k in 1:4) plot(f_star[,k], type="l")
}

if(FALSE){
	x.2 = sin(2*3.1416*(1:200)/100*40)
	x.1 = sin(2*3.1416*(1:200)/100*1)
	X=cbind(x.1, x.2)
	X=X + 0.1 * matrix(rnorm(length(X)), ncol=ncol(X))
	#PURE_procedure_for_trial(X, index_trial=1, n_trials=10, win_spans=c(3, 10, 100, 50))
	f_tilde <- PURE_procedure(X, n_trials=10, win_spans=c(3, 10, 100, 50), win=signal::triang)
	par(mfrow=c(2,2)); for(k in 1:4) plot(f_tilde[,k], type="l")
	#dim(f_tilde)


	smooth_over_boxcar <- function(matrix_D, C_T) matrix(stats::filter(matrix_D, signal::boxcar(C_T), circular=T)/C_T, ncol=ncol(matrix_D))
	#
	#sum_over_variance <- function(f_estimator, f_0, C_T) abs(f_estimator - #smooth_over_boxcar(f_0, C_T)) ^ 2
	#Hilbert–Schmidt norm
	#sum_over_variance <- function(f_estimator, f_0, C_T) rowSums(abs(f_estimator - smooth_over_boxcar(f_0, C_T)) ^ 2)

	delta_C <- function(C_T) (C_T %/% 2 - 1)
	Hilbert_Schmidt_norm <- function(Y) mean(abs(rowSums(Y) ^ 2) / ncol(Y))
	sum_over_variance <- function(f_estimator, f_0, C_T) sapply(1:nrow(f_estimator), function(idx) Hilbert_Schmidt_norm(
		f_estimator[idx,] - 
		f_0[seq(max(1, idx - delta_C(C_T) ), min(nrow(f_0), idx + delta_C(C_T) )), ]
	))



	normalize_to_max <- function(X) X/max(X)
	var_lag <- 4
	C_T <- 11
	N_x <- ceiling(nrow(X)/2)
	#
	f_0 <- normalize_to_max(raw_periodogram(X)[1:(N_x+1),])
	f_tilde <- normalize_to_max(PURE_procedure(X, n_trials=10, win_spans=seq(2, 20, by=2), win=signal::gausswin)[1:(N_x+1),])
	#
	source("D:/Marco/Projects-2.0/OP-Consulting/neoprojects/P21.TSDependency/time-series-dependency/var/method.R", chdir=T)
	#source("D:/Marco/Projects-2.0/OP-Consulting/neoprojects/P21.TSDependency/time-series-dependency/gse/method.R", chdir=T)
	var_X <- var_coefficients(X, var_lag)
	V_tilde <- normalize_to_max(t(sapply(seq(0, N_x) / nrow(X), function(w) c(abs(phi_spectrum(var_X$Phi, w=w))^2))))

	#
	spectrum_max <- max(max(f_0), max(f_tilde), max(V_tilde))
	beta2_w <- sum_over_variance(f_tilde, f_0, C_T=C_T)
	alpha2_w <- sum_over_variance(V_tilde, f_0, C_T=C_T)
	delta2_w <- 0.5 * (sum_over_variance(V_tilde, f_tilde, C_T=C_T) + sum_over_variance(f_tilde, V_tilde, C_T=C_T))

	hat_W <- (beta2_w - 0.5 * (alpha2_w + beta2_w - delta2_w) ) / (delta2_w)
	hat_W[hat_W < 0] <- 0
	hat_W[hat_W > 1] <- 1

	f_star <- hat_W * V_tilde + (1 - hat_W) * f_tilde

	print(c( length(hat_W), nrow(hat_W), ncol(hat_W) ))
	print(dim(f_0))
	print(dim(f_tilde))
	print(dim(V_tilde))
	par(mfrow=c(1, 1)); plot(hat_W, type="l")
	par(mfrow=c(2,2)); for(k in 1:4) plot(V_tilde[,k], type="l")
	par(mfrow=c(2,2)); for(k in 1:4) plot(f_tilde[,k], type="l")
	par(mfrow=c(2,2)); for(k in 1:4) plot(f_star[,k], type="l")


	#par(mfrow=c(2, 2)); plot(alpha2_w, type="l"); plot(beta2_w, type="l"); plot(delta2_w, type="l"); plot(hat_W, type="l");

}

#
#
#








