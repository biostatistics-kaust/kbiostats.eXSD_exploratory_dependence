spectral_pca <- function(data, var_order, estimation_type="FFT", variance_window_C_T=1, n_trials=10, win_spans=seq(2, 20, by=2), win="gausswin"){
	#model <- spectrum_model(data, var_order=var_order, estimation_type=estimation_type, number_freq_points=number_freq_points, variance_window_C_T=variance_window_C_T, n_trials=n_trials, win_spans=win_spans, win=win)
	model <- spectrum_model(data, estimation_type=estimation_type, variance_window_C_T=variance_window_C_T, n_trials=n_trials, win_spans=win_spans, win=win)
	n_components <- ncol(data)
	pca <- estimate_SPCA.modelNonParametric(model, X=data, n_components=n_components)
	n_channels <- ncol(data)
	marginal_spectrum <- (1:n_channels) + (1:n_channels - 1) * n_channels
	list(
        freqs=pca$freqs[1:ceiling(nrow(data)/2)],
        spectra=pca$spectrum[1:ceiling(nrow(data)/2), ][, marginal_spectrum],
        PCs=pca$factors,
        pca=pca
    )
}
