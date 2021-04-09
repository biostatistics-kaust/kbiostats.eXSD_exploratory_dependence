partial_coherence <- function(data, var_order, estimation_type=NULL, number_freq_points=200, variance_window_C_T=11, n_trials=10, win_spans=seq(2, 20, by=2), win="gausswin"){
	model <- spectrum_model(data, var_order=var_order, estimation_type=estimation_type, number_freq_points=number_freq_points, variance_window_C_T=variance_window_C_T, n_trials=n_trials, win_spans=win_spans, win=win)
    freqs <- (0:(number_freq_points-1)) / ( 2 *number_freq_points)
    coh <- partial_coherence_freqset(model, freqs)
    list(
        freqs=freqs,
        coh=coh
    )
}


