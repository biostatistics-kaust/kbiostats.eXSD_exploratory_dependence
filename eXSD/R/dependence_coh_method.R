coherence <- function(data, var_order, estimation_type=NULL, number_freq_points=200, variance_window_C_T=11, n_trials=10, win_spans=seq(2, 20, by=2), win="gausswin"){
    spec <- spectrum_model(data, var_order=var_order, estimation_type=estimation_type, number_freq_points=number_freq_points, variance_window_C_T=variance_window_C_T, n_trials=n_trials, win_spans=win_spans, win=win)
    freqs <- (0:(number_freq_points-1)) / ( 2 *number_freq_points)
    coh <- coherence_freqset(spec, freqs)
    list(
        freqs=freqs,
        coh=coh
    )
}

#' @export
coherence_as_feature <- function(X, var_order, estimation_type="olsVARS", number_freq_points=10, variance_window_C_T=11, n_trials=10, win_spans=seq(2, 20, by=2), win="gausswin", lstfmt="lst%03d", matfmt="ch%02d", force_name=FALSE){
  spec <- spectrum_model(X, var_order=var_order, estimation_type=estimation_type, number_freq_points=number_freq_points, variance_window_C_T=variance_window_C_T, n_trials=n_trials, win_spans=win_spans, win=win)
  freqs <- (0:(number_freq_points-1)) / ( 2 *number_freq_points)
  coh <- coherence_freqset(spec, freqs)
  matrix_list_as_feature(coh, lstfmt=lstfmt, matfmt=matfmt, force_name=force_name)
}


