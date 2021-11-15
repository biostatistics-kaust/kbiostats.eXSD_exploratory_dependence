complex_cov_to_cor = function (A){
    p <- ncol(A)
    a <- sqrt(1/diag(A))
    Pi <- Conj(a) * A * rep(a, each = p)
    Pi
}

normalize_PDC_matrix <- function (A){
    p <- ncol(A)
    A <- (diag(p) - A)
    inv_a <- 1 / sqrt(colSums(abs(A) ^ 2))
    Pi <- A * rep(inv_a, each = p)
    Pi
}

normalize_GPDC_matrix = function (A, Sigma){
    p <- ncol(A)
    inv_sigmas <- sqrt(1 / diag(Sigma))
    A <- (diag(p) - A) * inv_sigmas
    inv_a <- 1 / sqrt(colSums(abs(A) ^ 2))
    #Pi <- A * rep(inv_a, each = p) * Conj(rep(inv_a, each = p))
    Pi <- A * rep(inv_a, each = p)
    Pi
}


partial_directed_coherence_singleton <- function(Phi, Sigma, w, type="PDC") {
    eps_Phi <- + 1e-10 * diag(nrow(Phi))
    A <- phi_spectrum(Phi, w)
    Pi <- if(type=="PDC") {normalize_PDC_matrix(A)}
          else if(type=="GPDC") {normalize_GPDC_matrix(A, Sigma)}
          else {NULL}
    #Pi <- (A)
    Pi
}

abs_partial_directed_coherence_freqs <- function(Phi, Sigma, freqs=(0:100)/200, type="PDC") {
  stopifnot(all(freqs >= 0))
  stopifnot(all(freqs <= 0.5))
    Sx <- lapply(freqs, function(p) abs(partial_directed_coherence_singleton(Phi, Sigma, p, type)))
    # Snippet for accessing a cell i, j
    # get_item = function(coh, i, j) sapply(1:length(coh), function(p) coh[[p]][i, j])
    Sx
}

#ONLY VAR-related methods allowed
partial_directed_coherence <- function(data, var_order, var_estimation_type=NULL, number_freq_points=200, type="PDC"){
	model <- spectrum_model(data, var_order=var_order, estimation_type=var_estimation_type, number_freq_points=number_freq_points)
    freqs <- (0:(number_freq_points-1)) / ( 2 *number_freq_points)
    coh = abs_partial_directed_coherence_freqs(model$Phi, model$Sigma, freqs, type)
    list(
        freqs=freqs,
        coh=coh
    )
}


