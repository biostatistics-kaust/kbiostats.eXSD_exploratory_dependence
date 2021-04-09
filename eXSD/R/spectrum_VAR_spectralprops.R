estimate_coherence <- function(model, w){
	S <- estimate_spectrum(model, w)
    C <- cov2cor(abs(S))
    C
}

coherence_freqset <- function(model, freqs=(0:100)/200) {
	print(paste("COHERENCE", class(model)))
	Sx <- lapply(freqs, function(p) estimate_coherence(model, p))
    # Snippet for accessing a cell i, j
    # get_item = function(coh, i, j) sapply(1:length(coh), function(p) coh[[p]][i, j])
    Sx
}


# https://sccn.ucsd.edu/wiki/Chapter_4.3._A_partial_list_of_VAR-based_spectral,_coherence_and_GC_estimators
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3128923/
estimate_partial_coherence <- function(model, w){
	S <- estimate_spectrum(model, w)
	G <- solve(S)
    P <- cov2cor(abs(G))
    P
}

partial_coherence_freqset <- function(model, freqs=(0:100)/200) {
	print(paste("COHERENCE", class(model)))
	Sx <- lapply(freqs, function(p) estimate_partial_coherence(model, p))
    # Snippet for accessing a cell i, j
    # get_item = function(coh, i, j) sapply(1:length(coh), function(p) coh[[p]][i, j])
    Sx
}

estimate_spectrum <- function(model, w) { UseMethod("estimate_spectrum", model) }


phi_spectrum <- function(Phi, w) {
    phi_rows <- nrow(Phi)
    phi_cols <- ncol(Phi)
    lags <- (1:(phi_cols * phi_rows) - 1) %/% (phi_rows * phi_rows)
    lags <- matrix(lags, nrow=phi_rows) + 1
    complex_exponentials_lags <- exp(-2i * pi * lags * w) * Phi
    #complex_exponentials_lags <- matrix(complex_exponentials_lags, nrow=phi_cols)
    complex_exponentials_lags <- matrix(complex_exponentials_lags, nrow=phi_rows * phi_rows)
    if(phi_cols > phi_rows){
        A <- matrix(rowSums(complex_exponentials_lags),nrow=phi_rows)
    }else{
        A <- matrix(complex_exponentials_lags, nrow=phi_rows)
    }
    A
}

spectrum_VAR <- function(Phi, Sigma, w){
	eps_Phi <- + 1e-10 * diag(nrow(Phi))
    A <- phi_spectrum(Phi, w)
    H <- solve(diag(nrow(Phi)) - A + eps_Phi)
    S <- (H %*% Sigma) %*% t(Conj(H)) + eps_Phi
	S
}

pair_phi_spectrum <- function(Phi, i, j, freqs=(0:100)/200) {
    Sx = abs(sapply(freqs, function(p) spectrum_from_coeffs(Phi, p)[i, j]))
    Sx
}

estimate_spectrum.modelVAR <- function(model, w){
	spectrum_VAR(model$Phi, model$Sigma, w)
}

if(FALSE){
    source("D:/Marco/Projects-2.0/OP-Consulting/neoprojects/P21.TSDependency/time-series-dependency/var/method.R")
    data(Canada)
    Y <- matrix(c(as.numeric(Canada)), ncol=4)
    c1 = var_coefficients(Y, 4)
    c2 = var_coefficients(Y, 4, 1)
    r = coherence(c1$Phi, c1$Sigma, 0.1)
    print(length(r))
    r = coherence(c2$Phi, c2$Sigma, 0.1)
    print(length(r))
    coh = coherence_freqs(c2$Phi, c2$Sigma)
    print(length(coh))
    get_item = function(coh, i, j) sapply(1:length(coh), function(p) coh[[p]][i, j])
    r = get_item(coh, 2, 3)
    plot(r, type="l")
    print(r)
}
