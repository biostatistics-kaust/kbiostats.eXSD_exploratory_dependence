spectrum_fft <- function(data){
    N <- nrow(data)
    S_fft = sapply(1:ncol(data), function(c) fft(data[,c]))
    #freqs <- (0:(N - 1)) / N
    #freqs <- freqs[1:(N %/% 2)]
    S_fft <- abs(S_fft[1:(N %/% 2), ])
    S_fft
}

spectrum_fft_interpolated <- function(data, number_blocks=10, block_index=0, freq0=0, freq1=0.25, length_section=400){
    cols <- ncol(data)
    N <- nrow(data)
    block_size <- floor(N / number_blocks)
    N <- block_size * number_blocks
    i0 <- block_index * block_size + 1
    i1 <- (block_index + 1) * block_size
    X <- data[i0: i1, ]
    S_fft <- spectrum_fft(X)
    #
    freq_indexes <- seq(to=0, from=0.5, length.out=nrow(S_fft))
    S_fft_section <- S_fft[(freq_indexes >= freq0) & (freq_indexes <= freq1), ]
    #
    index_in <- 1:nrow(S_fft_section)
    index_out <- seq(from=1, to=nrow(S_fft_section), length.out=length_section)
    S_fft_section <- sapply(1:cols, function(i) splinefun(index_in, S_fft_section[,i])(index_out))
    colnames(S_fft_section) <- colnames(data)
    S_fft_section
}

lagged_dualfreq_coherence <- function(data, number_blocks=10, block_index0=0, freqs0=c(0,0.25), block_index1=0, freqs1=c(0,0.25), length_section=400){
    S0 <- spectrum_fft_interpolated(data, number_blocks=number_blocks, block_index=block_index0, freq0=freqs0[1], freq1=freqs0[2], length_section=length_section)
    S1 <- spectrum_fft_interpolated(data, number_blocks=number_blocks, block_index=block_index1, freq0=freqs1[1], freq1=freqs1[2], length_section=length_section)
    #c(1,2,3)* V * rep(c(1,2,3), each=3)
    m0 <- 1 / (colSums(S0) + 1e-10)
    m1 <- 1 / (colSums(S1) + 1e-10)
    V <- t(S0) %*% S1
    coh <- m0 * V * rep(m1, each=ncol(S1))
    freqs <- (1:nrow(S0) - 1) / (2 * nrow(S0))
    list(
        freqs=freqs,
        spectrum1=S0,
        spectrum2=S1,
        coh=coh
    )
}
