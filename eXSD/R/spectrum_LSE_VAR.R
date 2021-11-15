apply_lag <- function(X, l, total_lags){
    Y <- X[-c(0:(total_lags-l), (-l):1 + 1 + nrow(X)), ]
    colnames(Y) <- paste0(colnames(X), ".", l)
    Y
}

expand_lag <- function(X, lags){
    t.X <- matrix(sapply(1:lags, function(l) apply_lag(X, l, lags)), ncol=lags*ncol(X))
    colnames(t.X) <- c(sapply(1:lags, function(l) paste0(colnames(X), ".L", l)))
    list(
        X=t.X,
        Y=apply_lag(X, 0, lags)
    )
}

#Z=expand_lag(XX, lags=2)
LSE <- function(X, Y, keep.resid=FALSE){
    M <- cbind(1, X)
    tM_M_inv <- solve(t(M) %*% M)
    H <- tM_M_inv %*% t(M)
    beta_hat <- H %*% Y
    z.err = (Y - M %*% H %*% Y)
    sse_err <- sapply(1:ncol(z.err), function(i) var(z.err[,i]))
    sse_err <- sse_err * (nrow(X)+1)/(nrow(X)-ncol(X)+1)
    beta_std <- sqrt(matrix(diag(tM_M_inv), ncol=1) %*% sse_err)
    colnames(beta_std) <- colnames(beta_hat)
    rownames(beta_std) <- rownames(beta_hat)
    t_val <- beta_hat / beta_std
    m1 <- nrow(beta_hat)
    #m0 <- (if(keep.intercept) 1 else 2)
    list(
        beta_intercept=beta_hat[1],
        std_intercept=beta_std[1],
        t_val_intercept=t_val[1],
        beta=t(beta_hat[2:m1,]),
        std=t(beta_std[2:m1,]),
        t_val=t(t_val[2:m1,]),
        resid=if(keep.resid) z.err else NULL
    )
}

LASSLE <- function(X, y, keep.resid=FALSE){
    cvfit <- cv.glmnet(X, y, nfolds=10)
	nonzero_cols <- as.logical(coef(cvfit, s="lambda.min") != 0)[-c(1)]
	nonzero_cols <- which(nonzero_cols)
	fit_lse <- LSE(matrix(X[,nonzero_cols], ncol=length(nonzero_cols)), y, keep.resid=keep.resid)
    beta_intercept <- fit_lse$beta_intercept
    std_intercept <- fit_lse$std_intercept
    t_val_intercept <- fit_lse$t_val_intercept
    #
    beta <- matrix(rep(0, each=ncol(X)), nrow=1)
    colnames(beta) <- colnames(X)
    beta[nonzero_cols] <- fit_lse$beta
    #
    std <- matrix(rep(0, each=ncol(X)), nrow=1)
    colnames(std) <- colnames(X)
    std[nonzero_cols] <- fit_lse$std
    #
    t_val <- matrix(rep(0, each=ncol(X)), nrow=1)
    colnames(t_val) <- colnames(X)
    t_val[nonzero_cols] <- fit_lse$t_val
    #
    list(
        beta_intercept=beta_intercept,
        std_intercept=std_intercept,
        t_val_intercept=t_val_intercept,
        beta=beta,
        std=std,
        t_val=t_val,
        resid=fit_lse$resid
    )
}

VAR <- function(X, Y, LS.estimator=LSE) {
    copy_y_names <- function(V){
        rownames(V) <- colnames(Y)
        V
    }
    Omega <- t(sapply(1:ncol(Y), function(k) LS.estimator(X, Y[,k], keep.resid=TRUE)))
    sigma <- cov(do.call(cbind, Omega[, "resid"]))
    rownames(sigma) <- colnames(sigma) <- colnames(Y)
    k_resid = which.max(colnames(Omega) == "resid")
    Omega <- Omega[, -c(k_resid)]
    VAR.Omega <- lapply(colnames(Omega), function(p) copy_y_names(do.call(rbind, Omega[,p])))
    names(VAR.Omega) <- colnames(Omega)
    VAR.Omega$sigma <- sigma
    VAR.Omega
}


if(FALSE){
    #XX=t(matrix(rep(1:10, each=3), nrow=3))
    #colnames(XX) <- paste0("C", 1:ncol(XX))
    #Z=expand_lag(XX, lags=5)

    t = (1:250)/20
    XX=cbind(t+rnorm(length(t)), t^2+rnorm(length(t)), t^3+t+rnorm(length(t)))
    colnames(XX) <- paste0("C", 1:ncol(XX))
    Z=expand_lag(XX, lags=2)
    summary(lm(C1.0 ~ C1.L1+C2.L1+C3.L1+C1.L2+C2.L2+C3.L2, data=as.data.frame(cbind(Z$Y, Z$X))))
    summary(lm(C2.0 ~ C1.L1+C2.L1+C3.L1+C1.L2+C2.L2+C3.L2, data=as.data.frame(cbind(Z$Y, Z$X))))
    summary(lm(C3.0 ~ C1.L1+C2.L1+C3.L1+C1.L2+C2.L2+C3.L2, data=as.data.frame(cbind(Z$Y, Z$X))))

    LASSLE(Z$X, Z$Y[,1])

    LSE(Z$X, Z$Y[,1])

    VAR(Z$X, Z$Y, LS.estimator=LSE)

    VAR(Z$X, Z$Y, LS.estimator=LASSLE)
    
}

