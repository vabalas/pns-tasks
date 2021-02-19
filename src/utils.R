#' Kernel density estimates
#'
#' @param observations a vector containing the sample data
#' @param kernel kernel method's nameg
#'
#' Plots a kernel density plot using sensible defaults.
#'
kernel_density_estimate <- function(observations, kernel = "epanechnikov", ...) {
    kernels <- eval(formals(density.default)$kernel)
    stopifnot(kernel %in% kernels)

    ## Sensible automatic choice
    bandwidth <- bw.SJ(observations)

    ## Bandwidth Adjustment for "Exactly Equivalent Kernels"
    adjustment <- sapply(kernels, function(k) {
        density(kernel = k, give.Rkern = TRUE)
    })
    adjustment <- (adjustment["gaussian"] / adjustment)^ 0.2

    density_estimate <- density(
        observations,
        bw = bandwidth,
        kernel = kernel,
        adjust = adjustment[kernel]
    )

    return(density_estimate)
}

alpha <- function(col, alpha) {
    h <- 256
    x <- col2rgb(col)
    return(rgb(x[1] / h, x[2] / h, x[3] / h, alpha = alpha))
}

list(
    kernel_density_estimate = kernel_density_estimate,
    alpha = alpha
)
