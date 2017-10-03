#' Calculate crude rate.
#'
#'Crude rate and confidence intervals based on Wilson method
#'
#' @param data input data
#' @param count count variable
#' @param population population variable
#' @param s scale
#' @param r round
#' @param alpha alpha
#'
#' @return crude_rate, lower_crude_rate,
#' and upper_crude_rate
#' @export
#'
#' @examples to be added
od_crude_rate <- function(data, count, population, s = 100000, r = 1, alpha = 0.05 ){

	# confidence intervals with Wilson method
	#simpler than the direct method

	count <- enquo(count)
	population <- enquo(population)

	suppressWarnings(suppressMessages(require(dplyr)))
	suppressWarnings(suppressMessages(require(tibble)))

	a <- data %>% pull(count)
	N <- data %>% pull(population)
	p <- a/N
	z <- -qnorm(alpha/2)
	moe <- ((z/sqrt(N))*sqrt(p*(1-p) + z^2/(4*N)))/(1+z^2/N)
	center <- (p+z^2/(2*N))/(1+z^2/N)
	lower_limit <- center-moe
	upper_limit <- center+moe

	data %>%
		add_column(crude_rate = round(p*s, r) ,
							 lower_crude_rate = round(lower_limit*s, r),
							 upper_crude_rate = round(upper_limit*s, r))
}
