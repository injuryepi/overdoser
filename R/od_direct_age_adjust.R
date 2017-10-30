#' Title
#'
#' @param data input data
#' @param agegrp age group variable
#' @param count count variable
#' @param population population variable
#' @param standard_pop standar population table or NULL
#' @param s scale numeric
#' @param r round numeric
#' @param alpha alpha numeric
#'
#' @return age_adj_rate, lower_age_adj
#' and upper_age_adj
#' @export
#'
#' @examples to be added
od_direct_age_adjust <- function(data, agegrp, count, population, 
    standard_pop = NULL, s = 1e+05, r = 1, alpha = 0.05) {
    agegrp <- enquo(agegrp)
    count <- enquo(count)
    population <- enquo(population)
    
    suppressWarnings(suppressMessages(require(dplyr)))
    suppressWarnings(suppressMessages(require(tibble)))
    
    us2000std = c(0.013818, 0.055317, 0.145565, 0.138646, 0.135573, 0.162613, 
        0.134834, 0.087247, 0.066037, 0.044842, 0.015508)
    std_pop <- if (is.null(standard_pop)) {
        us2000std
    } else {
        standard_pop
    }
    # calculate adjusted rate and confidence interval with gamma distribution
    # modified from the R package
    # https://cran.r-project.org/web/packages/epitools/index.html with more
    # method details from
    # https://pdfs.semanticscholar.org/584d/0d020d77e84d193f42e162c59c64795dac6c.pdf
    
    
    data <- data %>% arrange(as.numeric(!!agegrp))
    rate <- data %>% mutate(rate = (!!count)/(!!population)) %>% pull(rate)
    stdwt <- std_pop/sum(std_pop)
    dsr <- sum(stdwt * rate)
    var_k <- data %>% mutate(var_k = !!count/(!!population)^2) %>% pull(var_k)
    dsr_var <- sum((stdwt^2) * var_k)
    pop <- data %>% pull(!!population)
    wm <- max(stdwt/pop)
    gamma_lci <- qgamma(alpha/2, shape = (dsr^2)/dsr_var, scale = dsr_var/dsr)
    gamma_uci <- qgamma(1 - alpha/2, shape = ((dsr + wm)^2)/(dsr_var + wm^2), 
        scale = (dsr_var + wm^2)/(dsr + wm))
    
    data0 <- data %>% summarise_at(vars(!!count, !!population), sum)
    
    
    data0 %>% add_column(age_adj_rate = round(dsr * s, r), lower_age_adj = round(gamma_lci * 
        s, r), upper_age_adj = round(gamma_uci * s, r))
    
}
