#' Abnormal Return Plot
#'
#' @param df data.frame with abnormal return in long format; 
#' @param firm set this parameter if just one firm should be plotted
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param facet should each firm get its own plot (default = T)
#' @param ncol number of facet columns
#' @param xVar x variable name
#' @param yVar y variable name
#' 
#' @return a ggplot2 object
#'
#' @export
arPlot <- function(df, firm = NULL, xlab = "", ylab = "Abnormal Returns", facet = T, ncol = 4, xVar = "eventTime", yVar = "ar") {
  
  if (!is.null(firm)) {
    df %>% 
      dplyr::filter(Firm == group) -> df
  }
  
  df %>% 
    ggplot() +
    geom_hline(yintercept = 0, color = "gray50", alpha = .5) +
    geom_vline(xintercept = 0, color = "gray50", linetype = 2, alpha = .5) +
    geom_line(aes_string(x = xVar, y = yVar), color = pal[3]) + 
    scale_y_continuous(label = percent) +
    xlab(xlab) +
    ylab(ylab) +
    theme_tq() -> p
  
  if (facet)
    p <- p +
      facet_wrap( ~ Firm, ncol = ncol)
  
  p
}


#' Averaged Abnormal Return Plot
#'
#' @param df data.frame with abnormal return in long format; 
#' @param group set this parameter if just one group should be plotted
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param facet should each firm get its own plot (default = T)
#' @param ncol number of facet columns
#' 
#' @return a ggplot2 object
#'
#' @export
aarPlot <- function(df, group = NULL, xlab = "", ylab = "Averaged Abnormal Returns", facet = T, ncol = 4) {
  
  if (!is.null(group)) {
    df %>% 
      dplyr::filter(level == group) -> df
  }
  
  df %>% 
    dplyr::mutate(aar = as.numeric(aar)) %>% 
    ggplot() +
    geom_hline(yintercept = 0, color = "gray50", alpha = .5) +
    geom_vline(xintercept = 0, color = "gray50", linetype = 2, alpha = .5) +
    geom_line(aes(x = eventTime, y = aar), color = pal[3]) + 
    facet_wrap( ~ level, ncol = 4) +
    scale_y_continuous(label = percent) +
    xlab("") +
    ylab("Abnormal Return") +
    theme_tq()
  
  if (facet)
    p <- p +
    facet_wrap( ~ Firm, ncol = ncol)
  
  p
}


#' Pointwise Cumulative Abnormal Return Plot
#' 
#' @param df data.frame with abnormal return in long format; 
#' @param firm set this parameter if just one firm should be plotted
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param facet should each firm get its own plot (default = T)
#' @param ncol number of facet columns
#' 
#' @return a ggplot2 object
#' 
#' @export 
pointwiseCARPlot <- function(df, firm = NULL, xlab = "", ylab = "pointwise Cumulative Abnormal Returns", facet = T, ncol = 4) {
  
  if (!is.null(firm)) {
    df %>% 
      dplyr::filter(Firm == group) -> df
  }
  
  # calculate cumulative sum
  df <- as.data.table(df)
  setkeyv(df, c("Firm", "eventTime"))
  df[, car := cumsum(ar), by = Firm]
  
  # plot pCAR
  df %>% 
    arPlot(xlab  = xlab, 
           ylab  = ylab, 
           facet = facet, 
           ncol  = ncol, 
           xVar  = "eventTime", 
           yVar  = "car")
}

