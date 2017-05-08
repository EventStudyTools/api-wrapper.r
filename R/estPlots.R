#' @name arPlot
#' 
#' @title Abnormal Return Plot
#'
#' @description Abnormal return plots
#'
#' @param ResultParserObj An object of class \code{ResultParser}
#' @param firm set this parameter if just one firm should be plotted
#' @param window filter event time window
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param alpha alpha value
#' @param facetVar should each firm get its own plot (default = T)
#' @param ncol number of facet columns
#' @param addAAR add aar line
#' @param xVar x variable name
#' @param yVar y variable name
#' 
#' @return a ggplot2 object
#'
#' @export
arPlot <- function(ResultParserObj, firm = NULL, window = NULL, 
                   xlab = "", ylab = "Abnormal Returns", 
                   alpha = .5,
                   facetVar = NULL, ncol = 4,
                   addAAR = F,
                   xVar = "eventTime", yVar = "ar") {
  ar <- ResultParserObj$arResults
  if (!is.null(firm)) {
    ar %>% 
      dplyr::filter(Firm == firm) -> ar
  }
  
  if (is.null(window))
    window <- range(ar$eventTime)
  selectedWindow <- seq(from = window[1], to = window[2], by = 1)
  pal <- RColorBrewer::brewer.pal(3, "Blues")
  ar %>% 
    dplyr::filter(eventTime %in% selectedWindow) -> ar
  
  ar %>% 
    ggplot() +
    geom_hline(yintercept = 0, color = "black", alpha = .5) +
    geom_vline(xintercept = 0, color = "black", linetype = 2, alpha = .5) +
    geom_line(aes_string(x = xVar, y = yVar, group = "Firm"), 
              color = pal[3], alpha = alpha) + 
    scale_y_continuous(label = scales::percent) +
    xlab(xlab) +
    ylab(ylab) +
    theme_tq() -> q
  
  if (addAAR) {
    if (facetVar != "Firm") {
      setnames(ar, yVar, "y")
      ar %>% 
        dplyr::group_by_(.dots = c(xVar, facetVar)) %>% 
        dplyr::summarise(y = mean(y, na.rm = T)) -> mAr
      setnames(ar, "y", yVar)
      q <- q +
        geom_line(data = mAr, aes_string(x = xVar, y = "y"), color = "black")
    }
  }
  
  if (!is.null(facetVar)) {
    facetForm <- as.formula(paste0(" ~ ", facetVar))
    q <- q +
      facet_wrap(facetForm, ncol = ncol, scales = "free")
  }
  q
}


#' @name aarPlot
#' 
#' @title Averaged Abnormal Return Plot
#'
#' @description Averaged abnormal return plots
#' 
#' @param ResultParserObj An object of class \code{ResultParser}
#' @param cumSum plot CAAR
#' @param group set this parameter if just one group should be plotted
#' @param window numeric vector of length 2
#' @param ciStatistics Statistic used for confidence intervals
#' @param p p-value
#' @param ciType type of CI band 
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param facet should each firm get its own plot (default = T)
#' @param ncol number of facet columns
#' 
#' @return a ggplot2 object
#'
#' @export
aarPlot <- function(ResultParserObj, 
                    cumSum       = F,
                    group        = NULL, 
                    window       = NULL, 
                    ciStatistics = NULL, 
                    p            = .95, 
                    ciType       = "band",
                    xlab         = "", 
                    ylab         = "Averaged Abnormal Returns", 
                    facet        = T, 
                    ncol         = 4) {
  
  aar <- ResultParserObj$aarResults
  if (cumSum) {
    aar %>% 
      ResultParserObj$cumSum(var     = "aar", 
                             timeVar = "eventTime", 
                             cumVar  = "level") -> aar
  }
  
  if (!cumSum && !is.null(ciStatistics)) {
    ciInterval <- ResultParserObj$calcAARCI(statistic = ciStatistics, 
                                            p         = p)
    aar$lower <- ciInterval$lower
    aar$upper <- ciInterval$upper
  }
  
  if (!is.null(group)) {
    aar %>% 
      dplyr::filter(level == group) -> aar
  }
  
  if (is.null(window))
    window <- range(aar$eventTime)
  selectedWindow <- seq(from = window[1], to = window[2], by = 1)
  
  pal <- RColorBrewer::brewer.pal(3, "Blues")
  aar %>% 
    dplyr::filter(eventTime %in% selectedWindow) -> aar
  
  aar %>% 
    dplyr::mutate(aar = as.numeric(aar)) %>% 
    ggplot() +
    geom_hline(yintercept = 0, color = "black", alpha = .5) +
    geom_vline(xintercept = 0, color = "black", linetype = 2, alpha = .5) +
    geom_line(aes(x = eventTime, y = aar), color = pal[3]) + 
    scale_y_continuous(label = percent) +
    xlab(xlab) +
    ylab(ylab) +
    theme_tq() -> q
  
  # plot CI
  if (!cumSum && !is.null(ciStatistics)) {
    if (ciType == "band") {
      q <- q +
        geom_line(aes(x = eventTime, y = lower), linetype = 2, color = "gray50", alpha = .5) + 
        geom_line(aar, aes(x = eventTime, y = upper), linetype = 2, color = "gray50", alpha = .5)
    } else if (ciType == "ribbon") {
      q <- q +
        geom_ribbon(aes(x = eventTime, ymin = lower, ymax = upper), fill = "gray50", alpha = .25)
    }
  }
  
  # facet wrap
  if (facet) {
    q <- q +
      facet_wrap( ~ level, ncol = ncol, scales = "free")
  }
  q
}


#' @name pointwiseCARPlot
#' 
#' @title Pointwise Cumulative Abnormal Return Plot
#' 
#' @description Pointwise cumulative abnormal return plots
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
      dplyr::filter(Firm == firm) -> df
  }
  
  # calculate cumulative sum
  df <- data.table::as.data.table(df)
  data.table::setkeyv(df, c("Firm", "eventTime"))
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
