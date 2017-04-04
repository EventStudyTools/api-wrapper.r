#' @name arPlot
#' 
#' @title Abnormal Return Plot
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
arPlot <- function(df, firm = NULL, window = NULL, xlab = "", ylab = "Abnormal Returns", facet = T, ncol = 4, xVar = "eventTime", yVar = "ar") {
  
  if (!is.null(firm)) {
    df %>% 
      dplyr::filter(Firm == firm) -> df
  }
  
  if (is.null(windows))
    window <- range(df$eventTime)
  selectedWindow <- seq(from = window[1], to = window[2], by = 1)
  
  df %>% 
    dplyr::filter(eventTime %in% selectedWindow) -> df
  
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
    facet_wrap( ~ Firm, ncol = ncol, scales = "free_x")
  
  p
}


#' @name hcArPlot
#' 
#' @title Highchart version of the Abnormal Return Plot
#' 
#' @param df data.frame with abnormal return in long format; 
#' @param firm set this parameter if just one firm should be plotted
#' @param window 
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param facet should each firm get its own plot (default = T)
#' @param ncol number of facet columns
#' @param xVar x variable name
#' @param yVar y variable name
#' 
#' @return a highchart object
#' 
#' @export
hcArPlot <- function(df, firm = NULL, window = NULL, xlab = "", ylab = "Abnormal Returns", xVar = "eventTime", yVar = "ar") {
  
  if (!is.null(firm)) {
    df %>% 
      dplyr::filter(Firm == firm) -> df
  }
  
  hc <- highchart(type = "chart")
  nCols <- dplyr::n_distinct(df$Firm)
  mCols <- min(nCols, 9)
  pal <- RColorBrewer::brewer.pal(mCols, "Blues")
  
  if (nCols > mCols)
    pal <- grDevices::colorRampPalette(pal)(nCols)
  
  if (is.null(windows))
    window <- range(df$eventTime)
  selectedWindow <- seq(from = window[1], to = window[2], by = 1)
  
  df %>% 
    dplyr::filter(eventTime %in% selectedWindow) -> df
  
  for (i in 1:nCols) {
    firm <- df$Firm[i]
    df %>% 
      dplyr::filter(Firm  == firm ) %>% 
      dplyr::mutate(ar = 100 * ar) %>% 
      dplyr::rename(x = eventTime, y = ar) -> tmp
    hc %>% 
      hc_add_series(tmp %>% dplyr::select(x, y), 
                    type        = "area", 
                    fillOpacity = .15, 
                    lineWidth   = 1, 
                    color       = pal[i],
                    marker      = list(enabled = F),
                    name        = firm) -> hc
  }
  hc %>%   
    hc_tooltip(headerFormat  = '<b><span style="font-size: 12px">Event Day: {point.x}</span></b><br>',
               pointFormat   = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y}</b><br/>',
               sort          = F,
               valueDecimals = 2,
               valueSuffix   = "%",
               table         = T) %>% 
    hc_yAxis(title = list(text = ""),
             labels = list(
               format = "{value}%"
             )) %>% 
    hc_xAxis(title = list(text = ""),
             plotLines = list(
               list(
                 label = list(text          = "Event Day",
                              style         = list(color = "gray"),
                              rotation      = 0,
                              verticalAlign = "top",
                              y             = 10),
                 dashStyle = "Dash",
                 color     = "gray",
                 width     = 1,
                 value     = 0
               )
             )) %>% 
    hc_legend(align         = "right",
              title         = list(text = "Firms"),
              verticalAlign = "top",
              layout        = "vertical")
}


#' @name aarPlot
#' 
#' @title Averaged Abnormal Return Plot
#'
#' @param ResultParser An object of class \code{ResultParser}
#' @param group set this parameter if just one group should be plotted
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param facet should each firm get its own plot (default = T)
#' @param ncol number of facet columns
#' 
#' @return a ggplot2 object
#'
#' @export
aarPlot <- function(ResultParserObj, 
                    group = NULL, 
                    window = NULL, 
                    ciStatistics = NULL, 
                    p = .95, 
                    ciType = "band",
                    xlab = "", 
                    ylab = "Averaged Abnormal Returns", 
                    facet = T, 
                    ncol = 4) {
  
  aar <- ResultParserObj$aarResults
  
  if (!is.null(ciStatistics)) {
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
    geom_hline(yintercept = 0, color = "gray50", alpha = .5) +
    geom_vline(xintercept = 0, color = "gray50", linetype = 2, alpha = .5) +
    geom_line(aes(x = eventTime, y = aar), color = pal[3]) + 
    scale_y_continuous(label = percent) +
    xlab("") +
    ylab("Abnormal Return") +
    theme_tq() -> q
  
  # plot CI
  if (!is.null(ciStatistics)) {
    if (ciType == "band") {
      q <- q +
        geom_line(data = aar, aes(x = eventTime, y = lower), linetype = 2, color = "gray50", alpha = .5) + 
        geom_line(data = aar, aes(x = eventTime, y = upper), linetype = 2, color = "gray50", alpha = .5)
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


#' @name hcAarPlot
#' 
#' @title Highchart version of Averaged Abnormale Return Plot
#'
#' @param df data.frame with abnormal return in long format; 
#' @param group set this parameter if just one group should be plotted
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param facet should each firm get its own plot (default = T)
#' @param ncol number of facet columns
#' 
#' @return a highchart object
#'
#' @export
hcAarPlot <- function(df, group = NULL, window = NULL, xlab = "", ylab = "Averaged Abnormal Returns") {
  
  if (!is.null(group)) {
    df %>% 
      dplyr::filter(level == group) -> df
  }
  
  if (is.null(windows))
    window <- range(df$eventTime)
  selectedWindow <- seq(from = window[1], to = window[2], by = 1)
  
  df %>% 
    dplyr::filter(eventTime %in% selectedWindow) -> df
  
}


#' @name pointwiseCARPlot
#' 
#' @title Pointwise Cumulative Abnormal Return Plot
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


#' @name hcPointwiseCARPlot
#' 
#' @title Highchart version of Pointwise Cumulative Abnormal Return Plot
#' 
#' @param df data.frame with abnormal return in long format; 
#' @param firm set this parameter if just one firm should be plotted
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param facet should each firm get its own plot (default = T)
#' @param ncol number of facet columns
#' 
#' @return a highchart object
#' 
#' @export
hcPointwiseCARPlot <- function(df, firm = NULL, xlab = "", ylab = "pointwise Cumulative Abnormal Returns", facet = T, ncol = 4) {
  
  if (!is.null(firm)) {
    df %>% 
      dplyr::filter(Firm == firm) -> df
  }
  
  # calculate cumulative sum
  df <- as.data.table(df)
  setkeyv(df, c("Firm", "eventTime"))
  df[, car := cumsum(ar), by = Firm]
  
  # plot pCAR
  df %>% 
    hcArPlot(xlab  = xlab, 
             ylab  = ylab, 
             facet = facet, 
             ncol  = ncol, 
             xVar  = "eventTime", 
             yVar  = "car")
}
