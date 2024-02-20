## load required packages
library(lemon) # for repeated axis in facet wraps
library(ggthemes)
library(gridExtra)
library(grid)
library(ggtext)
library(latex2exp)

## set default plotting options (such as font size) for this session
theme_set(theme_classic(base_size = 16, 
                        base_family = "serif"))

## helper function to define custom (plotting) theme
element_textbox_highlight <- function(..., 
                                      hi.labels = NULL,
                                      hi.fill = NULL,
                                      hi.col = NULL, 
                                      hi.box.col = NULL,
                                      hi.family = NULL) {
  structure(
    c(element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col, hi.family = hi.family)
    ),
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element")
  )
}

## helper function to define custom (plotting) theme
element_grob.element_textbox_highlight <- function(element, 
                                                   label = "",
                                                   ...) {
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill
    element$colour <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
    element$family <- element$hi.family %||% element$family
  }
  NextMethod()
}


## define a custom theme
mytheme <- theme(axis.line = element_line(),
                 panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                 axis.ticks.y.right = element_line(color = "black", linewidth = 2),
                 legend.text=element_text(size=14),
                 legend.position="bottom",
                 legend.title=element_text(size=14),
                 legend.key = element_rect(fill = "white",colour = "white"),
                 strip.background = element_blank(),
                 strip.text = element_textbox_highlight(
                   size = 12, face = "bold",
                   fill = "white", box.color = "darkslategray", color = "darkslategray",
                   halign = .5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
                   padding = margin(3, 0, 3, 0), margin = ggplot2::margin(0, 1, 3, 1),
                   hi.labels = c("multivariate Gaussian with AR1 covariance matrix", "equicorrelated covariance matrix (rho=0.8)", "sparse Gaussian Bayesian network", "|LASSO coefficient| difference", "RF importance score difference"),
                   hi.fill = "darkslategray", hi.box.col = "black", hi.col = "white"
                 ))


## helper function from https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
## To arrange multiple (sub)plots with a (partially) shared legend. 
## The procedure involves extracting the legend from one graph, creating a custom layout, and inserting the plots and legend in their corresponding cell.
grid_arrange_shared_legend <-  function(...,
                                        ncol = length(list(...)),
                                        nrow = 1,
                                        position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <-
    ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x)
    x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x)
    x + theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(
    position,
    "bottom" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)
    ),
    "right" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 2,
      widths = unit.c(unit(1, "npc") - lwidth, lwidth)
    )
  )
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}


