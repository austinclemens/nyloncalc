#' Theme Function
#'
#' This sets all the ggplot parameters for theme_nyloncalc
#' @keywords ggplot2 theme
#' @export
#' @examples
#' theme_nyloncalc()

theme_nyloncalc <- function(base_size = 12,base_family="") {
  theme(
        line =                  element_line(colour = "#fbfbfb", size = 0.5, linetype = 1,lineend = "butt"),
        rect =                  element_rect(fill = "white", colour = "#fbfbfb", size = 0.5, linetype = 1),
        text =                  element_text(family = base_family, face = "plain",colour = "black", size = base_size,hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
        axis.text =             element_text(size = rel(0.8), colour = "grey50"),
        strip.text =            element_text(size = rel(0.8)),

        axis.line =             element_blank(),
        axis.text.x =           element_text(size=10,colour="#535353",face="bold",family="Daniel"),
        axis.text.y =           element_text(size=10,colour="#535353",face="bold",family="Daniel"),
        axis.ticks =            element_blank(),
        axis.title.x =          element_text(size=12,colour="#535353",face="bold",family="Gulim",vjust=0,lineheight=3),
        axis.title.y =          element_text(size=12,colour="#535353",face="bold",family="Gulim",angle=90,vjust=0,lineheight=3),
        axis.ticks.length =     unit(0.15, "cm"),
        axis.ticks.margin =     unit(0.1, "cm"),

        legend.background =     element_rect(fill="#Fbfbfb", size=0), 
        legend.margin =         unit(-.2, "cm"),
        legend.key =            element_rect(fill = 'transparent', size = 0),
        legend.key.size =       unit(1, "lines"),
        legend.key.height =     NULL,
        legend.key.width =      NULL,
        legend.text =           element_text(size = base_size * 0.8,family="Gulim"),
        legend.text.align =     NULL,
        legend.title =          element_blank(),
        legend.position =       "top",
        legend.direction =      NULL,
        legend.justification =  "center",
        legend.box =            NULL,
 
        panel.background =      element_rect(fill="#Fbfbfb",size=0), 
        panel.border =          element_blank(),
        panel.grid.major =      element_line(colour="#63d1f4",size=.15,linetype="dashed"),
        panel.grid.minor =      element_blank(),
        panel.margin =          unit(0.25, "lines"),
 
        strip.background =      element_rect(fill = NA, colour = NA), 
        strip.text.x =          element_text(colour = "black", size = base_size * 0.8),
        strip.text.y =          element_text(colour = "black", size = base_size * 0.8),
 
        plot.background =       element_rect(fill="#Fbfbfb",size=0),
        plot.title =            element_text(face="bold",colour="#3C3C3C",size=20,family="Gulim"),
        plot.margin =           unit(c(.5, .5, .5, .7), "cm"),

        complete = TRUE
        )
}

#' Scatter/line parameters
#'
#' This sets all parameters for scatter/line plots
#' @param x Vector of x values for graph
#' @param y Vector of y values for graph
#' @param legend Does the graph need a legend? Defaults to 0, replace with 1.
#' @export
#' @examples
#' get_params()


get_params <- function(x,y,legend=0) {
        min.x =         floor(x[which.min(x)])
        max.x =         ceiling(x[which.max(x)])
        min.y =         floor(y[which.min(y)])
        max.y =         ceiling(y[which.max(y)])
        domain =        max.x-min.x
        range =         max.y-min.y

        if (range<=5) {
            step_size_y =   round(range/10,1)
        }

        if (domain<=5) {
            step_size_x =   round(domain/10,1)
        }

        if (range>5) {
            step_size_y =   ceiling(range/10)
        }

        if (domain>5) {
            step_size_x =   ceiling(domain/10)
        }

        plot_min_y =    min.y-(range*.06)
        plot_max_y =    max.y+(range*.06)
        plot_min_x =    min.x
        plot_max_x =    max.x+(domain*.06)
        plot_domain =   plot_max_x-plot_min_x
        plot_range =    plot_max_y-plot_min_y

        rect_min_x =    -(domain*.29)+max.x

        if (legend==1) {
                rect_max_y =    min.y+(plot_range*.013)
        }

        if (legend==0) {
                rect_max_y =    min.y+(plot_range*.006)
        }

        text_x =        (rect_min_x+plot_max_x)/2
        text_y =        (rect_max_y+plot_min_y)/2

        params <- list(min.x=min.x,max.x=max.x,min.y=min.y,max.y=max.y,domain=domain,range=range,plot_min_y=plot_min_y,plot_max_y=plot_max_y,plot_min_x=plot_min_x,plot_max_x=plot_max_x,step_size_x=step_size_x,step_size_y=step_size_y,rect_min_x=rect_min_x,rect_max_y=rect_max_y,text_x=text_x,text_y=text_y)

        return(params)
}

#' Histogram parameters
#'
#' This sets all parameters for histograms
#' @param x Vector of x values for graph
#' @param limit_x highest value for x, defaults to 0, which lets ggplot pick
#' @export
#' @examples
#' get_params_hist()


get_params_hist <- function(x,limit_x) {
        d <- density(x)
        if (limit_x==0) {
                min.x =         x[which.min(x)]
                max.x =         ceiling(d$x[which.max(d$x)])
        }

        if (limit_x!=0) {
                min.x=          x[which.min(x)]
                max.x =         limit_x
        }

        min.y =         round(d$y[which.min(d$y)],3)
        max.y =         round(d$y[which.max(d$y)],3)
        domain =        max.x-min.x
        range =         max.y-min.y

        plot_min_y =    min.y-(range*.06)
        plot_max_y =    max.y+(range*.06)
        plot_min_x =    min.x
        plot_max_x =    max.x+(domain*.06)
        plot_domain =   plot_max_x-plot_min_x
        plot_range =    plot_max_y-plot_min_y

        step_size_y =   round(range/10,3)
        step_size_x =   ceiling(domain/10)

        rect_min_x =    -(plot_domain*.29)+max.x
        rect_max_y =    min.y+(plot_range*.006)

        text_x =        (rect_min_x+plot_max_x)/2
        text_y =        (rect_max_y+plot_min_y)/2

        params <- list(min.x=min.x,max.x=max.x,min.y=min.y,max.y=max.y,domain=domain,range=range,plot_min_y=plot_min_y,plot_max_y=plot_max_y,plot_min_x=plot_min_x,plot_max_x=plot_max_x,step_size_x=step_size_x,step_size_y=step_size_y,rect_min_x=rect_min_x,rect_max_y=rect_max_y,text_x=text_x,text_y=text_y)

        return(params)
}

#' Scatter/line parameters
#'
#' This sets all parameters for bar charts
#' @param x Vector of x values for graph
#' @param y Vector of y values for graph
#' @param legend Does the graph need a legend? Defaults to 0, replace with 1.
#' @export
#' @examples
#' get_params_bar()


get_params_bar <- function(x,y,legend) {
        f<-nlevels(x)

        min.y =         0
        max.y =         ceiling(y[which.max(y)])
        min.x =         0
        max.x =         4
        range =         max.y-min.y
        domain =        max.x-min.x

        plot_min_y =    0
        plot_max_y =    max.y+(range*.06)
        plot_min_x =    min.x
        plot_max_x =    max.x+(domain*.06)
        plot_domain =   plot_max_x-plot_min_x
        plot_range =    plot_max_y-plot_min_y

        step_size_y =   round(range/10,3)
        step_size_x =   0

        rect_min_x =    -(domain*.25)+f

        if (legend==1) {
                rect_max_y =    (plot_range*.08)
        }

        if (legend==0) {
                rect_max_y =    (plot_range*.1)
        }

        text_x =        (rect_min_x+plot_max_x)/2
        text_y =        (rect_max_y+plot_min_y)/2

        params <- list(min.x=min.x,max.x=max.x,min.y=min.y,max.y=max.y,domain=domain,range=range,plot_min_y=plot_min_y,plot_max_y=plot_max_y,plot_min_x=plot_min_x,plot_max_x=plot_max_x,step_size_x=step_size_x,step_size_y=step_size_y,rect_min_x=rect_min_x,rect_max_y=rect_max_y,text_x=text_x,text_y=text_y)

        return(params)
}
