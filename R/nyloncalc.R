library(ggplot2)
library(grid)

theme_nyloncalc <- function(base_size = 12,base_family="") {
  theme(
        line =                  element_line(colour = "black", size = 0.5, linetype = 1,lineend = "butt"),
        rect =                  element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
        text =                  element_text(family = base_family, face = "plain",colour = "black", size = base_size,hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
        axis.text =             element_text(size = rel(0.8), colour = "grey50"),
        strip.text =            element_text(size = rel(0.8)),

        axis.line =             element_blank(),
        axis.text.x =           element_text(size=10,colour="#535353",face="bold",family="Daniel"),
        axis.text.y =           element_text(size=10,colour="#535353",face="bold",family="Daniel"),
        axis.ticks =            element_blank(),
        axis.title.x =          element_text(size=12,colour="#535353",face="bold",family="Gulim",vjust=-.2),
        axis.title.y =          element_text(size=12,colour="#535353",face="bold",family="Gulim",angle=90,vjust=0.8),
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

get_params <- function(x,y,legend,constrain_y_min=NULL,constrain_y_max=NULL,constrain_x_min=NULL,constrain_x_max=NULL) {

        if (is.null(constrain_y_min)==FALSE) {
            min.y =         constrain_y_min
        }
        if (is.null(constrain_y_min)==TRUE) {
            min.y =         floor(y[which.min(y)])
        }

        if (is.null(constrain_y_max)==FALSE) {
            max.y =         constrain_y_max
        }
        if (is.null(constrain_y_min)==TRUE) {
            max.y =         ceiling(y[which.max(y)])
        }


        if (is.null(constrain_x_min)==FALSE) {
            min.x =         constrain_x_min
        }
        if (is.null(constrain_x_min)==TRUE) {
            min.x =         floor(x[which.min(x)])
        }


        if (is.null(constrain_x_max)==FALSE) {
            max.x =         constrain_x_max
        }
        if (is.null(constrain_x_min)==TRUE) {
            max.x =         ceiling(x[which.max(x)])
        }

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

        f_plotminx=floor(plot_min_x)
        f_plotminy=floor(plot_min_y)
        c_plotmaxx=ceiling(plot_max_x)
        c_plotmaxy=ceiling(plot_max_y)

        tfamily="Chalk Line Outline"
        tlabel="Nylon Calculus"

        params <- list(tfamily=tfamily,tlabel=tlabel,f_plotminx=f_plotminx,f_plotminy=f_plotminy,c_plotmaxx=c_plotmaxx,c_plotmaxy=c_plotmaxy,min.x=min.x,max.x=max.x,min.y=min.y,max.y=max.y,domain=domain,range=range,plotminy=plot_min_y,plotmaxy=plot_max_y,plotminx=plot_min_x,plotmaxx=plot_max_x,stepsizex=step_size_x,stepsizey=step_size_y,rect_min_x=rect_min_x,rect_max_y=rect_max_y,text_x=text_x,text_y=text_y)

        return(params)
}

# simple line
attach(data)
params <- get_params(year,threes,0)
attach(params)
dev.new(width=8,height=6)

ggplot(data,aes(x=year,y=threes,colour='red')) +
        theme_nyloncalc() +
        ggtitle("Increasing Reliance on the 3-pointer") +
        ylab("% of Shots That are 3s") +
        xlab("Year") +
        geom_line(size=1.2,alpha=.8,colour="cyan") +
        scale_x_continuous(breaks=seq(f_plotminx,c_plotmaxx,stepsizex),limits=c(plotminx,plotmaxx)) +
        scale_y_continuous(breaks=seq(f_plotminy,c_plotmaxy,stepsizey),limits=c(plotminy,plotmaxy)) +
        annotate("rect",xmin=rect_min_x,xmax=plot_max_x,ymin=plotminy,ymax=rect_max_y,alpha=.8) +
        annotate("text",x=text_x,y=text_y,label=tlabel,colour="white",family=tfamily,size=4.3,vjust=.4,hjust=.5)
ggsave(filename="/Users/austinc/Desktop/nyloncalc1.png")


# grouped line
attach(data)
params <- get_params(Year,PER,1)
attach(params)
dev.new(width=8,height=6)

ggplot(data,aes(x=Year,y=PER,group=Player,colour=Player)) +
        theme_nyloncalc() +
        ggtitle("Player PER Over Career") +
        ylab("Player Efficiency Rating") +
        xlab("Years Since Draft") +
        scale_colour_brewer(palette="Dark2") +
        geom_line(size=1.2,alpha=.8) +
        scale_x_continuous(breaks=seq(f_plotminx,c_plotmaxx,stepsizex),limits=c(plotminx,plotmaxx)) +
        scale_y_continuous(breaks=seq(f_plotminy,c_plotmaxy,stepsizey),limits=c(plotminy,plotmaxy)) +
        annotate("rect",xmin=rect_min_x,xmax=plot_max_x,ymin=plotminy,ymax=rect_max_y,alpha=.8) +
        annotate("text",x=text_x,y=text_y,label=tlabel,colour="white",family=tfamily,size=4.3,vjust=.4,hjust=.5)
ggsave(filename="/Users/austinc/Desktop/nyloncalc2.png")



attach(data)
params <- get_params(Year,PER,0)
dev.new(width=8,height=6)

ggplot(data,aes(x=Year,y=PER,group=Player,colour=Player)) +
        theme_nyloncalc() +
        ggtitle("Player PER Over Career") +
        ylab("Player Efficiency Rating") +
        xlab("Years Since Draft") +
        scale_colour_brewer(palette="Dark2") +
        theme(legend.position="none") +
        geom_line(size=1.2,alpha=.8) +
        scale_x_continuous(breaks=seq(floor(params$plot_min_x),ceiling(params$plot_max_x),params$step_size_x),limits=c(params$plot_min_x,params$plot_max_x)) +
        scale_y_continuous(breaks=seq(floor(params$plot_min_y),ceiling(params$plot_max_y),params$step_size_y),limits=c(params$plot_min_y,params$plot_max_y)) +
        annotate("rect",xmin=params$rect_min_x,xmax=params$plot_max_x,ymin=params$plot_min_y,ymax=params$rect_max_y,alpha=.8) +
        annotate("text",x=params$text_x,y=params$text_y,label="Nylon Calculus",colour="#ffffff",family="Chalk Line Outline",size=4.3,vjust=.4,hjust=.5)
ggsave(filename="/Users/austinc/Desktop/nyloncalc_1.png")




attach(data2)
params <- get_params(mid,drtg,0)
dev.new(width=8,height=6)

ggplot(data2,aes(x=mid,y=drtg,group=mavs,label=Over)) +
        theme_nyloncalc() +
        ggtitle("Teams That Force Midrange Shots Excel Defensively") +
        ylab("Defensive Rating") +
        xlab("% of Opponent Shots Taken from Midrange") +
        geom_point(shape=1,alpha=.8) +
        geom_smooth() +
        geom_text(size=2.5,family="Gulim",vjust=data2$vjust+.3,hjust=data2$hjust+.5) +
        scale_x_continuous(breaks=seq(floor(params$plot_min_x),ceiling(params$plot_max_x),params$step_size_x),limits=c(params$plot_min_x,params$plot_max_x)) +
        scale_y_continuous(breaks=seq(floor(params$plot_min_y),ceiling(params$plot_max_y),params$step_size_y),limits=c(params$plot_min_y,params$plot_max_y)) +
        annotate("rect",xmin=params$rect_min_x,xmax=params$plot_max_x,ymin=params$plot_min_y,ymax=params$rect_max_y,alpha=.8) +
        annotate("text",x=params$text_x,y=params$text_y,label="Nylon Calculus",colour="#ffffff",family="Chalk Line Outline",size=4.3,vjust=.4,hjust=.5)
ggsave(filename="/Users/austinc/Desktop/nyloncalc2.png")





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

attach(data3)
params <- get_params_hist(shot_distance,0)
dev.new(width=8,height=6)

ggplot(data3,aes(x=shot_distance)) +
        theme_nyloncalc() +
        ggtitle("The Distribution of NBA Shots") +
        ylab("Density") +
        xlab("Distance From Basket (feet)") +
        geom_density(alpha=.4,fill="#1b9e77") +
        scale_x_continuous(breaks=seq(params$plot_min_x,params$plot_max_x,params$step_size_x),limits=c(params$plot_min_x,params$plot_max_x)) +
        annotate("rect",xmin=params$rect_min_x,xmax=params$plot_max_x,ymin=params$plot_min_y,ymax=params$rect_max_y,alpha=.8) +
        annotate("text",x=params$text_x,y=params$text_y,label="Nylon Calculus",colour="#ffffff",family="Chalk Line Outline",size=4.3,vjust=.4,hjust=.5)
ggsave(filename="/Users/austinc/Desktop/nyloncalc3_1.png")

attach(data3)
params <- get_params_hist(shot_distance,38)
dev.new(width=8,height=6)

ggplot(data3,aes(x=shot_distance)) +
        theme_nyloncalc() +
        ggtitle("The Distribution of NBA Shots") +
        ylab("Density") +
        xlab("Distance From Basket (feet)") +
        geom_density(alpha=.4,fill="#1b9e77") +
        scale_x_continuous(breaks=seq(params$plot_min_x,params$plot_max_x,params$step_size_x),limits=c(params$plot_min_x,params$plot_max_x)) +
        annotate("rect",xmin=params$rect_min_x,xmax=params$plot_max_x,ymin=params$plot_min_y,ymax=params$rect_max_y,alpha=.8) +
        annotate("text",x=params$text_x,y=params$text_y,label="Nylon Calculus",colour="#ffffff",family="Chalk Line Outline",size=4.3,vjust=.4,hjust=.5)
ggsave(filename="/Users/austinc/Desktop/nyloncalc3_2.png")




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

attach(data4)
params <- get_params_bar(Player,TS.,1)
dev.new(width=8,height=6)

ggplot(data4,aes(x=Player,y=TS.,fill=Condition)) +
        theme_nyloncalc() +
        ggtitle("Aldridge's Effect on Teammates") +
        ylab("True Shooting %") +
        geom_bar(stat="identity",alpha=.8, position="dodge") +
        scale_y_continuous(breaks=seq(params$plot_min_y,params$plot_max_y,params$step_size_y),limits=c(params$plot_min_y,params$plot_max_y)) +
        annotate("rect",xmin=params$rect_min_x,xmax=params$plot_max_x,ymin=0,ymax=params$rect_max_y,alpha=.8) +
        annotate("text",x=params$text_x,y=params$text_y,label="Nylon Calculus",colour="#ffffff",family="Chalk Line Outline",size=4.3,vjust=.4,hjust=.5)

ggsave(filename="/Users/austinc/Desktop/nyloncalc4.png")



attach(data5)
params <- get_params(Height,X3PTA.FGA,1)
dev.new(width=8,height=6)

ggplot(data5,aes(x=Height,y=X3PTA.FGA,group=group,colour=group,shape=a)) +
        theme_nyloncalc() +
        ggtitle("Visualizing the Rise of the Stretch 4") +
        ylab("3PTA/FGA") +
        xlab("Height in Inches") +
        geom_point(shape=1,alpha=.8,size=3,shape=a) +
        scale_x_continuous(breaks=seq(floor(params$plot_min_x),ceiling(params$plot_max_x),params$step_size_x),limits=c(params$plot_min_x,params$plot_max_x)) +
        scale_y_continuous(breaks=seq(floor(params$plot_min_y),ceiling(params$plot_max_y),params$step_size_y),limits=c(params$plot_min_y,params$plot_max_y)) +
        annotate("rect",xmin=params$rect_min_x,xmax=params$plot_max_x,ymin=params$plot_min_y,ymax=params$rect_max_y,alpha=.8) +
        annotate("text",x=params$text_x,y=params$text_y,label="Nylon Calculus",colour="#ffffff",family="Chalk Line Outline",size=4.3,vjust=.4,hjust=.5)
ggsave(filename="/Users/austinc/Desktop/nyloncalc5.png")


attach(data)
dev.new(width=8,height=6)
params <- get_params(year,threes,0)

ggplot(data,aes(x=year,y=threes,colour='red')) +
        theme_nyloncalc() +
        ggtitle("Increasing Reliance on the 3-pointer") +
        ylab("% of Shots That are 3s") +
        xlab("Year") +
        geom_line(size=1.2,alpha=.8,colour="cyan") +
        scale_x_continuous(breaks=seq(floor(params$plot_min_x),ceiling(params$plot_max_x),params$step_size_x),limits=c(params$plot_min_x,params$plot_max_x)) +
        scale_y_continuous(breaks=seq(floor(params$plot_min_y),ceiling(params$plot_max_y),params$step_size_y),limits=c(params$plot_min_y,params$plot_max_y)) +
        annotate("rect",xmin=params$rect_min_x,xmax=params$plot_max_x,ymin=params$plot_min_y,ymax=params$rect_max_y,alpha=.8) +
        annotate("text",x=params$text_x,y=params$text_y,label="Nylon Calculus",colour="#ffffff",family="Chalk Line Outline",size=4.3,vjust=.4,hjust=.5)
ggsave(filename="/Users/austinc/Desktop/nyloncalc6_1.png")



