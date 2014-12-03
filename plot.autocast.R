plot.autocast<- function(out, title=NULL,
                         subtitle=NULL,print="device",print.args =
                         list(), uncertainty=F){
y <- out$y
yhat <- out$yhat
bounds <- out$bounds
#cone <- do.call(abind, list(cone, along=3))
sample.frame <- out$aux$args.yourcast$sample.frame
validation.years <- as.numeric(out$aux$holdout.years)
print.options <- print.args
filename <- print.options$filename
width <- 10
height <- 5


plot.list <- list(ageplot(y, yhat, bounds, sample.frame,
                          validation.years, uncertainty), timeplot(y,
                                                                   yhat, bounds, sample.frame, validation.years, uncertainty))
grid.args <- c(plot.list,list(nrow=1,ncol=length(plot.list),main=title,sub=subtitle))

if(print=="device"){dev.new(height=5,width=width)}
if(print=="pdf"){pdf(file=filename,height=5,width=width)}
do.call(grid.arrange,grid.args)
if(print=="device"){}
if(print=="pdf"){dev.off()}
}

### Age plot: time on x axis, dep var on y axis, groupings are ages
ageplot <- function(y,yhat,bounds, sample.frame, validation.years, uncertainty){
	xlab <- "Time"
        ylab <- "Data and Forecasts"
	
	#times <- unique(as.integer(yhat$time)) 
	#ages <- unique(as.integer(yhat$age))
	#time.min <- min(times)
	#time.max <- max(times)
	#age.min <- min(ages)
	#age.max <- max(ages)
	#sample.years <- unique(as.integer(names(which(y$years %in% validation.years==FALSE))))
	#forecast.years <- times
	
	y.obs <- y[y$time %in% validation.years==FALSE,]
	y.validation <- y[y$time %in% validation.years,]
	
	
	#plot
	plot.age <- ggplot(yhat, aes(x=time, y=yhat, color=age, group=age)) + geom_line() + theme_bw()  + scale_x_continuous(xlab) + scale_y_continuous(ylab)
	plot.age <- plot.age +
            scale_color_gradientn("Age",colours=rainbow(7)) +
                theme(legend.margin=unit(-0.02,"npc"),legend.text=element_text(size=8))
        if(uncertainty){
            plot.age <- plot.age + geom_ribbon(data=bounds,
                                               aes(x=time,y=NULL,ymin=lower.both,ymax=upper.both,fill=age),alpha=0.15,color=NA) + scale_fill_gradientn("Age",colours=rainbow(7))
        }
	# if insamp.obs==TRUE, plot in-sample observed values:
	plot.age <- plot.age + geom_path(data=y.obs,aes(x=time,y=y,color=age,group=age), linetype="dashed",na.rm=TRUE)
	# if validation==TRUE
	plot.age <- plot.age + geom_point(data=y.validation,aes(x=time,y=y,color=age,group=age), shape=4,na.rm=TRUE) 
	plot.age
}

####################################################


### time plot: age on x axis, dep var on y axis, groupings are times	
timeplot <- function(y,yhat,bounds,sample.frame, validation.years, uncertainty){
	xlab <- "Age"
    ylab <- "Data and Forecasts"

	y.obs <- y[y$time %in% validation.years==FALSE,]
	y.validation <- y[y$time %in% validation.years,]

	
	#plot			
	plot.time <- ggplot(yhat, aes(x=age, y=yhat, color=time, group=time)) + geom_line() + theme_bw() + scale_x_continuous(xlab) + scale_y_continuous(ylab)
	plot.time <- plot.time + scale_color_gradientn("Time",colours=rainbow(7)) + theme(legend.justification=c(0,1),legend.position=c(0.05,1),legend.direction="horizontal", legend.text=element_text(angle=45), legend.title.align=1, legend.background = element_rect(fill="transparent"))
	#if(time.opts$insamp.obs){
	#plot.time <- plot.time + geom_line(data=y.obs,aes(x=age,y=y,color=time,group=time), linetype="dashed") 
	#}	
	# if validation
	#geom_point(data=y.validation,aes(x=age,y=y,color=time,group=time), shape=4,na.rm=TRUE) 
	plot.time
}

