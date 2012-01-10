setwd('H:/UserData/winMigrationBU/Deakin/Marrow2011');
source('getPlotLimits.r');
source('getTickMarkLabels.r');
data <- read.table('Marrow2011ReAna.csv',header=TRUE,sep=',');
# count.fields('Marrow2011ReAna.csv',sep=',')	#R can't handle ' in header!
xmuuttuja = 'StratecMaMassD..g.cm³.';
ymuuttuja = c('Radial.division.0.vBMD..mg.cm³.','Radial.division.1.vBMD..mg.cm³.','Radial.division.2.vBMD..mg.cm³.','MeA..mm².','CoA..mm².','MuA..cm².',
			'SSI..mm³.','CoD..mg.cm³.');
xakseli = "MaD";
yakseli = c("EndoD","MidD","PeriD","MeA","CoA","Muscle CSA","SSI","CoD");


pointColor  = c("#FFFFFF","#000000");
otsikot = c(
	substitute(paste(ya," [",mg/cm^3,"]"),list(ya=yakseli[1]))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=yakseli[2]))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=yakseli[3]))
	,substitute(paste(ya," [",mm^2,"]"),list(ya=yakseli[4]))
	,substitute(paste(ya," [",mm^2,"]"),list(ya=yakseli[5]))
	,substitute(paste(ya," [",cm^2,"]"),list(ya=yakseli[6]))
	,substitute(paste(ya," [",mm^3,"]"),list(ya=yakseli[7]))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=yakseli[8]))

	);
yksikot = c(
	substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mm^2,"]"),list(ya=""))
	,substitute(paste(ya," [",mm^2,"]"),list(ya=""))
	,substitute(paste(ya," [",cm^2,"]"),list(ya=""))
	,substitute(paste(ya," [",mm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	);
tickJakoja = 2;

for(i in 1:length(ymuuttuja)){
	y = data[ymuuttuja[i]];
	x = data[xmuuttuja];
	myline.fit <- lm(y[,1] ~ x[,1])
	summa <- summary(myline.fit); # get information about the fit
	Parvo =  summa$coefficients[2,4];
	slope =  summa$coefficients[2,1];
	int =  summa$coefficients[1,1];
	Rsq =  summa$r.squared[1];
	png(paste('H:/UserData/winMigrationBU/Deakin/Marrow2011/Julkaisu/Scatterplots/Scatterplot_',yakseli[i],'.png', sep = ""),width=1800,height=1200,res=200)
	par('mar' = c(2.7,2.8,3.0,1.1),'mgp'=c(1.55, 0.45, 0), 'bg' = pointColor[1],'cex'=2.0); #Margins bottom, left, top, right
	xLimits <- getPlotLimits(data[xmuuttuja],3,tickJakoja);
	xTick <- getTickMarkLabels(xLimits[1],xLimits[2],tickJakoja,3);

	yLimits <- getPlotLimits(data[ymuuttuja[i]],3,tickJakoja);
	yTick <- getTickMarkLabels(yLimits[1],yLimits[2],tickJakoja,3);

	#Plot the figure
			plot(x[,1],y[,1],type='p', pch = 19,
			xlab=substitute(paste(xa," [",mg/cm^3,"]"),list(xa=xakseli)),
			xaxp = c(xLimits[1],xLimits[2],tickJakoja), #For determining, xtick length...
			yaxp = c(yLimits[1],yLimits[2],tickJakoja ), #For determining, xtick length...
			ylab=yksikot[i],
			frame.plot = FALSE,
			col = pointColor[2],
			xlim=c(xLimits[3],xLimits[4]),			
			ylim=c(yLimits[3],yLimits[4]),
			
			axes=FALSE,
			main = yakseli[i],
			#cex = 2.0
			);

#ADD AXES
axis(1, at=xTick, labels = xTick)
# draw an axis on the left
axis(2, at=yTick, labels = yTick )

	if (Parvo <= 0.05){
		abline(lwd = 5.0,myline.fit) # draw the fit line on the plot
	}
	if (Parvo < 0.001){
		mtext("P < 0.001",line = -1.5, at=max(data[xmuuttuja],na.rm=TRUE),cex = 2.0);
	}else{
		mtext(bquote(P == .(round(Parvo*1000)/1000)),line = -1.5, at=max(data[xmuuttuja],na.rm=TRUE),cex = 2.0);
	}
	mtext(bquote(R^2 == .(round(Rsq*100)/100)),line = -0.5, at=max(data[xmuuttuja],na.rm=TRUE),cex = 2.0);
	dev.off()
}