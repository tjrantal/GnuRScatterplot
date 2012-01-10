dataFilePath = 'H:/UserData/winMigrationBU/Deakin/Marrow2011';	#Path to the data file.
dataFileName = 'Marrow2011ReAna.csv';							#Data file name
rFilePath = dataFilePath;										#Path to getPlotLimits.r and getTickMarkLabels.r
figureTargetPath = 'H:/UserData/winMigrationBU/Deakin/Marrow2011/Julkaisu/Scatterplots/';	#Path to where the figures will be saved.
figureTargetPrefix = 'Scatterplot_';							#Prefix for figure names.
#setwd('H:/UserData/winMigrationBU/Deakin/Marrow2011');	
source(paste(rFilePath,'getPlotLimits.r'));
source(paste(rFilePath,'getTickMarkLabels.r'));
data <- read.table(paste(dataFilePath,dataFileName),header=TRUE,sep=',');
# count.fields('Marrow2011ReAna.csv',sep=',')	#R can't handle ' in header!
xVariable = 'StratecMaMassD..g.cm³.';
yVariables = c('Radial.division.0.vBMD..mg.cm³.','Radial.division.1.vBMD..mg.cm³.','Radial.division.2.vBMD..mg.cm³.','MeA..mm².','CoA..mm².','MuA..cm².',
			'SSI..mm³.','CoD..mg.cm³.');
xAxisTitle = "MaD";
yAxisTitles = c("EndoD","MidD","PeriD","MeA","CoA","Muscle CSA","SSI","CoD");


pointColor  = c("#FFFFFF","#000000");
headings = c(
	substitute(paste(ya," [",mg/cm^3,"]"),list(ya=yAxisTitles[1]))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=yAxisTitles[2]))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=yAxisTitles[3]))
	,substitute(paste(ya," [",mm^2,"]"),list(ya=yAxisTitles[4]))
	,substitute(paste(ya," [",mm^2,"]"),list(ya=yAxisTitles[5]))
	,substitute(paste(ya," [",cm^2,"]"),list(ya=yAxisTitles[6]))
	,substitute(paste(ya," [",mm^3,"]"),list(ya=yAxisTitles[7]))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=yAxisTitles[8]))

	);
units = c(
	substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mm^2,"]"),list(ya=""))
	,substitute(paste(ya," [",mm^2,"]"),list(ya=""))
	,substitute(paste(ya," [",cm^2,"]"),list(ya=""))
	,substitute(paste(ya," [",mm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	);
tickDivisions = 2;

for(i in 1:length(yVariables)){
	y = data[yVariables[i]];
	x = data[xVariable];
	myline.fit <- lm(y[,1] ~ x[,1])
	summaryData <- summary(myline.fit); # get information about the fit
	pValue =  summaryData$coefficients[2,4];
	slope =  summaryData$coefficients[2,1];
	int =  summaryData$coefficients[1,1];
	Rsq =  summaryData$r.squared[1];
	png(paste(figureTargetPath, figureTargetPrefix,yAxisTitles[i],'.png', sep = ""),width=1800,height=1200,res=200)
	par('mar' = c(2.7,2.8,3.0,1.1),'mgp'=c(1.55, 0.45, 0), 'bg' = pointColor[1],'cex'=2.0); #Margins bottom, left, top, right
	xLimits <- getPlotLimits(data[xVariable],3,tickDivisions);
	xTick <- getTickMarkLabels(xLimits[1],xLimits[2],tickDivisions,3);

	yLimits <- getPlotLimits(data[yVariables[i]],3,tickDivisions);
	yTick <- getTickMarkLabels(yLimits[1],yLimits[2],tickDivisions,3);

	#Plot the figure
			plot(x[,1],y[,1],type='p', pch = 19,
			xlab=substitute(paste(xa," [",mg/cm^3,"]"),list(xa=xAxisTitle)),
			xaxp = c(xLimits[1],xLimits[2],tickDivisions), #For determining, xtick length...
			yaxp = c(yLimits[1],yLimits[2],tickDivisions ), #For determining, xtick length...
			ylab=units[i],
			frame.plot = FALSE,
			col = pointColor[2],
			xlim=c(xLimits[3],xLimits[4]),			
			ylim=c(yLimits[3],yLimits[4]),
			
			axes=FALSE,
			main = yAxisTitles[i],
			#cex = 2.0
			);

#ADD AXES
axis(1, at=xTick, labels = xTick)
# draw an axis on the left
axis(2, at=yTick, labels = yTick )

	if (pValue <= 0.05){
		abline(lwd = 5.0,myline.fit) # draw the fit line on the plot
	}
	if (pValue < 0.001){
		mtext("P < 0.001",line = -1.5, at=max(data[xVariable],na.rm=TRUE),cex = 2.0);
	}else{
		mtext(bquote(P == .(round(pValue*1000)/1000)),line = -1.5, at=max(data[xVariable],na.rm=TRUE),cex = 2.0);
	}
	mtext(bquote(R^2 == .(round(Rsq*100)/100)),line = -0.5, at=max(data[xVariable],na.rm=TRUE),cex = 2.0);
	dev.off()
}