dataFilePath = 'H:/UserData/winMigrationBU/Deakin/Marrow2011/';	#Path to the data file.
dataFileName = 'Marrow2011ReAna.csv';							#Data file name
rFilePath = dataFilePath;										#Path to getPlotLimits.r and getTickMarkLabels.r
figureTargetPath = 'H:/UserData/winMigrationBU/Deakin/Marrow2011/Julkaisu/Scatterplots/';	#Path to where the figures will be saved.
figureTargetPrefix = 'Scatterplot_';							#Prefix for figure names.
#setwd('H:/UserData/winMigrationBU/Deakin/Marrow2011');	
source(paste(rFilePath,'getPlotLimits.r',sep=""));
source(paste(rFilePath,'getTickMarkLabels.r',sep=""));
data <- read.table(paste(dataFilePath,dataFileName,sep=""),header=TRUE,sep=',');
# count.fields('Marrow2011ReAna.csv',sep=',')	#R can't handle ' in header!
xVariable = 'StratecMaMassD..g.cm³.';
yVariables = c('Radial.division.0.vBMD..mg.cm³.','Radial.division.1.vBMD..mg.cm³.','Radial.division.2.vBMD..mg.cm³.','MeA..mm².','CoA..mm².','MuA..cm².',
			'SSI..mm³.','CoD..mg.cm³.');
xAxisTitle = "MaD";
yAxisTitles = c("Tibial mid-shaft CoA","Tibial mid-shaft IPo");
pointColor  = c("#FFFFFF","#000000");
yDesiredDigits = c(2,2,2,2,2,2,2,2);
xDesiredDigits = c(3,3,3,3,3,3,3,3);
#xDesiredDigits = c(4,4,4,4,4,4,4,4);
yXtraSpace = c(0,0,0,0,0,0,0,0);
xXtraSpace = c(1,1,1,1,1,1,1,1);


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

for(i in 1:length(yVariables)){			#Loop to going through the yVariables
	y = data[yVariables[i]];			#Get y-axis data
	x = data[xVariable];				#Get x-axis data
	myline.fit <- lm(y[,1] ~ x[,1]);	#Get linear fit, i.e. correlation
	summaryData <- summary(myline.fit); # get information about the fit
	pValue =  summaryData$coefficients[2,4];
	slope =  summaryData$coefficients[2,1];
	int =  summaryData$coefficients[1,1];
	Rsq =  summaryData$r.squared[1];
	png(paste(figureTargetPath, figureTargetPrefix,yAxisTitles[i],'.png', sep = ""),width=1800,height=1200,res=200);	#Create a png to plot to
	par('mar' = c(3.3,3.6,3.0,1.1),'mgp'=c(2.2, 0.45, 0), 'bg' = pointColor[1],'cex'=2.0);								#Margins bottom, left, top, right
	#Get x-axis limits and ticks
	xLimits <- getPlotLimits(data[xVariable],xDesiredDigits[i],tickDivisions,xXtraSpace[i]);
	xTick <- getTickMarkLabels(xLimits[1],xLimits[2],xDesiredDigits[i],tickDivisions);

	#Get y-axis limits and ticks
	yLimits <- getPlotLimits(data[yVariables[i]],yDesiredDigits[i],tickDivisions,yXtraSpace[i]);
	yTick <- getTickMarkLabels(yLimits[1],yLimits[2],yDesiredDigits[i],tickDivisions);
	#Plot the figure.
	plot(
		x[,1],y[,1],type='p', pch = 19,
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
	axis(1, at=xTick, labels = xTick,tck = -0.02);	#X-axis
	axis(2, at=yTick, labels = yTick,las = 2,tck = -0.02);	#Y-axis
	#Add P-values, R2 and if significant, linear fit line.
	if (pValue <= 0.05){
		abline(lwd = 5.0,myline.fit) # draw the fit line on the plot
	}
	textXpos = c((xLimits[4]-xLimits[3])*0.9+xLimits[3],(xLimits[4]-xLimits[3])*0.8+xLimits[3]);
	PText = "";
	if (pValue <= 0.05){
		if (pValue < 0.001){
			PText = "P < 0.001";
		}
		if (pValue < 0.01 && pValue >= 0.001){
			PText = "P < 0.01";
		}
		if (pValue <= 0.05 && pValue >= 0.01){
			PText = "P < 0.05";
		}
	}else{
		PText = paste("P = ",format(round(pValue,digits=2),nsmall =2,digits =2),sep="");
	}
	RText = paste("=",format(round(Rsq,digits=2),nsmall=2,digits=2));
	text(textXpos[2] ,yLimits[3],substitute(paste(yb,", ",R^2,ya,sep=""),list(ya=RText,yb=PText)),pos=3,offset=0);
	

	dev.off()
}