dataFilePath = 'H:/UserData/winMigrationBU/Deakin/Marrow2011/';	#Path to the data file.
dataFileName = 'Marrow2011ReAna.csv';							#Data file name
rFilePath = dataFilePath;										#Path to getPlotLimits.r and getTickMarkLabels.r
figureTargetPath = 'H:/UserData/winMigrationBU/Deakin/Marrow2011/Julkaisu/Scatterplots/';	#Path to where the figures will be saved.
figureTargetPrefix = 'Scatterplot_';							#Prefix for figure names.

source(paste(rFilePath,'getPlotLimits.r',sep=""));
source(paste(rFilePath,'getTickMarkLabels.r',sep=""));
dataIn <- read.table(paste(dataFilePath,dataFileName,sep=""),header=TRUE,sep=',');
# count.fields('Marrow2011ReAna.csv',sep=',')	#R can't handle ' in header!
xVariables = c('StratecMaMassD..g.cmÂ³.');
yVariables = c('Radial.division.0.vBMD..mg.cmÂ³.','Radial.division.1.vBMD..mg.cmÂ³.','Radial.division.2.vBMD..mg.cmÂ³.','MeA..mmÂ².','CoA..mmÂ².','MuA..cmÂ².',
			'SSI..mmÂ³.','CoD..mg.cmÂ³.');


#Creating groups, use subset http://www.ats.ucla.edu/stat/r/faq/subset_R.htm
#Create group data
groupedData.group1 = subset(dataIn,Ryhmä == 1 | Ryhmä == 2 | Ryhmä == 4);
dim(groupedData.group1);
groupedData.group2 = subset(dataIn,Ryhmä == 3 | Ryhmä == 5 | Ryhmä == 6);
dim(groupedData.group2);
groupedData = list(groupedData.group1,groupedData.group2);


groundIndices = getGroupIndices();
xAxisTitles = c("MaD");
yAxisTitles = c("EndoD","MidD","PeriD","MeA","CoA","Muscle CSA","SSI","CoD");

xunits = c(
	substitute(paste(xa," [",mg/cm^3,"]"),list(xa=xAxisTitles[1]))
	);

yunits = c(
	substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mm^2,"]"),list(ya=""))
	,substitute(paste(ya," [",mm^2,"]"),list(ya=""))
	,substitute(paste(ya," [",cm^2,"]"),list(ya=""))
	,substitute(paste(ya," [",mm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	);

yDesiredDigits = c(2,2,2,2,2,2,2,2);
xDesiredDigits = c(3,3,3,3,3,3,3,3);

yXtraSpace = c(0,0,0,0,0,0,0,0);
xXtraSpace = c(1,1,1,1,1,1,1,1);


tickDivisions = 2;
pointColor  = c("#ffffff","#000000","#777777");

for (j in 1:length(xVariables)){
	for(i in 1:length(yVariables)){			#Loop to going through the yVariables
		#Create figure to plot to
		#Get x-axis limits and ticks
		xLimits <- getPlotLimits(dataIn[xVariables[j]],xDesiredDigits[i],tickDivisions,xXtraSpace[i]);
		xTick <- getTickMarkLabels(xLimits[1],xLimits[2],xDesiredDigits[i],tickDivisions);

		#Get y-axis limits and ticks
		yLimits <- getPlotLimits(dataIn[yVariables[i]],yDesiredDigits[i],tickDivisions,yXtraSpace[i]);
		yTick <- getTickMarkLabels(yLimits[1],yLimits[2],yDesiredDigits[i],tickDivisions);
		png(paste(figureTargetPath, figureTargetPrefix,xAxisTitles[j],'_',yAxisTitles[i],'.png', sep = ""),width=1800,height=1200,res=200);	#Create a png to plot to
		par('mar' = c(3.3,3.6,3.0,1.1),'mgp'=c(2.2, 0.45, 0), 'bg' = pointColor[1],'cex'=2.0);								#Margins bottom, left, top, right
		for (g in 1:length(groupedData)){
			groupData = groupedData[[g]];
			y = groupData[yVariables[i]];			#Get y-axis data
			x = groupData[xVariables[j]];				#Get x-axis data
			pearsonR = cor(x,y);
			myline.fit <- lm(y[,1] ~ x[,1]);	#Get linear fit, i.e. correlation
			summaryData <- summary(myline.fit); # get information about the fit
			pValue =  summaryData$coefficients[2,4];
			slope =  summaryData$coefficients[2,1];
			int =  summaryData$coefficients[1,1];
			Rsq =  summaryData$r.squared[1];
			

			#Plot the figure.
			if (g == 1){
				plot(
					x[,1],y[,1],type='p', pch = 19,
					xlab = xunits[j],
					xaxp = c(xLimits[1],xLimits[2],tickDivisions), #For determining, xtick length...
					yaxp = c(yLimits[1],yLimits[2],tickDivisions ), #For determining, xtick length...
					ylab = yunits[i],
					frame.plot = FALSE,
					col = pointColor[g+1],
					xlim=c(xLimits[3],xLimits[4]),			
					ylim=c(yLimits[3],yLimits[4]),
					axes=FALSE,
					main = yAxisTitles[i],
					#cex = 2.0
				);
				#ADD AXES
				axis(1, at=xTick, labels = xTick,tck = -0.02);	#X-axis
				axis(2, at=yTick, labels = yTick,las = 2,tck = -0.02);	#Y-axis
			}else{
				points(x[,1],y[,1],type='p', pch=21,col = pointColor[2],bg =NA);
			}
			#Add P-values, R2 and if significant, linear fit line.
			if (pValue <= 0.05){
				abline(lwd = 5.0,myline.fit,col = pointColor[g+1]) # draw the fit line on the plot
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
			mtext(substitute(paste(yb,", ",R^2,ya,sep=""),list(ya=RText,yb=PText)), side = 3, line = g-1,col = pointColor[g+1],adj = 1,cex = 2.0);
			#text(textXpos[2] ,yLimits[3]+(yLimits[4]-yLimits[3])*0.1*(g-1),substitute(paste(yb,", ",R^2,ya,sep=""),list(ya=RText,yb=PText)),pos=3,offset=0,col = pointColor[g+1]);
		}
	dev.off();
	}
}