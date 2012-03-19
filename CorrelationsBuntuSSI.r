#Ubuntu execute from R command line source("/media/sf_Deakin/Marrow2011/CorrelationsBuntuSSI.r")

dataFilePath = 'H:/UserData/winMigrationBU/Deakin/Marrow2011/';	#Path to the data file.
dataFilePath = '/home/timo/Desktop/Media/sf_Deakin/Marrow2011/';	#Path to the data file.
dataFileName = 'Marrow2011ReAnaB.csv';							#Data file name
rFilePath = dataFilePath;										#Path to getPlotLimits.r and getTickMarkLabels.r
figureTargetPath = 'H:/UserData/winMigrationBU/Deakin/Marrow2011/Julkaisu/Scatterplots/';	#Path to where the figures will be saved.
figureTargetPath = '/home/timo/Desktop/Media/sf_Deakin/Marrow2011/Julkaisu/Scatterplots/';	#Path to where the figures will be saved.
figureTargetPrefix = 'Scatterplot_';							#Prefix for figure names.

source(paste(rFilePath,'getPlotLimits.r',sep=""));
source(paste(rFilePath,'getXPlotLimits.r',sep=""));
source(paste(rFilePath,'getTickMarkLabels.r',sep=""));
source(paste(rFilePath,'getTickMarkLabelsIntersect.r',sep=""));
dataIn <- read.table(paste(dataFilePath,dataFileName,sep=""),header=TRUE,sep=',');
# count.fields('Marrow2011ReAna.csv',sep=',')	#R can't handle ' in header!
xVariables = c('StratecMaMassD..g.cm..');
yVariables = c('CoA..mm..','CoD..mg.cm..','SSI..mm..');


#Creating groups, use subset http://www.ats.ucla.edu/stat/r/faq/subset_R.htm
#Create group data
groupedData.group1 = subset(dataIn,Ryhma == 1 | Ryhma == 2 | Ryhma == 4);
dim(groupedData.group1);
groupedData.group2 = subset(dataIn,Ryhma == 3 | Ryhma == 5 | Ryhma == 6);
dim(groupedData.group2);
groupedData = list(groupedData.group1,groupedData.group2);

groupSymbols = c("\u25CF","\u25CB");

xAxisTitles = c("MaD");
yAxisTitles = c("Cortical Area","Cortical Density","SSI");

xunits = c(
	substitute(paste(xa," [",mg/cm^3,"]"),list(xa=xAxisTitles[1]))
	);

yunits = c(
	substitute(paste(ya," [",mm^2,"]"),list(ya=""))
	,substitute(paste(ya," [",mg/cm^3,"]"),list(ya=""))
	,substitute(paste(ya," [",mm^3,"]"),list(ya=""))
	);

#Set x-axis limits and ticks manually
xTicks = c(
			c(0,"0.920","0.940","0.960","0.980","1.000","1.020")
			);
dim(xTicks) = c(7,1);
xLims = c(
			c(0.92,1.02,0.92,1.02)
			);
dim(xLims) = c(4,1);
	
#Set y-axis limits and ticks manually
yTicks = c(
			c(100,200,300,400,500,700),
			c(0,1000,1100,1200,1300,1500),
			c(300,1100,1900,2700,3500,5000)
						
			);
dim(yTicks) = c(6,3);
yLims = c(
			c(100,500,100,500),
			c(1000,1200,1000,1200),
			c(300,3500,300,3500)
			);
dim(yLims) = c(4,3);


tickDivisions = 2;
pointColor  = c("#ffffff","#000000","#000000");

for (j in 1:length(xVariables)){
	for(i in 3){#:length(yVariables)){			#Loop to going through the yVariables
		#Create figure to plot to
		#Get x-axis limits and ticks
		xLimits = xLims[,1];
		xTick = xTicks[,1];
		
		#Get y-axis limits and ticks
		yLimits = yLims[,i];
		yTick = yTicks[,i];
		
		png(paste(figureTargetPath, figureTargetPrefix,100+i,xAxisTitles[j],'_',yAxisTitles[i],'.png', sep = ""),width=2400,height=2400,res=300);	#Create a png to plot to
		par('mar' = c(3.6,4.0,3.0,1.1),'mgp'=c(2.7, 0.45, 0), 'bg' = pointColor[1],'cex'=2.0, 'xaxs'="i", 'yaxs'="i");								#Margins bottom, left, top, right
		for (g in 1:length(groupedData)){
			groupData = groupedData[[g]];
			x = dataIn[xVariables[j]];
			y = dataIn[yVariables[i]];			
			pearsonR = cor(x,y);
			myline.fit <- lm(y[,1] ~ x[,1]);	#Get linear fit, i.e. correlation
			summaryData <- summary(myline.fit); # get information about the fit
			pValue =  summaryData$coefficients[2,4];
			slope =  summaryData$coefficients[2,1];
			int =  summaryData$coefficients[1,1];
			Rsq =  summaryData$r.squared[1];
			
			y = groupData[yVariables[i]];			#Get y-axis data
			x = groupData[xVariables[j]];				#Get x-axis data

			#Plot the figure.
			if (g == 1){
				plot(
					x[,1],y[,1],type='p', pch = 19,
					xlab = xunits[j],
					#xaxp = c(xLimits[1],xLimits[2],tickDivisions), #For determining, xtick length...
					#yaxp = c(yLimits[1],yLimits[2],tickDivisions ), #For determining, xtick length...
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
				points(x[,1],y[,1],type='p', pch=21,col = pointColor[g+1],bg ="white");
			}
			#Add P-values, R2 and if significant, linear fit line.
			if (g ==1){
				if (pValue <= 0.05){
					abline(lwd = 5.0,myline.fit,col = pointColor[g+1],lty =g) # draw the fit line on the plot
				}
				textXpos = c((xLimits[4]-xLimits[3])*0.9+xLimits[3],(xLimits[4]-xLimits[3])*0.75+xLimits[3]);
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
				#mtext(substitute(paste(R^2,ya,", ",yb,sep=""),list(ya=RText,yb=PText)), side = 3, line = g-1,col = pointColor[g+1],adj = 1,cex = 1.8);
				text(textXpos[2] ,yLimits[3]+(yLimits[4]-yLimits[3])*0.05+(yLimits[4]-yLimits[3])*0.1*(g-1),substitute(paste("  ",R^2,ya,", ",yb,sep=""),list(ya=RText,yb=PText)),pos=3,offset=0,col = pointColor[g+1]);
			}
		}
	dev.off();
	}
}