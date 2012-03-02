dataFilePath = 'H:/UserData/winMigrationBU/Deakin/Marrow2011/';	#Path to the data file.
dataFilePath = '/home/timo/Desktop/Media/sf_Deakin/Marrow2011/';	#Path to the data file.
dataFileName = 'Marrow2011ReAnaB.csv';							#Data file name
rFilePath = dataFilePath;										#Path to getPlotLimits.r and getTickMarkLabels.r
figureTargetPath = 'H:/UserData/winMigrationBU/Deakin/Marrow2011/Julkaisu/BarGraphs/';	#Path to where the figures will be saved.
figureTargetPath = '/home/timo/Desktop/Media/sf_Deakin/Marrow2011/Julkaisu/BarGraphs/';	#Path to where the figures will be saved.
figureTargetPrefix = 'BarGraph_';							#Prefix for figure names.

source(paste(rFilePath,'getPlotLimits.r',sep=""));
source(paste(rFilePath,'getTickMarkLabels.r',sep=""));
dataIn <- read.table(paste(dataFilePath,dataFileName,sep=""),header=TRUE,sep=',');
# count.fields('Marrow2011ReAna.csv',sep=',')	#R can't handle ' in header!

variables = c('StratecMaMassD..g.cm..','Density.weighted.fat.percentage....','MuA..cm..','CoA..mm..',
			'SSI..mm..','CoD..mg.cm..');

groups = c("High impact","Odd impact","High Magnitude","Repetitive, low impact","Repetitive, non-impact","Referents");
axisTitles = c("MaD","Fat Percentage","Muscle CSA","CoA","SSI","CoD");


units = c(
	substitute(paste(xa," [",mg/cm^3,"]"),list(xa=axisTitles[1])),
	substitute(paste(xa," [%]"),list(xa=axisTitles[2])),
	substitute(paste(xa," [",cm^2,"]"),list(xa=axisTitles[3])),
	substitute(paste(xa," [",mm^2,"]"),list(xa=axisTitles[4])),
	substitute(paste(xa," [",mm^3,"]"),list(xa=axisTitles[5])),
	substitute(paste(xa," [",mg/cm^3,"]"),list(xa=axisTitles[6]))
	);


desiredDigits = c(2,2,2,2,2,2,2,2);


yXtraSpace = c(0,0,0,0,0,0,0,0);
xXtraSpace = c(1,1,1,1,1,1,1,1);


tickDivisions = 2;
pointColor  = c("#ffffff","#777777","#000000");

for (i in 1:length(variables)){

	averages = aggregate(dataIn[,variables[i]],by=list(dataIn[,"Ryhma"]),FUN=mean, na.rm=TRUE);
	stdevs = aggregate(dataIn[,variables[i]],by=list(dataIn[,"Ryhma"]),FUN=sd, na.rm=TRUE);
	#Get y-axis limits and ticks
	yLimits <- getPlotLimits(averages,desiredDigits[i],tickDivisions,yXtraSpace[i]);
	yTick <- getTickMarkLabels(yLimits[1],yLimits[2],desiredDigits[i],tickDivisions);
	png(paste(figureTargetPath, figureTargetPrefix,axisTitles[i],'.png', sep = ""),width=1800,height=1200,res=200);	#Create a png to plot to
	par('mar' = c(3.3,3.6,3.0,1.1),'mgp'=c(2.2, 0.45, 0), 'bg' = pointColor[1],'cex'=2.0);								#Margins bottom, left, top, right
	#Plot the figure.
	#barplot(height=averages[,], main=axisTitles[i], horiz=FALSE, names.arg=groups, cex.names=0.8);#,
	barplot(height=c(averages[,2]), main=axisTitles[i], horiz=FALSE, names.arg=groups, cex.names=0.8);#,
	#	yaxp = c(yLimits[1],yLimits[2],tickDivisions ), #For determining, xtick length...
	#	ylab = units[i],
	#	ylim=c(yLimits[3],yLimits[4])
	#);
	dev.off();
}