# Function to get plot limits
#parameters:
#dataIn:	An array of data for one axis
#digits:	How many significant figures are required
#tickMarks:	How many tickmarks are required
#returns:	An array [lowerLimit,upperLimit,lowerTickLimit,upperTickLimit]
getPlotLimits <- function(dataIn,digitsIn,tickMarks,xtraSpace) { #DEBUG dataIn =data[xVariable] digitsIn=xDesiredDigits[i] tickMarks=tickDivisions xtraSpace=xXtraSpace[i]
	exponent= floor(log10(max(dataIn,na.rm=TRUE)));
	multiplier = 10^((digitsIn-1)-exponent);
	lowerLimit = floor(min(dataIn,na.rm=TRUE)*multiplier )/multiplier;
	lowerTickLimit = lowerLimit-xtraSpace/multiplier;#;# 
	upperLimit = ceiling(max(dataIn,na.rm=TRUE)*multiplier )/multiplier;
	upperTickLimit = upperLimit+xtraSpace/multiplier;#;# 
	if ((upperTickLimit*multiplier -lowerTickLimit*multiplier )%%tickMarks!= 0){
		upperTickLimit = (upperTickLimit*multiplier +tickMarks-((upperTickLimit*multiplier -lowerTickLimit*multiplier )%%tickMarks))/multiplier; 
	}
	result = c(lowerLimit,upperLimit,lowerTickLimit,upperTickLimit);
	return(result)
}
