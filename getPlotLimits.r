# Function to get plot limits
#parameters:
#dataIn:	An array of data for one axis
#digits:	How many significant figures are required
#tickMarks:	How many tickmarks are required
#returns:	An array [lowerLimit,upperLimit,lowerTickLimit,upperTickLimit]
getPlotLimits <- function(dataIn,digitsIn,tickMarks) {
	exponent= floor(log10(max(dataIn,na.rm=TRUE)));
	if (exponent ==0){
		exponent = exponent+1;
	} 
	multiplier = 10^((digitsIn-1)-exponent);
	lowerLimit = floor(min(dataIn,na.rm=TRUE)*multiplier )/multiplier;
	lowerTickLimit = lowerLimit -1/multiplier;
	upperLimit = ceiling(max(dataIn,na.rm=TRUE)*multiplier )/multiplier;
	upperTickLimit = upperLimit +1/multiplier;
	if ((upperTickLimit*multiplier -lowerTickLimit*multiplier )%%tickMarks!= 0){
		upperTickLimit = (upperTickLimit*multiplier +tickMarks-((upperTickLimit*multiplier -lowerTickLimit*multiplier )%%tickMarks))/multiplier; 
	}
	result = c(lowerLimit,upperLimit,lowerTickLimit,upperTickLimit);
	return(result)
}
