#Function to create tick mark labels...
getTickMarkLabelsIntersect <- function(low, high,digitsIn,tickMarks){ 
	#low = xLimits[1]; high=xLimits[2]; digitsIn=xDesiredDigits[i]; tickMarks =tickDivisions;
	exponent= floor(log10(high));
	nsmallVal = digitsIn-1-exponent;
	if (nsmallVal < 0){nsmallVal = 0;}
	result = c(format(low,nsmall = nsmallVal,digits = nsmallVal+1));	
	for (i in 1:tickMarks){
		result = c(result,format(low+(high-low)*i/tickMarks,nsmall = nsmallVal,digits = nsmallVal+1));
	}
	result = c(low-abs(high-low),result);
	return(result)
}
