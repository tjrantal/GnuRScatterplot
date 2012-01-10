#Function to create tick mark labels...
getTickMarkLabels <- function(low, high, tickMarks,digitsIn){
	exponent= floor(log10(high));
	if (exponent ==0){
		exponent = exponent+1;
	} 
	nsmallVal = digitsIn-1-exponent;
	if (nsmallVal < 0){nsmallVal = 0;}
	if (exponent ==0){
		exponent = exponent+1;
	} 
	result = c(format(low,nsmall = nsmallVal));	
	for (i in 1:tickMarks){
		result = c(result,format(low+(high-low)*i/tickMarks,digits = nsmallVal+1,nsmall = nsmallVal));
	}
	return(result)
}
