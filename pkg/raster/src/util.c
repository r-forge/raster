#include    <R.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<math.h>
#include "Rmath.h"


double mod(double x, double n) {
	return(x - n * floor(x/n));
}

/* Convert degrees to radians */
double toRad(double deg) {
	return deg * M_PI / 180. ;
}

double toDeg(double rad) {
	return rad  * 180. / M_PI ;
}

double normLonDeg(double lon) {
	return mod(lon + 180., 360.) - 180.; 
}

double normLonRad(double lon) {
	return mod(lon + M_PI, M_2PI) - M_PI; 
}


