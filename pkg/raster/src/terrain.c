/* Robert Hijmans, June 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"


double mod(double x, double n) {
	return(x - n * floor(x/n));
}


SEXP terrain(SEXP d, SEXP dim, SEXP res, SEXP un, SEXP opt) {
	# define pi 3.14159265358979323846	
	# define halfpi 1.57079632679489661923
	# define twopi  6.283185301798647692	
					
	R_len_t i, j;
	SEXP val;
	int nrow, ncol, n, unit, option;
	double *xd, *xval, dx, dy;

	PROTECT(d = coerceVector(d, REALSXP));

	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];
	n = nrow * ncol;
	
	unit = INTEGER(un)[0];
	option = INTEGER(opt)[0];
	dx = REAL(res)[0];
	dy = REAL(res)[1];
	
	PROTECT( val = allocVector(REALSXP, n) );

	xd = REAL(d);
	xval = REAL(val);
	
	if (option == 1) {  
	// terrain ruggedness
		for (i = ncol+1; i < ncol * (nrow-1); i++) {
			xval[i] = (fabs(xd[i-1-ncol]-xd[i]) + fabs(xd[i-1]-xd[i]) + fabs(xd[i-1+ncol]-xd[i]) +  fabs(xd[i-ncol]-xd[i]) +
				fabs(xd[i+ncol]-xd[i]) +  fabs(xd[i+1-ncol]-xd[i]) + fabs(xd[i+1]-xd[i]) +  fabs(xd[i+1+ncol]-xd[i])) / 8;
		}
	} else if (option == 2) {
	// topograhic position
		for (i = ncol+1; i < ncol * (nrow-1); i++) {
			xval[i] = xd[i] - (xd[i-1-ncol] + xd[i-1] + xd[i-1+ncol] + xd[i-ncol]
								+ xd[i+ncol] + xd[i+1-ncol] + xd[i+1] + xd[i+1+ncol]) / 8;
		}
	} else if (option == 3) {
	// roughness 
		int a[9] = { -1-ncol, -1, -1+ncol, -ncol, 0, ncol, 1-ncol, 1, 1+ncol };
		double min, max, v;
		for (i = ncol+1; i < ncol * (nrow-1); i++) {
			min = xd[i + a[0]];
			max = xd[i + a[0]];
			for (j = 1; j < 9; j++) {
				v = xd[j+a[j]]; 
				if (v > max) {
					max = v;
				} else if (v < min) {
					min = v;
				}
			}
			xval[i] = max - min;
		}
	} else if (option == 4) {
	// slope 4 neighbors	

		double zy, zx; 
		double xw[9] = {-1,1};
		double yw[9] = {-1,1};
		for (i=0; i<2; i++) {
			xw[i] = xw[i] / (-2 * dx);
			yw[i] = yw[i] / (2 * dy);
		}
		for (i = ncol+1; i < ncol * (nrow-1); i++) {
			zx = xd[i-1] * xw[0] + xd[i+1] * xw[1];
			zy = xd[i-ncol] * yw[0] + xd[i+ncol] * yw[1];
			xval[i] = atan( sqrt( pow(zy, 2) + pow(zx, 2) ) );
		}
		if (unit == 0) {
			double adj = 180 / pi;
			for (i = ncol+1; i < ncol * (nrow-1); i++) {
				xval[i] = xval[i] * adj;
			}
		}
		
	} else if (option == 5) {
	// slope 8 neighbors	
	
		double zy, zx; 
		double xw[6] = {-1,-2,-1,1,2,1};
		double yw[6] = {-1,1,-2,2,-1,1};
		for (i=0; i<6; i++) {
			xw[i] = xw[i] / (-8 * dx);
			yw[i] = yw[i] / (8 * dy);
		}
		for (i = ncol+1; i < ncol * (nrow-1); i++) {
			zx = xd[i-1-ncol] * xw[0] + xd[i-1] * xw[1] + xd[i-1+ncol] * xw[2]
					+ xd[i+1-ncol] * xw[3] + xd[i+1] * xw[4] + xd[i+1+ncol] * xw[5];
  			zy = xd[i-1-ncol] * yw[0] + xd[i-1+ncol] * yw[1] + xd[i-ncol] * yw[2] 
					+ xd[i+ncol] * yw[3] + xd[i+1-ncol] * yw[4] + xd[i+1+ncol] * yw[5];
			xval[i] = atan( sqrt( pow(zy, 2) + pow(zx, 2) ) );
		}
		if (unit == 0) {
			double adj = 180 / pi;
			for (i = ncol+1; i < ncol * (nrow-1); i++) {
				xval[i] = xval[i] * adj;
			}
		}
		
	} else if (option == 6) {
	// aspect 4 neighbors	

		double zy, zx; 
		double xw[9] = {-1,1};
		double yw[9] = {-1,1};
		for (i=0; i<2; i++) {
			xw[i] = xw[i] / (-2 * dx);
			yw[i] = yw[i] / (2 * dy);
		}
		for (i = ncol+1; i < ncol * (nrow-1); i++) {
			zx = xd[i-1] * xw[0] + xd[i+1] * xw[1];
			zy = xd[i-ncol] * yw[0] + xd[i+ncol] * yw[1];
			zx = atan2(zy, zx);
			xval[i] = mod( halfpi -zx, twopi);
		}
		if (unit == 0) {
			double adj = 180 / pi;
			for (i = ncol+1; i < ncol * (nrow-1); i++) {
				xval[i] = xval[i] * adj;
			}
		}
	
	} else if (option == 7) {
	// aspect 8 neighbors	
	
		double zy, zx; 
		double xw[6] = {-1,-2,-1,1,2,1};
		double yw[6] = {-1,1,-2,2,-1,1};
		for (i=0; i<6; i++) {
			xw[i] = xw[i] / (-8 * dx);
			yw[i] = yw[i] / (8 * dy);
		}
		for (i = ncol+1; i < ncol * (nrow-1); i++) {
			zx = xd[i-1-ncol] * xw[0] + xd[i-1] * xw[1] + xd[i-1+ncol] * xw[2]
					+ xd[i+1-ncol] * xw[3] + xd[i+1] * xw[4] + xd[i+1+ncol] * xw[5];
  			zy = xd[i-1-ncol] * yw[0] + xd[i-1+ncol] * yw[1] + xd[i-ncol] * yw[2] 
					+ xd[i+ncol] * yw[3] + xd[i+1-ncol] * yw[4] + xd[i+1+ncol] * yw[5];
			zx = atan2(zy, zx);
			xval[i] = mod( halfpi -zx, twopi);
		}
		if (unit == 0) {
			double adj = 180 / pi;
			for (i = ncol+1; i < ncol * (nrow-1); i++) {
				xval[i] = xval[i] * adj;
			}
		}
		
	}
	
// Set edges to NA	
// first row	
	for (i = 0; i < ncol; i++) {  
		xval[i] = R_NaReal;
	}
// last row	
	for (i = ncol * (nrow-1); i < n; i++) {  
		xval[i] = R_NaReal;
	}
// first and last columns
	for (i = 1; i < nrow; i++) {  
		xval[i * ncol] = R_NaReal;
		xval[i * ncol - 1] = R_NaReal;
	}
	
	UNPROTECT(2);
	return(val);
}


