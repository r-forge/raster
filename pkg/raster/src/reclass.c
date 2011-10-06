/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
#include "Rmath.h"

#include "util.h"


SEXP reclass(SEXP d, SEXP r, SEXP low, SEXP right, SEXP hasNA, SEXP onlyNA, SEXP valNA) {
					
	R_len_t i, j;
	SEXP val;
	double *xd, *rcl, *xval, lowval;

	PROTECT(d = coerceVector(d, REALSXP));

	SEXP rdim = getAttrib(r, R_DimSymbol);
	int a = INTEGER(rdim)[0];
	int b = a * 2;
	
	PROTECT(r = coerceVector(r, REALSXP));
	rcl = REAL(r);
	xd = REAL(d);


	int doright = INTEGER(right)[0];
	int dolowest  = INTEGER(low)[0];
	int NAonly = INTEGER(onlyNA)[0];
	int NAhas = INTEGER(hasNA)[0];
	double NAval = REAL(valNA)[0];

	int n = length(d);
	
	PROTECT( val = allocVector(REALSXP, n) );
	xval = REAL(val);

	
	if (NAonly) {
	
		for (i=0; i<n; i++) {
			if (!R_FINITE(xd[i])) {
				xval[i] = NAval;
			} else {
				xval[i] = xd[i];
			}
		}
		
	} else if (!NAhas) {

		if (doright) {
		
			if (dolowest) {
			
				lowval = rcl[0];
				for (j=0; j<a; j++) {
					if (rcl[j] < lowval) {
						lowval = rcl[j];
					}
				}
				for (i=0; i<n; i++) {
					xval[i] = xd[i];
					if (xd[i] == lowval) {
						xval[i] = rcl[b];
					} else {
						for (j=0; j<a; j++) {
							if ((xd[i] > rcl[j]) & (xd[i] <= rcl[j+a])) {
								xval[i] = rcl[j+b];
								break;
							}
						}
					}
				}
				
			} else {

				for (i=0; i<n; i++) {
					xval[i] = xd[i];
					for (j=0; j<a; j++) {
						if ((xd[i] > rcl[j]) & (xd[i] <= rcl[j+a])) {
							xval[i] = rcl[j+b];
							break;
						}
					}
				}			
			}
			
		} else {
		
			if (dolowest) { // which means highest if right=FALSE
			
				lowval = rcl[b];
				for (j=a; j<b; j++) {
					if (rcl[j] > lowval) {
						lowval = rcl[j];
					}
				}
				for (i=0; i<n; i++) {
					xval[i] = xd[i];
					if ((xd[i] == rcl[a+a])) {
						xval[i] = rcl[a+b];
					} else {
						for (j=0; j<a; j++) {
							if ((xd[i] >= rcl[j]) & (xd[i] < rcl[j+a])) {
								xval[i] = rcl[j+b];
								break;
							}
						}
					}
				}
				
			} else {
			
				for (i=0; i<n; i++) {
					xval[i] = xd[i];
					for (j=0; j<a; j++) {
						if ((xd[i] >= rcl[j]) & (xd[i] < rcl[j+a])) {
							xval[i] = rcl[j+b];
							break;
						}
					}
				}
			}
		}	
	
	
	} else { // hasNA and other values
	
		if (doright) {
		
			if (dolowest) {
			
				lowval = rcl[0];
				for (j=0; j<a; j++) {
					if (rcl[j] < lowval) {
						lowval = rcl[j];
					}
				}
				for (i=0; i<n; i++) {
					xval[i] = xd[i];
					if (xd[i] == lowval) {
						xval[i] = rcl[b];
					} else {
						for (j=0; j<a; j++) {
							if ((xd[i] > rcl[j]) & (xd[i] <= rcl[j+a])) {
								xval[i] = rcl[j+b];
								break;
							}
						}
					}
				}
				
			} else {

				for (i=0; i<n; i++) {
					xval[i] = xd[i];
					for (j=0; j<a; j++) {
						if ((xd[i] > rcl[j]) & (xd[i] <= rcl[j+a])) {
							xval[i] = rcl[j+b];
							break;
						}
					}
				}			
			}
			
		} else {
		
			if (dolowest) { // which means highest if right=FALSE
			
				lowval = rcl[b];
				for (j=a; j<b; j++) {
					if (rcl[j] > lowval) {
						lowval = rcl[j];
					}
				}
				for (i=0; i<n; i++) {
					xval[i] = xd[i];
					if ((xd[i] == rcl[a+a])) {
						xval[i] = rcl[a+b];
					} else {
						for (j=0; j<a; j++) {
							if (!R_FINITE(xd[i])) {
								xval[i] = NAval;
								break;
							} 
							if ((xd[i] >= rcl[j]) & (xd[i] < rcl[j+a])) {
								xval[i] = rcl[j+b];
								break;
							}
						}
					}
				}
				
			} else {
			
				for (i=0; i<n; i++) {
					xval[i] = xd[i];
					for (j=0; j<a; j++) {
						if (!R_FINITE(xd[i])) {
							xval[i] = NAval;
							break;
						} 
						if ((xd[i] >= rcl[j]) & (xd[i] < rcl[j+a])) {
							xval[i] = rcl[j+b];
							break;
						}
					}
				}
			}
		}
	}
	
	UNPROTECT(3);
	return(val);
}


