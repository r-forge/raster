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


SEXP reclass(SEXP d, SEXP r, SEXP low, SEXP right) {
					
	R_len_t i, j;
	SEXP val;
	double *xd, *rcl, *xval;

	PROTECT(d = coerceVector(d, REALSXP));

	SEXP rdim = getAttrib(r, R_DimSymbol);
	int a = INTEGER(rdim)[0];
	int b = a * 2;
	
	PROTECT(r = coerceVector(r, REALSXP));
	rcl = REAL(r);
	xd = REAL(d);


	int doright = INTEGER(right)[0];
	int dolowest  = INTEGER(low)[0];
	int n = length(d);
	
	PROTECT( val = allocVector(REALSXP, n) );
	xval = REAL(val);
	
	for (i=0; i<n; i++) {
		xval[i] = xd[i];
	}
	
	if (doright) {
		for (i=0; i<n; i++) {
			if (dolowest) {
				if ((xd[i] == rcl[0])) {
					xval[i] = rcl[b];
					continue;
				}
			}
			for (j=0; j<a; j++) {
				if (!R_FINITE(rcl[j]) | !R_FINITE(rcl[j+a])) {
					if (!R_FINITE(xd[i])) {
						xval[i] = rcl[j+b];
						break;
					}
				} 
				if ((xd[i] > rcl[j]) & (xd[i] <= rcl[j+a])) {
					xval[i] = rcl[j+b];
					break;
				}
			}
		}
	} else {
		int n = length(d);
		for (i=0; i<n; i++) {
			for (j=0; j<a; j++) {
				if (!R_FINITE(rcl[j]) | !R_FINITE(rcl[j+a])) {
					if (!R_FINITE(xd[i])) {
						xval[i] = rcl[j+b];
						break;
					}
				} 
				if ((xd[i] >= rcl[j]) & (xd[i] < rcl[j+a])) {
					xval[i] = rcl[j+b];
					break;
				}
			}
			if (dolowest) {
				if ((xd[i] == rcl[a+a])) {
					xval[i] = rcl[a+b];
				}
			}
		}
	}
	
	UNPROTECT(3);
	return(val);
}


