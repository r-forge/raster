/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a,b) ((a)>(b)?(a):(b))


SEXP layerize(SEXP d, SEXP classes) {
					
	R_len_t i, j;
	SEXP v;
	int *xd, *xc;

	PROTECT(d = coerceVector(d, INTSXP));
	PROTECT(classes = coerceVector(classes, INTSXP));
	xd = INT(d);
	xc = INT(classes);
	
	PROTECT( v = allocVector(REALSXP, length(d) * length(classes)) );
	xv = REAL(v);
	for (i=0; i<length(v); i++) {
		xv[i] = 0;
	}	
	int m = length(d);
	for (i=0; i<length(d); i++) {
		if (R_FINITE(xd[i])) {
			for (j=0; j<length(classes); j++) {
				if (xd[i] == xc[i]) {
					xv[xc[i] * m + i] = 1;
					break;
				}
			}
		} else {
			for (j=0; j<length(classes); j++) {
				xv[xc[i] * m + i] = R_NaReal;
			}
		}
	}
	UNPROTECT(3);
	return(v);
}



