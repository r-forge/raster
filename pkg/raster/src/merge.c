/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

SEXP merge(SEXP d) {

	R_len_t i, j, k;
	SEXP val;
	double *xd, *xval;

	SEXP dim = getAttrib(d, R_DimSymbol);

	if (isNull(dim))
		error("wdim is null");

	int nl = INTEGER(dim)[0];
	int n = INTEGER(dim)[1];
	PROTECT(d = coerceVector(d, REALSXP));
	PROTECT( val = allocVector(REALSXP, n) );

	xd = REAL(d);
	xval = REAL(val);

	for (i=0; i<n; i++) {
		xval[i] = R_NaReal;
		for (j=0; j<nl; j++) {
			k = i*nl + j;
			if (R_FINITE(xd[k])) {
				xval[i] = xd[k];
				break;
			} 
		}
	}
	UNPROTECT(2);
	return(val);
}



