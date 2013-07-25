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


SEXP clamp(SEXP d, SEXP r, SEXP usevals) {
					
	R_len_t i;
	SEXP val;
	double *xd, *rcl, *xval;
	PROTECT(d = coerceVector(d, REALSXP));
	PROTECT(r = coerceVector(r, REALSXP));
  	int uv = INTEGER(usevals)[0];
	rcl = REAL(r);
	xd = REAL(d);

	int n = length(d);
	PROTECT( val = allocVector(REALSXP, n) );
	xval = REAL(val);
	
	for (i=0; i<n; i++) {
		if ( (R_IsNA(xd[i])) | (xd[i] < rcl[1]) | (xd[i] > rcl[2])) {
			xval[i] = R_NaReal;
		} else {
			xval[i] = xd[i];
		}
	}	
	UNPROTECT(3);
	return(val);
}


