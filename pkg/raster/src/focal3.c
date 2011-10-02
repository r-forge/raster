/* Robert Hijmans, June 2011 */

#include <R.h>
#include <Rinternals.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<math.h>
#include	<Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

SEXP focal3c(SEXP d, SEXP w, SEXP dim) {

	R_len_t i;
	SEXP val;
	int nrow, ncol, n;
	double *xd, *xval, *xw;

	PROTECT(d = coerceVector(d, REALSXP));
	PROTECT(w = coerceVector(w, REALSXP));
	
	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];
	n = nrow * ncol;
	
	PROTECT( val = allocVector(REALSXP, n) );

	xd = REAL(d);
	xval = REAL(val);
	xw = REAL(w);

	for (i = ncol+1; i < ncol * (nrow-1); i++) {
		xval[i] = xd[i-1-ncol] * xw[0] + xd[i-1] * xw[1] + xd[i-1+ncol] * xw[2] + xd[i-ncol] * xw[3] + xd[i] * xw[4] 
                      + xd[i+ncol] * xw[5] + xd[i+1-ncol] * xw[6] + xd[i+1] * xw[7] + xd[i+1+ncol] * xw[8];		
	}
	

	for (i = 0; i < ncol; i++) {  
		xval[i] = R_NaReal;
	}
	for (i = ncol * (nrow-1); i < n; i++) {  
		xval[i] = R_NaReal;
	}
	for (i = 1; i < nrow; i++) {  
		xval[i * ncol] = R_NaReal;
		xval[i * ncol - 1] = R_NaReal;
	}

	
	UNPROTECT(3);
	return(val);
}


