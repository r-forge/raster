/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

SEXP focal_fun(SEXP d, SEXP w, SEXP dim, SEXP fn, SEXP rho) {

	R_len_t i, j, k, q;
    SEXP R_fcall, ans, x;	
	int nrow, ncol, n, wn;
	double *xd, *xans, *xw, *xx;

    if(!isFunction(fn)) error("'fn' must be a function");
    if(!isEnvironment(rho)) error("'rho' should be an environment");
    PROTECT(R_fcall = lang2(fn, R_NilValue));
	
	SEXP wdim = getAttrib(w, R_DimSymbol);
	int wrows = INTEGER(wdim)[0];
	int wcols = INTEGER(wdim)[1];
	wn = wrows * wcols;
	
	PROTECT(d = coerceVector(d, REALSXP));
	PROTECT(w = coerceVector(w, REALSXP));
	
	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];

	n = nrow * ncol;
	PROTECT( ans = allocVector(REALSXP, n) );
	PROTECT(x = allocVector(REALSXP, wn));
	xx = REAL(x);
	
	if ((wrows % 2 == 0) | (wcols % 2 == 0))
		error("weights matrix must have uneven sides");
	int wr = floor(wrows / 2);
	int wc = floor(wcols / 2);

	xd = REAL(d);
	xans = REAL(ans);
	xw = REAL(w);

	for (i = ncol*wr; i < ncol * (nrow-wr); i++) {
		q = 0;
		for (j = -wr; j <= wr; j++) {
			for (k = -wc; k <= wc; k++) {
				xx[q] = xd[j * ncol + k + i];
				q++;
			}
		}
		SETCADR(R_fcall, x);
		xans[i] = REAL(eval(R_fcall, rho))[0];
	}
	

// Set edges to NA	
	for (i = wr; i < nrow; i++) {  
		for (j = 0; j < wc; j++) {
			xans[i * ncol + j] = R_NaReal;
			xans[i * ncol - j - 1] = R_NaReal;
		}
	}

// first rows
	for (i = 0; i < ncol*wr; i++) {  
		xans[i] = R_NaReal;
	}
// last rows
	for (i = ncol * (nrow-wr); i < n; i++) {  
		xans[i] = R_NaReal;
	}
	UNPROTECT(5);
	return(ans);
}

