/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"


SEXP focal_sum(SEXP d, SEXP w, SEXP dim, SEXP glob) {

	R_len_t i, j, k, q;
	SEXP val;
	int nrow, ncol, n, globll;
	double *xd, *xval, *xw;

	SEXP wdim = getAttrib(w, R_DimSymbol);
	int wrows = INTEGER(wdim)[0];
	int wcols = INTEGER(wdim)[1];
	
	PROTECT(d = coerceVector(d, REALSXP));
	PROTECT(w = coerceVector(w, REALSXP));

	globll = INTEGER(glob)[0];
	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];

	n = nrow * ncol;
	PROTECT( val = allocVector(REALSXP, n) );

	if ((wrows % 2 == 0) | (wcols % 2 == 0))
		error("weights matrix must have uneven sides");
	int wr = floor(wrows / 2);
	int wc = floor(wcols / 2);

	xd = REAL(d);
	xval = REAL(val);
	xw = REAL(w);

	for (i = ncol*wr; i < ncol * (nrow-wr); i++) {
		xval[i] = 0;
		q = 0;
		for (j = -wr; j <= wr; j++) {
			for (k = -wc; k <= wc; k++) {
				xval[i] += xd[j * ncol + k + i]  * xw[q];
				q++;
			}
		}
	}
	
// Set edges to NA	
// first and last columns
// do not do this for global lonlat data (wrap around is OK)
// but currently these edge values are wrong (the wrong warp around is used)
	if (globll != 0) {
		for (i = wr; i < nrow; i++) {  
			for (j = 0; j < wc; j++) {
				xval[i * ncol + j] = R_NaReal;
				xval[i * ncol - j - 1] = R_NaReal;
			}
		}
	} 

// first rows
	for (i = 0; i < ncol*wr; i++) {  
		xval[i] = R_NaReal;
	}
// last rows
	for (i = ncol * (nrow-wr); i < n; i++) {  
		xval[i] = R_NaReal;
	}
	UNPROTECT(3);
	return(val);
}

