/* Robert Hijmans, November 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"


SEXP edge(SEXP d, SEXP dim, SEXP classes, SEXP type, SEXP fval) {

	R_len_t i, j;
	SEXP val;
	int nrow, ncol, n;
	int *xd, *xval;

	int class = INTEGER(classes)[0];
	int edgetype = INTEGER(type)[0];
	int falseval = INTEGER(fval)[0];

	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];
	n = nrow * ncol;
	PROTECT(d = coerceVector(d, INTSXP));
	xd = INTEGER(d);

	PROTECT( val = allocVector( INTSXP, n) );
	xval = INTEGER(val);

	int r[9] = { -1,-1,-1,0,0,0,1,1,1 };
	int c[9] = { -1,0,1,-1,0,1,-1,0,1 };
	
	if (class == 0) {
		if (edgetype == 0) {
			for (i = ncol; i < ncol * (nrow-1); i++) {
				xval[i] = R_NaInt;
				if ( xd[i] != R_NaInt ) {
					xval[i] = falseval;
					for (j=0; j<9; j++) {			
						if ( xd[r[j] * ncol + c[j] + i] == R_NaInt ) {
							xval[i] = 1;
							break;
						}
					}
				}
			}
			Rprintf ("%s \n", "1");
			
		} else if (edgetype == 1) {

			for (i = ncol; i < ncol * (nrow-1); i++) {
				xval[i] = falseval;
				if ( (xd[i] == R_NaInt) ) {
					xval[i] = R_NaInt;
					for (j=0; j<9; j++) {			
						if ( xd[r[j] * ncol + c[j] + i] != R_NaInt ) {
							xval[i] = 1;
							break;
						}
					}
				}
			}
			Rprintf ("%s \n", "2");
			
		} else {
			Rprintf ("%s \n", "3");

			// edgetype = 2
		}
		
		
	} else { // by class
	
		int test;
		int k;
		for (i = ncol; i < ncol * (nrow-1); i++) {
			xval[i] = R_NaInt;
			test = R_NaInt;
			if ( xd[i] != R_NaInt) {
				xval[i] = 0;
				for (j=0; j<9; j++) {
					k = r[j]*ncol+c[j]+i;
					if ( xd[k] != R_NaInt )  {
						if ( test == R_NaInt) {
							test = xd[k]; 
						} else if (test != xd[k]) {
							xval[i] = 1;
							break;
						}
					}
				}
			}
		}
		Rprintf ("%s \n", "4");
		
	}
	
	UNPROTECT(2);
	return(val);
}



