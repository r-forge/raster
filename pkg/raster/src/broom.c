/* Robert Hijmans, October 2011 
This is an implementation of J. Ronald Eastman's pushbroom algorithm
*/

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

#define min( a, b ) ( ((a) < (b)) ? (a) : (b) )


SEXP broom(SEXP d, SEXP dm, SEXP dist, SEXP dw) {

	R_len_t i, r;
    SEXP dis;
	int n, nr, nc, down;
	double *xd, *xdis;
	double dx, dy, dxy;
	
	PROTECT(d = coerceVector(d, REALSXP));
	
	dx = REAL(dist)[0];
	dy = REAL(dist)[1];
	dxy = REAL(dist)[2];
	down = INTEGER(dw)[0];
	nr = INTEGER(dm)[0];
	nc = INTEGER(dm)[1];
	n = nr * nc;
//	Rprintf ("n = %i \n", n);

	xd = REAL(d);

	PROTECT( dis = allocVector(REALSXP, n) );
	xdis = REAL(dis);
	
	for (i=0; i<n; i++) {
		xdis[i] = R_PosInf;
	}

	if (down) {	
		//left to right
		//r = 0; first row, no row above it
		//i = 0; first cell, no cell left of it
		if ( R_FINITE(xd[0])) {
			xdis[0] = 0;
		}
		for (i=1; i<nc; i++) {
			if (! R_FINITE(xd[i])) {
				xdis[i] = xdis[i-1] + dx;
			} else {
				xdis[i] = 0;
			}
		}
		//other rows	
		for (r=1; r<nr; r++) {
			i=r*nc;
			if (R_FINITE(xd[i])) {
				xdis[i] = 0;
			}
			for (i=(r*nc+1); i<((r+1)*nc); i++) {
				if (! R_FINITE(xd[i])) {
					xdis[i] = min(min(xdis[i-1] + dx, xdis[i-nc] + dy), xdis[i-nc-1] + dxy);
				} else {
					xdis[i] = 0;
				}
			}
		}

		//right to left
		//first row
		if ( R_FINITE(xd[nc-1]) ) {
			xdis[nc-1] = 0;
		}
		for (i=(nc-2); i >= 0; i--) {
			if (! R_FINITE(xd[i])) {
				xdis[i] = xdis[i+1] + dx;
			} else {
				xdis[i] = 0;
			}
		}
		// other rows
		for (r=1; r<nr; r++) {
			i=(r+1)*nc-1;
			if (! R_FINITE(xd[i])) {
				xdis[i] = min(xdis[i], xdis[i-nc] + dy);
			} 
			for (i=(r+1)*nc-2; i>(r*nc-1); i--) {
				if (! R_FINITE(xd[i])) {
					xdis[i] = min(min(min(xdis[i], xdis[i+1] + dx), xdis[i-nc] + dy), xdis[i-nc+1] + dxy);		
				}
			}
		}
	
	} else { 
	// bottom to top
		// left to right

		// first (last) row
		r = nr-1;
		// first cell
		i = r*nc;
		if ( R_FINITE(xd[i])) {
			xdis[i] = 0;
		}
		// other cells
		for (i=(r*nc+1); i<n; i++) {
			if (! R_FINITE(xd[i])) {
				xdis[i] = xdis[i-1] + dx;
			} else {
				xdis[i] = 0;
			}
		}
		// other rows
		for (r=nr-2; r >= 0; r--) {
			i=r*nc;
			if (R_FINITE(xd[i])) {
				xdis[i] = 0;
			}
			for (i=(r*nc+1); i<((r+1)*nc); i++) {
				if (! R_FINITE(xd[i])) {
					xdis[i] = min(min(xdis[i-1] + dx, xdis[i+nc] + dy), xdis[i+nc-1] + dxy);
				} else {
					xdis[i] = 0;
				}
			}
		}
		
		// right to left
		// first row
		// first cell
		if ( R_FINITE(xd[n-1])) {
			xdis[n-1] = 0;
		}
		// other cells
		r = nr-1;
		for (i=n-2; i > r*nc; i--) {
			if (! R_FINITE(xd[i])) {
				xdis[i] = xdis[i-1] + dx;
			} else {
				xdis[i] = 0;
			}
		}
		// other rows
		for (r=nr-2; r >= 0; r--) {
			i=(r+1)*nc-1;
			if (! R_FINITE(xd[i])) {
				xdis[i] = min(xdis[i], xdis[i+nc] + dy);
			} 
			for (i=(r+1)*nc-2; i>(r*nc-1); i--) {
				if (! R_FINITE(xd[i])) {
					xdis[i] = min(min(min(xdis[i], xdis[i+1] + dx), xdis[i+nc] + dy), xdis[i+nc+1] + dxy);
				}
			}
		}
	}
	UNPROTECT(2);
	return(dis);
}

