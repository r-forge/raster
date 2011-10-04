
#include <R.h>
#include <Rdefines.h>

SEXP funky(SEXP list, SEXP fn, SEXP rho) {
       R_len_t i, n = length(list);
       SEXP R_fcall, ans;
     
       if(!isList(list)) error("'list' must be a list");
       if(!isFunction(fn)) error("'fn' must be a function");
       if(!isEnvironment(rho)) error("'rho' should be an environment");
	   
       PROTECT(R_fcall = lang2(fn, R_NilValue));
       PROTECT(ans = allocVector(VECSXP, n));
	   
       for(i = 0; i < n; i++) {
         SETCADR(R_fcall, VECTOR_ELT(list, i));
         SET_VECTOR_ELT(ans, i, eval(R_fcall, rho));
       }
	    
       setAttrib(ans, R_NamesSymbol, getAttrib(list, R_NamesSymbol));
       UNPROTECT(2);
       return(ans);
}



SEXP funky2(SEXP v, SEXP fn, SEXP rho) {
//library(raster); a = 1:100; 
//.Call('funky2', as.double(a), mean, new.env() )


    R_len_t i, j, n = 10, m=10;
    SEXP R_fcall, ans, x;
    double *xv, *xx, *zz;
	 
	 
    if(!isFunction(fn)) error("'fn' must be a function");
    if(!isEnvironment(rho)) error("'rho' should be an environment");
	   
    PROTECT(R_fcall = lang2(fn, R_NilValue));
	PROTECT(v = coerceVector(v, REALSXP));
	
    PROTECT(ans = allocVector(REALSXP, m));
	PROTECT(x = allocVector(REALSXP, n));
	
	xv = REAL(v);
	xx = REAL(x);
	zz = REAL(ans);
	for(i = 0; i < m; i++) {
		for(j = 0; j < n; j++) {
			xx[j] = xv[i*10+j];
		}
		SETCADR(R_fcall, x);
		zz[i] = REAL(coerceVector(eval(R_fcall, rho), REALSXP))[0];
	}
    UNPROTECT(4);
    return(ans);
}

