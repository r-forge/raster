#include "geodesic.h"

#include <Rinternals.h>

/* Robert Hijmans, May 2015 */
/**
 * A simple program to solve the inverse geodesic problem(for the WGS84 ellipsoid).
 **********************************************************************/
 
SEXP inversegeodesic(SEXP latitude1, SEXP longitude1, SEXP latitude2, SEXP longitude2, SEXP pa, SEXP pf) {


  PROTECT(latitude1 = coerceVector(latitude1, REALSXP));
  PROTECT(longitude1 = coerceVector(longitude1, REALSXP));
  PROTECT(latitude2 = coerceVector(latitude2, REALSXP));
  PROTECT(longitude2 = coerceVector(longitude2, REALSXP));
  double a = REAL(pa)[0];
  double f = REAL(pf)[0];

  double *lat1, *lon1, *lat2, *lon2, *xr;
  lat1 = REAL(latitude1);
  lon1 = REAL(longitude1);
  lat2 = REAL(latitude2);
  lon2 = REAL(longitude2);

  SEXP r;
  PROTECT( r = allocVector(REALSXP, length(latitude1) ));
  xr = REAL(r);  
   
  double azi1, azi2, s12;
  struct geod_geodesic g;

  geod_init(&g, a, f);
  
  int i;
  for (i=0; i < length(latitude1); i++) {
    geod_inverse(&g, lon1[i], lat1[i], lon2[i], lat2[i], &s12, &azi1, &azi2);
    xr[i] = s12;
/*    xr[i*3+1] = azi1;
    xr[i*3+2] = azi2;
*/  }
  
  UNPROTECT(5);
  return r;
}


SEXP polygonarea(SEXP latitude, SEXP longitude, SEXP pa, SEXP pf) {


  PROTECT(latitude = coerceVector(latitude, REALSXP));
  PROTECT(longitude = coerceVector(longitude, REALSXP));
  double *lat, *lon, *xr;
  lat = REAL(latitude);
  lon = REAL(longitude);

  double a = REAL(pa)[0];
  double f = REAL(pf)[0];
  
/*  double a = 6378137, f = 1/298.257223563;  WGS84 */
  double A, P;
  int  i;
  struct geod_geodesic g;
  struct geod_polygon p;
  
  geod_init(&g, a, f);
  geod_polygon_init(&p, 0);

  for (i=0; i<length(latitude); i++) {
    geod_polygon_addpoint(&g, &p, lon[i], lat[i]);
  }
  
  geod_polygon_compute(&g, &p, 0, 1, &A, &P);
  
  SEXP r;
  PROTECT( r = allocVector(REALSXP, 1) );
  xr = REAL(r);
/*  xr[0] = n;
  xr[1] = P; */
  xr[0] = A;

  UNPROTECT(3);
  return(r);  
}
