// (Euclidian) distance on a plane
double distance_plane(double x1, double y1, double x2, double y2);
std::vector<double> distance_plane(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2);
std::vector<double> distanceToNearest_plane(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2);

// (Geodesic) distance on a speroid 
double distance_lonlat(double lon1, double lat1, double lon2, double lat2, double a, double f);
std::vector<double> distance_lonlat(std::vector<double> lon1, std::vector<double> lat1, std::vector<double> lon2, std::vector<double> lat2, double a, double f) ;
std::vector<double> distanceToNearest_lonlat(std::vector<double> lon1, std::vector<double> lat1, std::vector<double> lon2, std::vector<double> lat2, double a, double f);

// direction
double direction_lonlat(double lon1, double lat1, double lon2, double lat2, bool degrees, double a, double f);
std::vector<double> direction_lonlat(std::vector<double> lon1, std::vector<double> lat1, std::vector<double> lon2, std::vector<double> lat2, bool degrees, double a, double f);
std::vector<double> directionToNearest_lonlat(std::vector<double> lon1, std::vector<double> lat1, std::vector<double> lon2, std::vector<double> lat2, bool degrees, bool from, double a, double f);

double direction_plane(double x1, double y1, double x2, double y2, bool degrees);
std::vector<double> direction_plane(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2, bool degrees);
std::vector<double> directionToNearest_plane(std::vector<double> x1, std::vector<double> y1, std::vector<double> x2, std::vector<double> y2, bool degrees, bool from);


