#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name = ".getMode")]]
double getMode(NumericVector x, int ties) {
	int iSize = x.length();
	
	NumericVector values(x);
    IntegerVector counts(iSize);

	if (ties < 3) {
		std::sort(x.begin(), x.end());
	}
	
    for (int i = 0; i < iSize; ++i) {
        counts[i] = 0;
        int j = 0;
        while ((j < i) && (values[i] != values[j])) {
            if (values[i] != values[j]) {
                ++j;
            }
        }
        ++(counts[j]);
    }
    int maxCount = 0;

	// first (lowest due to sorting)
	if (ties == 0) {
		for (int i = 1; i < iSize; ++i) {
			if (counts[i] > counts[maxCount]) {
				maxCount = i;
			}
		}
	// last	
	} else if (ties == 1) {
		for (int i = 1; i < iSize; ++i) {
			if (counts[i] >= counts[maxCount]) {
				maxCount = i;
			}
		}

	// dont care (first, but not sorted)
	} else if (ties == 2) {
		for (int i = 1; i < iSize; ++i) {
			if (counts[i] > counts[maxCount]) {
				maxCount = i;
			}
		}

	// random
	} else if (ties == 3) {
		for (int i = 1; i < iSize; ++i) {
			if (counts[i] > counts[maxCount]) {
				maxCount = i;
			} else if (counts[i] == counts[maxCount]) {
				if (R::runif(0,1) > 0.5) {
					maxCount = i;
				}			
			}
		}
	}
		
	
    return values[maxCount];
}

