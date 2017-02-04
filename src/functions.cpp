#include "functions.h"

// calculate euclidean distance
double distance(double x0, double y0, double x1, double y1) {
	return(sqrt(std::abs(Pow<2>(x0-x1)) + std::abs(Pow<2>(y0-y1))));
}


