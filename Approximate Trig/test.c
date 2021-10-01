#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
#include <time.h>

#define STRRED   "\x1b[31m" 
#define STRGREEN "\x1b[32m"
#define STRNORM  "\x1b[m"

#define NUM_LOOPS 100000

struct circle {
	double c;
	double s;
};

double a = 8*M_SQRT2/3 - 3;
double b = 4 - 8*M_SQRT2/3;

double complex complex_turn(double turn)
{
	return cexp(I*M_PI*turn);
}

double complex approx_turn(double turn)
{
	double p = turn*(b * turn * turn + a);
	double q = p*p;
	double r = 1 + q;
	double c = (1 - q) / r, s = 2*p / r;
	
	return (c*c - s*s) + I*(2*c*s);
}

void print_errors(const double *inputs, 
					const double complex *ideals,
					const double complex *approxs,
					int n)
{
	double c_error, s_error;
	double largest_c_error, largest_s_error;
	size_t largest_c_index, largest_s_index;

	double total_c_error = 0, total_s_error = 0;

	size_t i;
	double complex ideal, approx;

	for (i = 0; i < n; i++) {
		ideal = ideals[i];
		approx = approxs[i];

		//squared error in c components
		c_error = creal(ideal) - creal(approx);
		c_error *= c_error;
		//squared error in s components
		s_error = cimag(ideal) - cimag(approx);
		s_error *= s_error;
		
		if (largest_c_error < c_error) {
			largest_c_error = c_error;
			largest_c_index = i;
		}
		
		if (largest_s_error < s_error) {
			largest_s_error = s_error;
			largest_s_index = i;
		}

		total_c_error += c_error;
		total_s_error += s_error;
	}
	//these now contain the *average* squared error
	total_c_error /= (double) n;
	total_s_error /= (double) n;

	printf("Squared error in cosines: \n\tAverage: %f (%f%% error)\n\tLargest: %f (%f%% error)" \
		"\n\t\tInput:\t\t%f\n\t\tValue:\t\t%f\n\t\tApproximation:\t%f\n"
		, total_c_error, sqrt(total_c_error) * 100
		, largest_c_error, sqrt(largest_c_error) * 100
		, inputs[largest_c_index], creal(ideals[largest_c_index]), creal(approxs[largest_c_index]));
	printf("Squared error in sines: \n\tAverage: %f (%f%% error)\n\tLargest: %f (%f%% error)" \
		"\n\t\tInput:\t\t%f\n\t\tValue:\t\t%f\n\t\tApproximation:\t%f\n"
		, total_s_error, sqrt(total_s_error) * 100
		, largest_s_error, sqrt(largest_s_error) * 100
		, inputs[largest_c_index], cimag(ideals[largest_s_index]), cimag(approxs[largest_s_index]));
}

//time the length of the computation `f` in nanoseconds
long time_computation(	double complex (*f)(double), 
						const double *inputs, 
						double complex *results, 
						int n)
{
	size_t i;
	long tick;
	struct timespec tp;

	clock_gettime(CLOCK_MONOTONIC, &tp);
	tick = tp.tv_nsec;
	for (i = 0; i < n; i++) {
		results[i] = f(inputs[i]);
	}
	//this isn't quite proper, since the clock may have ticked over a second
	clock_gettime(CLOCK_MONOTONIC, &tp);

	return tp.tv_nsec - tick;
}

int main(int argn, char **args)
{
	long trig_time, rat_time;

	double rands[NUM_LOOPS];
	double complex trigs[NUM_LOOPS];
	double complex rats[NUM_LOOPS];

	size_t i;
	for (i = 0; i < NUM_LOOPS; i++) {
		rands[i] = rand() / (double) RAND_MAX;
	}

	trig_time = time_computation(&complex_turn, rands, trigs, NUM_LOOPS);
	printf("Timing for %d math.h sin and cos:\t%ldns\n", NUM_LOOPS, trig_time);

	rat_time = time_computation(&approx_turn, rands, rats, NUM_LOOPS);
	printf("Timing for %d approximations:\t%ldns\n", NUM_LOOPS, rat_time);

	long diff = rat_time - trig_time;
	double fracSpeed;
	if (diff > 0) {
		fracSpeed = rat_time / (double) trig_time;
		printf(STRRED "math.h" STRNORM " faster, speedup: %ldns (%2.2fx)\n", 
			diff, fracSpeed);
	} else {
		fracSpeed = trig_time / (double) rat_time;
		printf(STRGREEN "Approximation" STRNORM " faster, speedup: %ldns (%2.2fx)\n", 
			-diff, fracSpeed);
		print_errors(rands, trigs, rats, NUM_LOOPS);
	}

	return 0;
}
