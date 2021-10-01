#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

#define STRRED   "\x1b[31m" 
#define STRGREEN "\x1b[32m"
#define STRNORM  "\x1b[m"

struct circle {
	double c;
	double s;
};

double a = 8*M_SQRT2/3 - 3;
double b = 4 - 8*M_SQRT2/3;

void trig(double turn, struct circle *ret)
{
	double arg = M_PI * turn;
	ret->c = cos(arg);
	ret->s = sin(arg);
}

void rational(double turn, struct circle *ret)
{
	double p = turn*(b * turn * turn + a);
	double q = p*p;
	double r = 1 + q;
	double c = (1 - q) / r, s = 2*p / r;
	
	ret->c = c*c - s*s;
	ret->s = 2*c*s;
}

double errors(int n, const struct circle *circles1, const struct circle *circles2)
{
	double c_error, s_error;
	double largest_c_error, largest_s_error;

	double total_c_error = 0, total_s_error = 0;

	int i;
	struct circle circle1, circle2;

	for (i = 0; i < n; i++) {
		circle1 = circles1[i];
		circle2 = circles2[i];

		//squared error in c components
		c_error = circle1.c - circle2.c;
		c_error *= c_error;
		//squared error in s components
		s_error = circle1.s - circle2.s;
		s_error *= s_error;
		
		if (largest_c_error < c_error)
			largest_c_error = c_error;
		
		if (largest_s_error < s_error)
			largest_s_error = s_error;

		total_c_error += c_error;
		total_s_error += s_error;
	}
	//these now contain the *average* squared error
	total_c_error /= (double) n;
	total_s_error /= (double) n;

	printf("Squared error in cosines: \n\tAverage: %f (%f%% error)\n\tLargest: %f (%f%% error)\n"
		, total_c_error, sqrt(total_c_error) * 100
		, largest_c_error, sqrt(largest_c_error) * 100);
	printf("Squared error in sines: \n\tAverage: %f (%f%% error)\n\tLargest: %f (%f%% error)\n"
		, total_s_error, sqrt(total_s_error) * 100
		, largest_s_error, sqrt(largest_s_error) * 100);

	return 0;
}

int main(int argn, char **args)
{
	//struct circle ret;
	int i;

	struct timespec tp;
	long tick;
	long trig_time, rat_time;

	double rands[10000];
	struct circle trigs[10000];
	struct circle rats[10000];

	for (i = 0; i < 10000; i++) {
		rands[i] = rand() / (double) RAND_MAX;
	}

	clock_gettime(CLOCK_MONOTONIC, &tp);
	tick = tp.tv_nsec;
	for (i = 0; i < 10000; i++) {
		trig(rands[i], trigs+i);
	}
	clock_gettime(CLOCK_MONOTONIC, &tp);
	//this isn't quite proper, since the clock may have ticked over a second
	trig_time = tp.tv_nsec - tick;
	printf("Timing for 10000 math.h sin and cos:\t%ldns\n", trig_time);

	clock_gettime(CLOCK_MONOTONIC, &tp);
	tick = tp.tv_nsec;
	for (i = 0; i < 10000; i++) {
		rational(rands[i], rats+i);
	}
	clock_gettime(CLOCK_MONOTONIC, &tp);
	rat_time = tp.tv_nsec - tick;
	printf("Timing for 10000 approximations:\t%ldns\n", rat_time);

	double fracSpeed;
	long linSpeed = rat_time - trig_time;
	if (linSpeed > 0) {
		fracSpeed = rat_time / (double) trig_time;
		printf(STRRED "math.h" STRNORM " faster, speedup: %ldns (%2.2fx)\n", 
			linSpeed, fracSpeed);
	} else {
		fracSpeed = trig_time / (double) rat_time;
		printf(STRGREEN "Approximation" STRNORM " faster, speedup: %ldns (%2.2fx)\n", 
			-linSpeed, fracSpeed);
		errors(10000, rats, trigs);
	}
}
