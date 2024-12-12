#pragma once
#define _XOPEN_SOURCE

const int N 		= 128;			// domain size
const int M			= 50000;		// number of time steps
const double uhi	= 0.5;
const double ulo	=-0.5;
const double vhi	= 0.1;
const double vlo 	=-0.1;
const double a 	= 0.3;			// model parameter a
const double b 	= 0.1;			// model parameter b
const double c 	= 0.01;			// model parameter c
const double d 	= 0.0;			// model parameter d
const double Ic	= 0.03;
const double R		= 1.0;
const double dt	= 0.1;			// time step
const double dx	= 2.0;			// spatial resolution
const double DD 	= 1.0/(dx*dx);	// diffusion scaling
const int m			= 100;			// Norm calculation period

void init(double u[N][N], double v[N][N]);

void step(double du[N][N], double dv[N][N], double u[N][N], double v[N][N]);

void dxdt(double du[N][N], double dv[N][N], double u[N][N], double v[N][N]);

double norm(double x[N][N]);

double stim(int i, int j){
	 if ((i < 10) && (j < 10)){
	 	return Ic;
	 }else{
	 	return 0.0;
	 }
}

double f(double u, double v){
	return u*(1.0 - u)*(u - b) - v;
}

double g(double u, double v){
	return c*(a*u - v);
}