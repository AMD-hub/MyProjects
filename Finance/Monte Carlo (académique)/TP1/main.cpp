#include<vector>
using namespace std;
#include <stdio.h>
#include <iostream>
#include <math.h>
#include <time.h>

#include <gsl/gsl_rng.h>
#include <gsl/gsl_cdf.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_math.h>




double Call(double theta, double S, double K,  double r, double vol){
	double std = vol*sqrt(theta) ; 
	double d1 = (log(S/K)+(r+vol*vol/2)*theta)/(std);
	double d2 = d1 - std; 
	return S*gsl_cdf_ugaussian_P(d1)-K*exp(-r*theta)*gsl_cdf_ugaussian_P(d2); 
}

vector<double> Naive(double theta, double S, double K,  double r, double vol, int n,gsl_rng * omega){

double lambda = S*exp((r-vol*vol/2)*theta); 
double Mean ;
double Variance ;
Variance=0;
Mean=0;

double stock; 
for(int i=1; i<n;i++){
	double G = gsl_ran_gaussian(omega,sqrt(theta)); 
	double Payoff = exp(-r*theta)*GSL_MAX(0, lambda*exp(vol*G)-K); 
	stock = (Mean*i+Payoff)/(i+1); 
	Variance = Variance + gsl_pow_2(Mean) - gsl_pow_2(stock) + (gsl_pow_2(Payoff)- Variance - gsl_pow_2(stock) )/i; 
	Mean = stock; 
}

vector<double> v; 
v.push_back(Mean); v.push_back(Variance);
return v;

	
}

vector<double> Controle(double theta, double S, double K,  double r, double vol, int n,gsl_rng * omega){

double lambda = S*exp((r-vol*vol/2)*theta); 
double Mean ;
double Variance ;
Variance=0;
Mean=0;

double stock; 
for(int i=1; i<n;i++){
	double G = gsl_ran_gaussian(omega,sqrt(theta)); 
	double Payoff = exp(-r*theta)*GSL_MAX(0, K-lambda*exp(vol*G)) + S -K*exp(-r*theta);  
	stock = (Mean*i+Payoff)/(i+1); 
	Variance = Variance + gsl_pow_2(Mean) - gsl_pow_2(stock) + (gsl_pow_2(Payoff)- Variance - gsl_pow_2(stock) )/i; 
	Mean = stock; 
}

vector<double> v; 
v.push_back(Mean); v.push_back(Variance);
return v;

	
}

vector<double> Anthetic(double theta, double S, double K,  double r, double vol, int n,gsl_rng * omega){

double lambda = S*exp((r-vol*vol/2)*theta); 
double Mean ;
double Variance ;
Variance=0;
Mean=0;

double stock; 
for(int i=1; i<n;i++){
	double G = gsl_ran_gaussian(omega,sqrt(theta)); 
	double Payoff = exp(-r*theta)*0.5*( GSL_MAX(0,  lambda*exp(vol*G) - K ) +GSL_MAX(0,  lambda*exp(-vol*G)  - K ) ) ;
 
	stock = (Mean*i+Payoff)/(i+1); 
	Variance = Variance + gsl_pow_2(Mean) - gsl_pow_2(stock) + (gsl_pow_2(Payoff)- Variance - gsl_pow_2(stock) )/i; 
	Mean = stock; 
}

vector<double> v; 
v.push_back(Mean); v.push_back(Variance);
return v;

	
}

vector<double> AnthCont(double theta, double S, double K,  double r, double vol, int n,gsl_rng * omega){

double lambda = S*exp((r-vol*vol/2)*theta); 
double Mean ;
double Variance ;
Variance=0;
Mean=0;

double stock; 
for(int i=1; i<n;i++){
	double G = gsl_ran_gaussian(omega,sqrt(theta)); 
	double Payoff = exp(-r*theta)*(GSL_MAX(0, K- lambda*exp(vol*G) ) + GSL_MAX(0, K- lambda*exp(-vol*G) ) )/2 + S -K*exp(-r*theta); 
	stock = (Mean*i+Payoff)/(i+1); 
	Variance = Variance + gsl_pow_2(Mean) - gsl_pow_2(stock) + (gsl_pow_2(Payoff)- Variance - gsl_pow_2(stock) )/i; 
	Mean = stock; 
}

vector<double> v; 
v.push_back(Mean); v.push_back(Variance);
return v;

	
}

int main(){
vector<double> v; 

// RANDOM GENNERATOR :
gsl_rng * r = gsl_rng_alloc (gsl_rng_mt19937);
gsl_rng_env_setup();
gsl_rng_set(r,time(NULL));

// VARIABLES :
double theta = 1;
double S = 100; 
double K = 110 ; 
double rho = 0.054;
double vol = 0.25;

// EXACT FORMULA :
double CallPrice = Call(theta, S, K, rho,vol); 


// Simaltion pour differente n : 

int n = 4000;
vector<double> NaiveV; 
vector<double> ControleV; 
vector<double> AntheticV; 
vector<double> AnthContV; 


freopen("ComparingMethodes.txt", "w", stdout);
cout<<"Number"<<" ; "<< "Black_Formula" <<" ; "<< "Naive_MC" <<" ; "<< "Controle_MC" <<" ; "<< "Anthetique_MC"<<" ; "<< "Anthetique_Controle_MC" <<" ; ";
cout<< "Naive_Err" <<" ; "<< "Controle_Err" <<" ; "<< "Anthetique_Err"<<" ; "<< "Anthetique_Controle_Err" <<endl;

for(int i=1; i<n;i++){
	NaiveV 	 = Naive(theta, S, K, rho,vol,i,r) ;
	ControleV = Controle(theta, S, K, rho,vol,i,r);
	AntheticV = Anthetic(theta, S, K, rho,vol,i,r); 
	AnthContV = AnthCont(theta, S, K, rho,vol,i,r);
	
	cout<<i<<" ; "<< CallPrice <<" ; "<< NaiveV[0] <<" ; "<< ControleV[0] <<" ; "<< AntheticV[0]<<" ; "<< AnthContV[0] <<" ; " ;
	cout<< NaiveV[1] <<" ; "<< ControleV[1] <<" ; "<< AntheticV[1]<<" ; "<< AnthContV[1] <<endl;

}

// Close the file when done writing
fclose(stdout);

// Restore the original standard output stream
freopen("CONOUT$", "w", stdout);

cout<<"LES PRICES SELON LES DIFFERENTES METHODE : "<<endl;
cout<<" FORMULE B && S EXACTE :  "<< CallPrice <<endl <<" MONTE CALRO NAIVE : "<< NaiveV[0] <<endl <<" MONTE CARLO CONTROL VARIABLE : "<< ControleV[0]  <<endl<<" MONTE CARLO VARIABLE ANTHETIQUE :  "<< AntheticV[0] <<endl<<" MONTE CARLO CONTROL + ANTHETIQUE : "<< AnthContV[0] <<endl;

}
