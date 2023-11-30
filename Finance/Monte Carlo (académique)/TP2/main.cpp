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

#include <gsl/gsl_vector.h>

// Définition de Model Characteristics 
#define mu 0.078
#define vol 0.33
#define X0 140


double alpha(double t, double X){
	return mu*X;
}

double sigma(double t, double X){
	return vol*X;
}

double dsigma(double t, double X){
	return vol;
}

vector<double> Erreur(int n,double T,gsl_rng * r){
	double h = T/n; 
	double W=0; 
	double t = 0; 	
	double BS_exacte;
	double BS_Euler=X0;
	double BS_Milshtein=X0;
	double G;

	double Err_Euler=0;
	double Err_Milshtein=0;
	double Err; 
	for(int i=1;i<n;i++){
		G=gsl_ran_ugaussian(r);
	
//	C'est la simulation exacte à l'aide de Mvt Brownienne : 
		W = W + sqrt(h)*G;
		BS_exacte = X0*exp( vol*W + (mu-0.5*vol*vol)*t );

//	C'est la simulation exacte à l'aide de Schéma Euler : 		
		BS_Euler = BS_Euler +alpha(t,BS_Euler)*h+sigma(t,BS_Euler)*sqrt(h)*G;
		Err = gsl_pow_2( BS_Euler - BS_exacte );
		Err_Euler = GSL_MAX(Err,Err_Euler); 
		
//	C'est la simulation exacte à l'aide de Schéma Milshtein : 		
		BS_Milshtein = BS_Milshtein +( alpha(t,BS_Milshtein) -0.5*dsigma(t,BS_Milshtein)*sigma(t,BS_Milshtein) )*h+sigma(t,BS_Milshtein)*sqrt(h)*G + 0.5*dsigma(t,BS_Milshtein)*sigma(t,BS_Milshtein)*h*G*G;
		Err = gsl_pow_2( BS_Milshtein - BS_exacte );
		Err_Milshtein = GSL_MAX(Err,Err_Milshtein); 
// Incrémentation de Temps : 
		t=t+h; 		}
		std::vector<double> v ;
		v.push_back(h);v.push_back(Err_Euler);v.push_back(Err_Milshtein);
		return v;
		

}

vector<double> Generate(int n,double T,gsl_rng * r){
	double h = T/n; 
	double W=0; 
	double t = 0; 	
	double BS_exacte;
	double BS_Euler=X0;
	double BS_Milshtein=X0;
	double G;

// File Oppening :  
	freopen("Tragets.txt", "w", stdout);
	cout<<"time"<<" ; "<< "BS_exacte" <<" ; "<< "BS_Euler" <<" ; "<< "BS_Milshtein"<<endl ;  
	for(int i=1;i<n;i++){
		G=gsl_ran_ugaussian(r);
	
//	C'est la simulation exacte à l'aide de Mvt Brownienne : 
		W = W + sqrt(h)*G;
		BS_exacte = X0*exp( vol*W + (mu-0.5*vol*vol)*t );
		
//	C'est la simulation exacte à l'aide de Schéma Euler : 		
		BS_Euler = BS_Euler +alpha(t,BS_Euler)*h+sigma(t,BS_Euler)*sqrt(h)*G;
		
//	C'est la simulation exacte à l'aide de Schéma Milshtein : 		
		BS_Milshtein = BS_Milshtein +( alpha(t,BS_Milshtein) -0.5*dsigma(t,BS_Milshtein)*sigma(t,BS_Milshtein) )*h+sigma(t,BS_Milshtein)*sqrt(h)*G + 0.5*dsigma(t,BS_Milshtein)*sigma(t,BS_Milshtein)*h*G*G;

// Ecriture dans un fichier : 
		cout<< t <<" ; "<< BS_exacte <<" ; "<< BS_Euler <<" ; "<< BS_Milshtein<<endl ; 

// Incrémentation de Temps : 
		t=t+h; 		}

// Close the file when done writing
	fclose(stdout);

// Restore the original standard output stream
	freopen("CONOUT$", "w", stdout);
	cout<<"Done"<<endl; 		

}

int main(){
// RANDOM GENNERATOR :
gsl_rng * r = gsl_rng_alloc (gsl_rng_mt19937);
gsl_rng_env_setup();
gsl_rng_set(r,time(NULL));

int n=100000;
double T=1;

// To generate Tragets :
Generate(n,T,r); 

//// Simulation de  l'Erreur : 
	vector<double> v; 
	freopen("Erreurs.txt", "w", stdout);
	cout<<"Number"<<" ; "<< "BS_Euler" <<" ; "<< "BS_Milshtein"<<endl ;  
	
	for(int i=1000;i<6000; i++ ){
		v = Erreur(i,T,r); 
		cout<<v[0]<<" ; "<< v[1] <<" ; "<< v[2]<<endl ;  
	}

//// Close the file when done writing
//// Restore the original standard output stream
	fclose(stdout);
	freopen("CONOUT$", "w", stdout);
	cout<<"Done"<<endl; 

}
