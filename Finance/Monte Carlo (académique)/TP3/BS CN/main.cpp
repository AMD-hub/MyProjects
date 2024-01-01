#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <math.h>
#include <iostream>
using namespace std;
#include<time.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_sf_erf.h>
#include<gsl/gsl_rng.h>
#include<gsl/gsl_randist.h>
#include<gsl/gsl_statistics_int.h>
#include<gsl/gsl_statistics_double.h>
#include <gsl/gsl_cdf.h>
#define pi 3.14159265359


gsl_matrix *Call(double K, double r, double sigma, double a, double b, double TT,int N,int M)
{	
double T = sigma*sigma*TT/2; 
	double q = 2*r/(sigma*sigma); 
    double n,k,h,u,x,t,beta,alpha;
    //Vecteur U(j)
	gsl_vector *U1; 
	U1 = gsl_vector_alloc(N+1);
	//Vecteur U(j+1)
    gsl_vector *U2;
	U2 = gsl_vector_alloc(N+1);
	//Vecteur F(j)
	gsl_vector *F1; 
	F1 = gsl_vector_alloc(N+1);
	//Vecteur F(j+1)
	gsl_vector *F2; 
	F2 = gsl_vector_alloc(N+1);
    //Matrice de produit 
    gsl_matrix *Prod; 
	Prod = gsl_matrix_alloc(N+1,N+1);
    //Les matrices A1 et A2
	gsl_matrix *A1=gsl_matrix_alloc(N+1,N+1); 
    gsl_matrix *A2=gsl_matrix_alloc(N+1,N+1);
    gsl_matrix *I=gsl_matrix_alloc(N+1,N+1);
    gsl_matrix_set_identity(I);
    //Déclaration de la matrice inverse de A2
    gsl_matrix *InvA2= gsl_matrix_alloc(N+1,N+1);
	//La matrice U
	gsl_matrix *U;
	U=gsl_matrix_alloc(N+1,M+1);
    h=(b-a)/(double)(N);
    k=T/(double)(M);

    for(int i=0;i<=N;i++)//Remplissage de la matrice U(0)
    {
        x= i*h+a;
        u= K*(exp(x)-1);//u(0,xi)
        gsl_vector_set(U1,i,u);//Insérer u(0,xi) dans le vecteur U(0)
    }
	//Initialisation des matrices A1 et A2 par 0
    gsl_matrix_set_zero(A1); 
    gsl_matrix_set_zero(A2);
    //Remplissage des matrices A1 et A2
	for(int i=0;i<=N;i++)   
    {
        for(int j=0;j<=N;j++)
        {
            if(i==j){gsl_matrix_set(A1,i,j,2);gsl_matrix_set(A2,i,j,2);}
            if(i==j+1 || i==j-1){gsl_matrix_set(A1,i,j,-1);gsl_matrix_set(A2,i,j,-1);}
        }
    }
    
    gsl_matrix_scale(A1,-0.5*1*k/pow(h,2));
    gsl_matrix_scale(A2,+0.5*1*k/pow(h,2));
    gsl_matrix_add(A1,I);
    gsl_matrix_add(A2,I);

	//Inversion de la matrice I+(k/h^2)*A 
    gsl_permutation *perm;
    perm = gsl_permutation_alloc(N+1);
    int s;
    gsl_linalg_LU_decomp(A2,perm,&s);
    gsl_linalg_LU_invert(A2,perm,InvA2);
    
    //Ajouter le vecteur U1 dans la première colonne de la matrice U
	gsl_matrix_set_col(U,0,U1);
	gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1,A1,InvA2,0,Prod);
    	t= 0 ; 
    for(int i=1;i<=M;i++)
    {	
//    	Defining Fj
		gsl_vector_set_zero(F1); 
	    alpha =  0		;
	    beta  =	 exp(-0.5*b*(q+1) - 0.25*t*(q+1)*(q+1) );
		gsl_vector_set(F1,0,(1/gsl_pow_2(h))*alpha )    ; 
		gsl_vector_set(F1,N,(1/gsl_pow_2(h))*beta )   ; 
		t = t+k ; 	 
//		Defining Fj+1
		gsl_vector_set_zero(F2); 
	    alpha =  0		;
	    beta  =	 exp(-0.5*b*(q+1) - 0.25*t*(q+1)*(q+1) );
		gsl_vector_set(F2,0,(1/gsl_pow_2(h))*alpha )    ; 
		gsl_vector_set(F2,N,(1/gsl_pow_2(h))*beta )   ; 
//		Defining k*(Fj/2+Fj+1/2) 
		gsl_vector_add(F1,F2) ; 
		gsl_vector_scale(F1,k/2); 
		gsl_blas_dgemv(CblasNoTrans,1,InvA2,F1,0,F1);
//		Defining U2 = invA2*A1*(U1) +k*invA2*(Fj/2+Fj+1/2)
        gsl_blas_dgemv(CblasNoTrans,1,Prod,U1,0,U2);
        gsl_vector_add(U2,F1) ;
        gsl_matrix_set_col(U, i, U2);
        gsl_vector_memcpy(U1,U2);
    }

    return U;
}


double CallPrice(double theta, double S, double K,  double r, double vol){
	double std = vol*sqrt(theta) ; 
	double d1 = (log(S/K)+(r+vol*vol/2)*theta)/(std);
	double d2 = d1 - std; 
	return S*gsl_cdf_ugaussian_P(d1)-K*exp(-r*theta)*gsl_cdf_ugaussian_P(d2); 
}

double soly(double t, double x, double T,  double K,  double r, double vol){
		double q = 2*r/(vol*vol); 
		double domin =  K*exp(-0.5*(q-1)*x-0.25*(q+1)*(q+1)*t )   ;
	return(  CallPrice(2*t/(vol*vol), K*exp(x), K, r,vol)/domin  ) ; 
}



int main()
{
	 double T = 1;
	 double a = 0.01;
	 double b = 1;
	 double M = 200;
	 double N = 200;
	 double K = 1; 
	 double r = 0.06;
	 double sigma = 0.3; 
     double dx=(b-a)/(double)(N);
     double dt=T/(double)(M);
     
	gsl_matrix *haha=Call( K,  r,  sigma,  a,  b,  T, N, M);
	printf("La Solution de Cranck-Nicolson Black Scholes :\n\n");
	freopen("BS.txt", "w", stdout);
	cout << "t" <<";"<< "x" <<";"<<"y(t,x)"<<";"<<"estim y(t,x)"<< endl; 
	for(int i=0;i<=N;i++)//Affichage de la matrice
	{
		for(int j=0;j<=M;j++)
		{
			cout << j*dt <<";"<< a+i*dx <<";"<< soly(j*dt, a+i*dx, T,  K,  r , sigma) <<";"<<  gsl_matrix_get(haha,i,j) << endl; 
		}
	}
	// Restore the original standard output stream
	freopen("CONOUT$", "w", stdout);
	
	cout<<"Programme is done "<<endl;  

}


