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


//la solution de Cranck-Nicolson.
gsl_matrix *CN(double a, double b, double T,double kappa,int N,int M)
{
    double n,k,h,u,x,t;
    //Vecteur U(j)
	gsl_vector *U1; 
	U1 = gsl_vector_alloc(N+1);
	//Vecteur U(j+1)
    gsl_vector *U2;
	U2 = gsl_vector_alloc(N+1);
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
        u=sin(x*pi/b);//u(0,xi)
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
    
    gsl_matrix_scale(A1,-0.5*kappa*k/pow(h,2));
    gsl_matrix_scale(A2,+0.5*kappa*k/pow(h,2));
    gsl_matrix_add(A1,I);
    gsl_matrix_add(A2,I);

	//Inversion de la matrice I-kappa(k/h^2)*A 
    gsl_permutation *perm;
    perm = gsl_permutation_alloc(N+1);
    int s;
    gsl_linalg_LU_decomp(A2,perm,&s);
    gsl_linalg_LU_invert(A2,perm,InvA2);
    
    //Ajouter le vecteur U1 dans la première colonne de la matrice U
	gsl_matrix_set_col(U,0,U1);
	gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1,A1,InvA2,0,Prod);
    	
    for(int i=1;i<=M;i++)
    {	 
        gsl_blas_dgemv(CblasNoTrans,1,Prod,U1,0,U2);
        gsl_matrix_set_col(U, i, U2);
        gsl_vector_memcpy(U1,U2);
    }

    return U;
}

double sol(double t , double x, double b, double kappa ){
	return( sin(x*pi/b)*exp(-kappa*gsl_pow_2(pi/b)*t) );
}



int main()
{
	int i, j, k;
	double T = 1;
	double a = 0;
	double b = 20;
	double kappa =  0.5; 
	double M = 200;
	double N = 500;
    double dx=(b-a)/(double)(N);
    double dt=T/(double)(M);

cout<<kappa*dt/gsl_pow_2(dx)<<endl ; 
//La Solution de Cranck-Nicolson Pour fonction de Test : 
gsl_matrix *crank=CN(a,b,T, kappa ,N,M);
	printf("La Solution de Cranck-Nicolson TEST :\n\n");
	
	freopen("CranckTest.txt", "w", stdout);
	cout << "t" <<";"<< "x" <<";"<<"u(t,x)"<<";"<<"estim u(t,x)" << endl; 
	
	for(int i=0;i<=N;i++)//Affichage de la matrice
	{
		for(int j=0;j<=M;j++)
		{
			cout << j*dt <<";"<< a+i*dx <<";"<< sol(j*dt,a+i*dx,b,kappa) << ";" << gsl_matrix_get(crank,i,j) << endl; 
		}
	}
	// Restore the original standard output stream
	freopen("CONOUT$", "w", stdout);
	
	cout<<"Programme is done "<<endl;    

}


