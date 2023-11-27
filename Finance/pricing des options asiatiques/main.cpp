#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <conio.h>
#include <math.h>
#include <gsl/gsl_sf_erf.h>
#include <time.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_statistics_int.h>
#include <gsl/gsl_statistics_double.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <cmath>

double KemnaVorst_put1(double S0, double K, double r, double sigma, double T)
{
if(S0>0)
{
//esperance et la variance de la variable Z
double Ez = log(S0)+(r-sigma*sigma*0.5)*0.5*T;
double Varz = (sigma*sigma*T)/3;
double d1 = (log(K)-Ez)/sqrt(Varz);
double d2 = d1 - sqrt(Varz);
return exp(-r*T)*(K*(1-gsl_sf_erf_Q(d1)) - exp(Ez+0.5*Varz) *(1- gsl_sf_erf_Q(d2)));
}
else
return 0;
}
double KemnaVorst_call1(double S0, double K, double r, double sigma, double T)
{
double ES = S0*(exp(r*T)-1)/(r*T) ;
return KemnaVorst_put1 (S0, K, r, sigma,T) + (ES-K)*exp(-r*T);
}


double max(double x,double y)
{
if (x<y) return y;
else return x;
}

double KemnaVorst_call2(double S, double K, double r, double sigma, double T, int n)
{
// variables initial
double sum = 0;
double Ez = log(S)+(r-sigma*sigma*0.5)*0.5*T;
double Varz = (sigma*sigma*T)/3;
//gsl_rng *r2;
//r2=gsl_rng_alloc(gsl_rng_taus);
// gsl_rng_env_setup();
//gsl_rng_set(r2,time(NULL));
float u1,u2,X;
// Boucle pour la simulation Monte Carlo
for (int i = 0; i < n; i++)
{
// Generer des v.a normales centr´ees r´eduites
u1=(float)rand()/RAND_MAX;
u2=(float)rand()/RAND_MAX;
if(u1>0)
X = sqrt(-2*log(u1)) * cos(M_PI*2*u2);
else continue;
//float X = gsl_ran_gaussian(r2,1);// g´en`ere une gaussienne en utilisant le generateur r2 ~ taus
// calculer la variable al´eatoire exp(Z)
double expZ = exp(X*sqrt(Varz)+Ez);
double expZn = exp(-X*sqrt(Varz)+Ez);
// Calculer le payoff de l'option
double payoff = max(expZ-K, 0.0) + max(expZn-K, 0.0);
// Cumuler le Payoff
sum += payoff;
}
// Calculate and return the option price
return (sum * exp(-r * T))/(2*n);
}


double KemnaVorst_put2(double S, double K, double r, double sigma, double T, int n)
{
// variables initial
double sum = 0;
double Ez = log(S)+(r-sigma*sigma*0.5)*0.5*T;
double Varz = (sigma*sigma*T)/3;
float u1,u2,X;
// Boucle pour la simulation Monte Carlo
for (int i = 0; i < n; i++)
{
// Generer des v.a normales centr´ees r´eduites
u1=(float)rand()/RAND_MAX;
u2=(float)rand()/RAND_MAX;
if(u1>0)
X = sqrt(-2*log(u1)) * cos(M_PI*2*u2);
else continue;
// calculer la variable al´eatoire exp(Z)
double expZ = exp(X*sqrt(Varz)+Ez);
double expZn = exp(-X*sqrt(Varz)+Ez);
// Calculer le payoff de l'option
double payoff = max(K-expZ, 0.0) + max(K-expZn, 0.0);
// Cumuler le Payoff
sum += payoff;
}
// Calculate and return the option price
return (sum * exp(-r * T))/(2*n);
}

// condition au limite
double v_T(double x) {
if (x > 0) {
return 0;
} else {
return -x;
}
}


double v_0(double t,double r,double T){
return (1-exp(-r*t))/(T*r);
}

gsl_matrix *implicit(double a, double b, double T,double r,double sigma,int N, int M)
{
double n,Delta_t,Delta_x,u,xi,t,beta_i,alpha_i,gamma_i,F_i;
float beta_1;
//Initialisation du vecteur U1 pour les valeurs de U(j)
gsl_vector *U1;
U1 = gsl_vector_alloc(N);
//Vecteur U2 pour stocker la valeur de U(j+1)
gsl_vector *U2;
U2 = gsl_vector_alloc(N);
//Initialisation de la matrice de solutions U
gsl_matrix *U;
U=gsl_matrix_alloc(N,M+1);
//La matrice A
gsl_matrix *A;
A=gsl_matrix_alloc(N,N);
//La matice d'identite
gsl_matrix *I;
I=gsl_matrix_alloc(N,N);
gsl_matrix_set_identity(I);
//Allouer la memoire pour la matrice inverse
gsl_matrix *invA;
invA= gsl_matrix_alloc(N,N);
//les pas
Delta_x = (b-a)/(double)(N+1);
Delta_t = T/(double)(M);
beta_1 = -((1/T +r*Delta_x)*Delta_x+pow(sigma*Delta_x,2));
//Remplissage du vecteur U(0)
for(int i=0;i<=N-1;i++)
{
xi= (i+1)*Delta_x+a;
u=v_T(xi);
gsl_vector_set(U1,i,u);
}
//Inserer le vecteur U1 dans la matrice U
gsl_matrix_set_col(U,0,U1);
//Initialisation de A
gsl_matrix_set_zero(A);
//Remplissage de la matrice A
for(int i=0;i<=N-1;i++)
{
xi = (i+1)*Delta_x+a;
alpha_i = (1/T + r*xi)*Delta_x - pow(sigma*xi,2);
beta_i = -((1/T +r*xi)*Delta_x + pow(sigma*xi,2));
gamma_i =2 * pow(sigma*xi,2);
for(int j=0;j<=N-1;j++)
{
//remplir la matrice A par les valeurs
if(i==j){gsl_matrix_set(A,i,j,gamma_i);}
if(i==j+1){gsl_matrix_set(A,i,j,beta_i);}
if(i==j-1){gsl_matrix_set(A,i,j,alpha_i);}
}
}
gsl_matrix_scale(A,Delta_t/(2*pow(Delta_x,2)));
//la somme de la matrice identit´e et A
gsl_matrix_add(A,I);
int s;
gsl_permutation *perm;
perm = gsl_permutation_alloc(N);
gsl_linalg_LU_decomp(A,perm,&s);
//Inversion de (I+(k/h^2)*A)
gsl_linalg_LU_invert(A,perm,invA);
//Inserer les valeurs
for(int i=1;i<=M;i++)
{
u = gsl_vector_get (U1,0);
F_i = -v_0(i*Delta_t,r,T)*(Delta_t)*beta_1/(2*pow(Delta_x,2)) + u;
gsl_vector_set(U1,0,F_i);
gsl_blas_dgemv(CblasNoTrans,1,invA,U1,0,U2);//U2 contient le produit Inv*U1
//Ins´erer le vecteur U2 dans la i-i`eme colonne de la matrice U
gsl_matrix_set_col(U,i,U2);
gsl_vector_memcpy(U1,U2);//Sauvegarder la valeur de U2 dans U1
}
return U;
}

double EDP_call3(double S, double K, double r, double sigma, double T, int N,int M,double a,double b ){
gsl_matrix * U; 
U = implicit(a,b,T,r,sigma,N,M);
int t=N; 

double floorResult = floor(M*K/(S*(b-a)));
int x = static_cast<int>(floorResult)-1;
return( S*exp(-r*T)*gsl_matrix_get(U,x,t) );
}
int main(){
printf("Les valeurs a l'aide d'approximation Kemna-Vorst\n"); 
printf("La valeur du Call de l'option asiatique est = %f\n",KemnaVorst_call1(120, 130, 0.1, 0.3,1));
printf("La valeur du Put  de l'option asiatique est = %f\n",KemnaVorst_put1(120, 130, 0.1, 0.3,1));
printf("Les valeurs a l'aide d'approximation Monte Carlo\n"); 
printf("La valeur du Call de l'option asiatique est = %f\n",KemnaVorst_call2(120,130,0.1,0.3,1,100000));
printf("La valeur du Put  de l'option asiatique est = %f",KemnaVorst_put2(120,130,0.1,0.3,1,100000));
printf("\nLes valeurs a l'aide d'approximation d'EDP \n"); 
printf("La valeur du Call de l'option asiatique est = %f\n",EDP_call3(120,130,0.1,0.3,1,1000,1000,0,20));
return 0;
getch();
}

