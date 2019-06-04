/* 
   ADinf.c

   $Revision: 1.1 $  $Date: 2014/06/09 10:18:10 $

   Original C code by G. and J. Marsaglia

   R interface by Adrian Baddeley

*/

#include <Rmath.h>

double  ADinf(double z);

/*
  A procedure for evaluating the limiting distribution of the
  Anderson-Darling statistic 
  A_n=-n-(1/n)[ln(x_1(1-x_n)+3ln(x_2(1-x_{n-1})+5ln(x_3(1-x_{n-2})+...
       +(2n-1)ln(x_n(1-x_1))]
  where x_1<x_2<...<x_n is an ordered set of purported uniform [0,1) variates.
  The function is ADinf(z)=lim_{n->infty} Pr[A_n<z]. About 15 digit accuracy.
  If you don't need that much accuracy, use the quick-and-easy adinf(z).
  ADinf uses a two-term recursion for coefficients in series
  for which initial values require the complementary normal integral, 
  included as cPhi(z). Otherwise, use erfc() if your C compiler has one with
  adequate accuracy.
*/

double cPhi(double z); /* prototype; listing follows main */

double ADf(double z,int j){ 
  /* called by ADinf(); see article. */
  double t,f,fnew,a,b,c,r;
  int i;
  t=(4*j+1)*(4*j+1)*1.23370055013617/z;
  if(t>150.) return 0.;
  a=2.22144146907918*exp(-t)/sqrt(t);
  b=3.93740248643060*2.*cPhi(sqrt(2*t));/* initialization requires cPhi */
  /*if you have erfc(), replace 2*cPhi(sqrt(2*t)) with erfc(sqrt(t))*/
  r=z*.125; f=a+b*r;
  for(i=1;i<200;i++) {
    c=((i-.5-t)*b+t*a)/i;
    a=b; b=c; r*=z/(8*i+8);
    if(fabs(r)<1e-40 || fabs(c)<1.e-40) return f;
    fnew=f+c*r;
    if(f==fnew) return f;
    f=fnew;            
  }
  return f;				
}

double ADinf(double z){
  int j;
  double ad,adnew,r;
  if(z<.01) return 0.; /* avoids exponent limits; ADinf(.01)=.528e-52 */
  r=1./z;
  ad=r*ADf(z,0);
  for(j=1;j<100;j++){
    r*=(.5-j)/j;
    adnew=ad+(4*j+1)*r*ADf(z,j);
    if(ad==adnew) {return ad;}
    ad=adnew;         
  }
  return ad;
}

/*
  Complementary normal distribution function
  cPhi(x) = integral from x to infinity of phi(x)=exp(-.5*t^2)/sqrt(2*pi)
  13-15 digit accuracy for abs(x)<16.
  Stores R(0),R(2),R(4),...,R(16), with cPhi(x)=R(x)*phi(x), phi normal density,
  then uses Taylor series for 
  R(z+h)=R(z)+hR'(z)+(1/2)h^2R''(z)+...
  with -1<h<1, and R(z) one of R(0),R(2),R(4),...,R(16) 
  stored as v[0],v[1],...v[8].
  Examples: cPhi(2.75) needs R(2.75) and 2.75=2+.75 so use h=.75 and R(2)=v[1],
            cPhi(3.3)  needs R(3.3) and 3.3=4-.7, so use h=-.7 and R(4)=v[2].
*/
double cPhi(double x){
  long double v[]={
    1.25331413731550025,  .421369229288054473,  .236652382913560671,
    .162377660896867462,  .123131963257932296,  .0990285964717319214,
    .0827662865013691773, .0710695805388521071, .0622586659950261958
  };
  double h,a,b,z,t,s,pwr;
  int i,j;
  j=(fabs(x)+1.)/2.;
  a=v[j];    z=2*j;  h=fabs(x)-z;
  b=z*a-1;   pwr=1;  s=a+h*b;
  for(i=2;i<100;i+=2){/* begin i loop */
    a=(a+z*b)/i;
    b=(b+z*a)/(i+1);
    pwr=pwr*h*h;
    t=s;
    s=s+pwr*(a+h*b);
    if(s==t){ 
      s*=exp(-.5*x*x-.91893853320467274178);
      return ((x>0) ? s: 1-s);
    }
  } /* end i loop */
  /* If not converged, return last estimate */
  return ((x>0) ? s: 1-s);
}

/* R interface */

void ADprobExactInf(double *a, int *na, double *prob) {
  int i, m;
  m = *na;
  for(i = 0; i < m; i++) 
    prob[i] = ADinf(a[i]);
}
