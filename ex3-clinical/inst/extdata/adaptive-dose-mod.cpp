[ PLUGIN ]
Rcpp
evtools
mrgx

[ ENV ] // R-code
DOSEQD = c(25, 50, 75, 100, 125)
x_int = 2
max_valQD = length(DOSEQD)

[ GLOBAL ] //for future Markov model
Rcpp::NumericVector DOSEQD;
Rcpp::NumericVector x_int;
Rcpp::NumericVector max_valQD;
double probs[5];
int states[5];

int findIndex(int *array, int size, int target)
{
  int i=0;
  while((i<size) && (array[i] != target)) i++;

  return(i<size) ? (i) : (-1);
}

// Pull from environment
[ PREAMBLE]
DOSEQD = mrgx::get<Rcpp::NumericVector>("DOSEQD", self);
x_int = mrgx::get<Rcpp::NumericVector>("x_int", self);
max_valQD = mrgx::get<Rcpp::NumericVector>("max_valQD", self);

[ PARAM ] @annotated
KA_pop    : 0.868  : KA for NHP uninfected
Tlag_pop  : 0.302  : Tlag for NHP
CLapp_pop : 2.809  : Cl for NHP uninfected
Vcapp_pop : 20.054 : VC for NHP
Qapp_pop  : 3.244  : Q for NHP
Vpapp_pop : 13.34  : VP for NHP


WTKG : 60 : Typical NHP

[ OMEGA ] @annotated
EKA : 0.012 : Eta on KA
ECL : 0.092 : Eta on CL
EVc : 0.200 : Eta on Vc
EQ  : 0.446 : Eta on Q
EVp : 0.264 : Eta on Vp

[ CMT ] @annotated
GUT  : Gut compartment
CENT : Central compartment
PERI : Peripheral compartment

[ MAIN ]

if(NEWIND<=1){

  double prob0 = 0.96;
  double prob1 = 0.01;
  double prob2 = 0.01;
  double prob3 = 0.01;
  double prob4 = 1-(0.96+0.01+0.01+0.01);


  probs[0] = prob0;
  probs[1] = prob1;
  probs[2] = prob2;
  probs[3] = prob3;
  probs[4] = prob4;

  double dose_ind = 0;

  int DOSE = DOSEQD[0];
  double state_dose = 0;
  double pstate_dose = 0;

  double DOSEKG = DOSE/WTKG;

  double dosetime = 0;
  double dtime = 0;
}

double KA = KA_pop * pow((DOSEKG/10), 0.16) * exp(EKA);
double Tlag = Tlag_pop;
double CLapp = CLapp_pop * pow((DOSEKG/10), 0.093) * pow((WTKG/3.105), 0.75) * exp(ECL);
double Vcapp = Vcapp_pop* pow((DOSEKG/10), 0.623) * pow((WTKG/3.105), 1) * exp(EVc);
double Qapp = Qapp_pop * pow((WTKG/3.105), 0.75) * exp(EQ);
double Vpapp = Vpapp_pop * pow((WTKG/3.105), 1) * exp(EVp);

ALAG_GUT = Tlag;
F_GUT = DOSE;

double ke = CLapp/Vcapp;
double k12 = Qapp/Vcapp;
double k21 = Qapp/Vpapp;

[ ODE]
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT -(ke+k12)*CENT + k21*PERI;
dxdt_PERI = k12*CENT - k21*PERI;
double Cc = CENT/Vcapp*1000;

double E = 100 * (1 - Cc/(200+Cc));

dtime = SOLVERTIME - dosetime;

if(SOLVERTIME > 0 && fmod(SOLVERTIME, 24.)<=0 && dtime !=0){

  double lgpincr = -2.2;
  double lgpdecr = -2.2;
  double lgpincr2 = -2.9;
  double pincr2 = exp(lgpincr2)/(1+exp(lgpincr2));
  double lgpincr3 = -3.47;
  double pincr3 = exp(lgpincr3)/(1+exp(lgpincr3));
  double lgpincr4 = -4.59;
  double pincr4 = exp(lgpincr4)/(1+exp(lgpincr4));
  double lgpdecr2 = -2.9;
  double pdecr2 = exp(lgpdecr2)/(1+exp(lgpdecr2));
  double lgpdecr3 = -3.47;
  double pdecr3 = exp(lgpdecr3)/(1+exp(lgpdecr3));
  double lgpdecr4 = -4.59;
  double pdecr4 = exp(lgpdecr4)/(1+exp(lgpdecr4));

  double hieff_fl = 0;
  if(E > 60) hieff_fl = 1;
  double hieff = 3.2;
  double pincr = exp(lgpincr+hieff*hieff_fl)/(1+exp(lgpincr+hieff*hieff_fl));

  double loeff_fl = 0;
  if(E < 30) loeff_fl = 1;
  double loeff =3.2;
  double pdecr = exp(lgpdecr+loeff*loeff_fl)/(1+exp(lgpdecr+loeff*loeff_fl));

  if (dose_ind == 0){
    prob1 = pincr;
    prob2 = pincr2;
    prob3 = pincr3;
    prob4 = pincr4;
    prob0 = 1-(prob1+prob2+prob3+prob4);
  } else if (dose_ind == 1){
    prob0 = pdecr;
    prob2 = pincr;
    prob3 = pincr2;
    prob4 = pincr3;
    prob1 = 1-(prob0+prob2+prob3+prob4);
  }  else if (dose_ind == 2){
    prob0 = pdecr2;
    prob1 = pdecr;
    prob3 = pincr;
    prob4 = pincr2;
    prob2 = 1-(prob0+prob1+prob3+prob4);
  }  else if (dose_ind == 3){
    prob0 = pdecr3;
    prob1 = pdecr2;
    prob2 = pdecr;
    prob4 = pincr;
    prob3 = 1-(prob0+prob1+prob2+prob4);
  } else if (dose_ind == 4){
    prob0 = pdecr4;
    prob1 = pdecr3;
    prob2 = pdecr2;
    prob3 =  pdecr;
    prob4 = 1-(prob0+prob1+prob2+prob3);
  }

  probs[0] = prob0;
  probs[1] = prob1;
  probs[2] = prob2;
  probs[3] = prob3;
  probs[4] = prob4;

  R::rmultinom(1, probs, 5, states);
  dose_ind = findIndex(states, 5, 1);
  pstate_dose = state_dose;
  state_dose = dose_ind;
  DOSE = DOSEQD[dose_ind];
  DOSEKG = DOSE/WTKG;
  dosetime = SOLVERTIME;

}

[ CAPTURE ]
Cc
E
DOSE
dose_ind
pstate_dose
state_dose
prob0
prob1
prob2
prob3
prob4
EVID
