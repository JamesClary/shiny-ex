[ PARAM ] @annotated
KA_pop    : 0.568  : KA for NHP infected
Tlag_pop  : 0.302  : Tlag for NHP
CLapp_pop : 2.827  : Cl for NHP infected
Vcapp_pop : 20.054 : VC for NHP
Qapp_pop  : 3.244  : Q for NHP
Vpapp_pop : 13.34  : VP for NHP

WTKG : 3.105 : Typical NHP
DOSE : 10    : DOSE administered (mg)
DOSEKG : 6    : DOSEKG administered (mg/kg)

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

double KA = KA_pop * pow((DOSEKG/10), 0.16) * exp(EKA);
double Tlag = Tlag_pop;
double CLapp = CLapp_pop * pow((DOSEKG/10), 0.093) * pow((WTKG/3.105), 0.75) * exp(ECL);
double Vcapp = Vcapp_pop* pow((DOSEKG/10), 0.623) * pow((WTKG/3.105), 1) * exp(EVc);
double Qapp = Qapp_pop * pow((WTKG/3.105), 0.75) * exp(EQ);
double Vpapp = Vpapp_pop * pow((WTKG/3.105), 1) * exp(EVp);

ALAG_GUT = Tlag;

double ke = CLapp/Vcapp;
double k12 = Qapp/Vcapp;
double k21 = Qapp/Vpapp;

[ ODE]
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT -(ke+k12)*CENT + k21*PERI;
dxdt_PERI = k12*CENT - k21*PERI;

[ TABLE ]
double Cc = CENT/Vcapp*1000;

[ CAPTURE ]
Cc
DOSE
CLapp
Vcapp
Qapp
Vpapp
