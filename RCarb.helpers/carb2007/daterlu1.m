function [ABS] = daterlu1(TIMEMAX)

disp(TIMEMAX); 

load data1.mat
%Step 2: Derive carbonate model
%C = [ones(FINISH./STEP1,1).*CC/100;linspace(CC/100,1E-5,(ONSET-FINISH)/STEP1+1)';zeros((round(TIMEMAX*STEP1)/STEP1-ONSET)/STEP1,1)+1E-5];
%WC = [ones(FINISH./STEP1,1).*WCF/100;linspace(WCF/100,WCI/100,(ONSET-FINISH)/STEP1+1)';ones((round(TIMEMAX*STEP1)/STEP1-ONSET)/STEP1,1).*WCI/100];
C = [ones(FINISH./STEP1,1).*CC/100;linspace(CC/100,1E-5,(ONSET-FINISH)/STEP1+1)';zeros((handles.TIMEMAX1-ONSET)/STEP1,1)+1E-5];
WC = [ones(FINISH./STEP1,1).*WCF/100;linspace(WCF/100,WCI/100,(ONSET-FINISH)/STEP1+1)';ones((handles.TIMEMAX1-ONSET)/STEP1,1).*WCI/100];
WF = C+WC;
WFA = WCF/100;
LEN = length(C);
%TIME = [0:STEP1:round(TIMEMAX*STEP1)/STEP1]'; TIME = [zeros(handles.TIMEMAX1-max(TIME),1);TIME];
TIME = [0:STEP1:handles.TIMEMAX1'];
TIME_ = [0:STEP1:round(TIMEMAX*STEP1)/STEP1]'; TIME_ = [zeros(handles.TIMEMAX1-max(TIME_),1);TIME_];
%Step 3: Derive linear uptake mode of uranium (code from Forbes Quarry work)  
lam_u235 = log(2)/703800000;
lam_u238 = log(2)/4.4680e+009;
[Aa] = rad_pop_LU(U234_U238,TIME_);
Aa = flipud(Aa);
N_u238 = Aa(:,1); N_u234 = Aa(:,2); N_t230 = Aa(:,3); N_u235 = Aa(:,4); N_p231 = Aa(:,5);
%1) Parent activity (Bq/mg)
A_u238 = lam_u238.*6.022E23.*1E-3./(238.*31.56E6);     %from A&A (1998)
A_u235 = lam_u235.*6.022E23.*1E-3./(235.*31.56E6);     %from A&A (1998)
%2) dose rate per ppm of parent
conv_const = 5.056E-3;
CONST_Q_U238 = 0.9927; CONST_Q_U235 = 1 - CONST_Q_U238;
D_b_u238 = conv_const.*A_u238.*0.8860.*CONST_Q_U238;
D_b_u234 = conv_const.*A_u238.*0.0120.*CONST_Q_U238;
D_b_t230 = conv_const.*A_u238.*1.385.*CONST_Q_U238;
%D_b_u235 = conv_const.*A_u235.*0.1860.*CONST_Q_U235;
%D_b_p231 = conv_const.*A_u235.*1.1090.*CONST_Q_U235;
D_g_u238 = conv_const.*A_u238.*0.0290.*CONST_Q_U238;
D_g_u234 = conv_const.*A_u238.*0.0020.*CONST_Q_U238;
D_g_t230 = conv_const.*A_u238.*1.7430.*CONST_Q_U238;
%D_g_u235 = conv_const.*A_u235.*0.2070.*CONST_Q_U235;
%D_g_p231 = conv_const.*A_u235.*0.4540.*CONST_Q_U235;
%3) i-m dose rate - no grain size correction
U238_b_diseq = D_b_u238.*N_u238.*U238;
U234_b_diseq = D_b_u234.*N_u234.*U238;
T230_b_diseq = D_b_t230.*N_t230.*U238;
%U235_b_diseq = D_b_u235.*N_u235.*U238;
%P231_b_diseq = D_b_p231.*N_p231.*U238;
U238_g_diseq = D_g_u238.*N_u238.*U238;
U234_g_diseq = D_g_u234.*N_u234.*U238;
T230_g_diseq = D_g_t230.*N_t230.*U238;
%U235_g_diseq = D_g_u235.*N_u235.*U238;
%P231_g_diseq = D_g_p231.*N_p231.*U238;


%Step 4: Calaculate time-dependent dose rate
MK = 1-exp(interp1(log(handles.MEJ(:,1)),log(handles.MEJ(:,2)),log(DIAM/1000)));
MT = 1-exp(interp1(log(handles.MEJ(:,1)),log(handles.MEJ(:,3)),log(DIAM/1000)));
MU = 1-exp(interp1(log(handles.MEJ(:,1)),log(handles.MEJ(:,4)),log(DIAM/1000)));
MU238 = 1; MU234 = 1; MT230 = 1; MU235 = 1; MP231 = 1;

XKB = griddata(log(handles.WD),log(handles.CD),handles.DATAek,log(WC),log(C));
XTB = griddata(log(handles.WD),log(handles.CD),handles.DATAet,log(WC),log(C));
XUB = griddata(log(handles.WD),log(handles.CD),handles.DATAeu,log(WC),log(C));
XKG = griddata(log(handles.WD),log(handles.CD),handles.DATApk,log(WC),log(C));
XTG = griddata(log(handles.WD),log(handles.CD),handles.DATApt,log(WC),log(C));
XUG = griddata(log(handles.WD),log(handles.CD),handles.DATApu,log(WC),log(C));
XKB = griddata(log(handles.WD),log(handles.CD),handles.DATAek,log(WC),log(C));
XU238B = griddata(log(handles.WD),log(handles.CD),handles.DATAeu238,log(WC),log(C));
XU234B = griddata(log(handles.WD),log(handles.CD),handles.DATAeu234,log(WC),log(C));
XT230B = griddata(log(handles.WD),log(handles.CD),handles.DATAet230,log(WC),log(C));
%XU235B = griddata(log(handles.WD),log(handles.CD),handles.DATAeu235,log(WC),log(C));
%XP231B = griddata(log(handles.WD),log(handles.CD),handles.DATAep231,log(WC),log(C));
XU238G = griddata(log(handles.WD),log(handles.CD),handles.DATApu238,log(WC),log(C));
XU234G = griddata(log(handles.WD),log(handles.CD),handles.DATApu234,log(WC),log(C));
XT230G = griddata(log(handles.WD),log(handles.CD),handles.DATApt230,log(WC),log(C));
%XU235G = griddata(log(handles.WD),log(handles.CD),handles.DATApu235,log(WC),log(C));
%XP231G = griddata(log(handles.WD),log(handles.CD),handles.DATApp231,log(WC),log(C));

XKBA = 1.25;
XTBA = 1.25;
XUBA = 1.25;
XU238BA = 1.25;
XU234BA = 1.25;
XT230BA = 1.25;
%XKU235A = 1.25;
%XKP231A = 1.25;
XKGA = 1.14;
XTGA = 1.14;
XUGA = 1.14;
XU238GA = 1.14;
XU234GA = 1.14;
XT230GA = 1.14;
%XU235GA = 1.14;
%XP231GA = 1.14;

DRKB = MK.*K.*handles.KB./(1 + XKB.*WF);
DRTB = MT.*T.*handles.TB./(1 + XTB.*WF);
DRUB = MU.*U.*handles.UB./(1 + XUB.*WF);
DRKG = MK.*K.*handles.KG./(1 + XKG.*WF);
DRTG = MT.*T.*handles.TG./(1 + XTG.*WF);
DRUG = MU.*U.*handles.UG./(1 + XUG.*WF);

DRU238B = MU238.*U238_b_diseq./(1 + XU238B.*WF);
DRU234B = MU238.*U234_b_diseq./(1 + XU234B.*WF);
DRT230B = MU238.*T230_b_diseq./(1 + XT230B.*WF);
%DRU235B = MU238.*U235_b_diseq./(1 + XU235B.*WF);
%DRP231B = MU238.*P231_b_diseq./(1 + XP231B.*WF);
DRU238G = MU238.*U238_b_diseq./(1 + XU238B.*WF);
DRU234G = MU238.*U234_b_diseq./(1 + XU234B.*WF);
DRT230G = MU238.*T230_b_diseq./(1 + XT230B.*WF);
%DRU235G = MU238.*U235_b_diseq./(1 + XU235B.*WF);
%DRP231G = MU238.*P231_b_diseq./(1 + XP231B.*WF);

DRKBA = MK.*KA.*handles.KB./(1 + XKBA.*WFA);
DRTBA = MT.*TA.*handles.TB./(1 + XTBA.*WFA);
DRUBA = MU.*UA.*handles.UB./(1 + XUBA.*WFA);
DRU238BA = MU238.*U238_b_diseq./(1 + XU238BA.*WFA);
DRU234BA = MU238.*U234_b_diseq./(1 + XU234BA.*WFA);
DRT230BA = MU238.*T230_b_diseq./(1 + XT230BA.*WFA);
%DRU235BA = MU238.*U235_b_diseq./(1 + XU235BA.*WFA);
%DRP231BA = MU238.*P231_b_diseq./(1 + XP231BA.*WFA);
DRKGA = MK.*KA.*handles.KG./(1 + XKGA.*WFA);
DRTGA = MT.*TA.*handles.TG./(1 + XTGA.*WFA);
DRUGA = MU.*UA.*handles.UG./(1 + XUGA.*WFA);
DRU238GA = MU238.*U238_g_diseq./(1 + XU238GA.*WFA);
DRU234GA = MU238.*U234_g_diseq./(1 + XU234GA.*WFA);
DRT230GA = MU238.*T230_g_diseq./(1 + XT230GA.*WFA);
%DRU235GA = MU238.*U235_g_diseq./(1 + XU235GA.*WFA);
%DRP231GA = MU238.*P231_g_diseq./(1 + XP231GA.*WFA);

%DR = DRKB + DRTB + DRUB + DRKG + DRTG + DRUG + COSMIC + INTERNAL + DRU238B + DRU234B + DRT230B + DRU235B + DRP231B + DRU238G + DRU234G + DRT230G + DRU235G + DRP231G;
%DRA = DRKBA + DRTBA + DRUBA + DRKGA + DRTGA + DRUGA + COSMIC + INTERNAL + DRU238BA + DRU234BA + DRT230BA + DRU235BA + DRP231BA + DRU238GA + DRU234GA + DRT230GA + DRU235GA + DRP231GA;
DR = DRKB + DRTB + DRUB + DRKG + DRTG + DRUG + COSMIC + INTERNAL + DRU238B + DRU234B + DRT230B + DRU238G + DRU234G + DRT230G;
DRA = DRKBA + DRTBA + DRUBA + DRKGA + DRTGA + DRUGA + COSMIC + INTERNAL + DRU238BA + DRU234BA + DRT230BA + DRU238GA + DRU234GA + DRT230GA;

%Step 5: Calculate date
CUMDR = cumsum([0;(DR(1:end-1)+DR(2:end)).*STEP1./2]);
CUMDRA = cumsum([0;(DRA(1:end-1)+DRA(2:end)).*STEP1./2]);
if DE > max(CUMDR)
    msgbox('ERROR: Dates greater than 500ka!')
    return
end
AGE = interp1(CUMDR,TIME,DE);
AGEA = interp1(CUMDRA,TIME,DE);
ABS = abs(AGE-TIMEMAX);

save data2.mat
