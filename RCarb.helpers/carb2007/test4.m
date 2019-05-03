clear
% Sensitivity test
handles.UB = 0.146; handles.UB_X = 0.001;
handles.TB = 0.027; handles.TB_X = 0.001;
handles.KB = 0.786; handles.KB_X = 0.008;
handles.UG = 0.113; handles.UG_X = 0.002;
handles.TG = 0.048; handles.TG_X = 0.002;
handles.KG = 0.245; handles.KG_X = 0.005;



load DATAek.txt; handles.DATAek = DATAek(2:end,2:end);
load DATAet.txt; handles.DATAet = DATAet(2:end,2:end);
load DATAeu.txt; handles.DATAeu = DATAeu(2:end,2:end);
load DATApk.txt; handles.DATApk = DATApk(2:end,2:end);
load DATApt.txt; handles.DATApt = DATApt(2:end,2:end);
load DATApu.txt; handles.DATApu = DATApu(2:end,2:end);
handles.WD = DATAek(2:end,ones(1,size(DATAek,1)-1));
handles.CD = handles.WD';
TIMEMAX1 = 10;   %ka
load mejdahl.txt; handles.MEJ = mejdahl;

fclose('all');
fid = fopen('test4.txt','r');
if fid > 0
    tline = [deblank(fgetl(fid)),' '];
    I = find((double(tline) == 9) | (double(tline) == 32));
    SAMP_NAME = cell(1);
    yes = 0;
    n = 0;
    while yes == 0
        n = n + 1;
        SAMP_NAME(n) = {tline(1:I(1)-1)};
        tline = tline(I(1):end);
        I = find((double(tline) ~= 9) & (double(tline) ~= 32));
        if isempty(I)
            yes = 1;
        else
            tline = tline(I(1):end);
        end
        I = find((double(tline) == 9) | (double(tline) == 32));
    end
end
n = n - 1;
SAMP_NAME = SAMP_NAME(2:end);

LINE = '[DATA] = textscan(fid,''%s';
for i = 1:n
    LINE = [LINE,'%f'];
end
LINE = [LINE,''');'];
eval(LINE);
VAR = char(DATA{1});
DATA = cell2mat(DATA(:,2:end));

for i = 1:size(VAR,1)
    eval([VAR(i,:),'=[',num2str(DATA(i,:)),'];']);
end
TIME = [0:STEP1:TIMEMAX1];

% % Open system
num = 1;
check = ~isempty(strmatch(['ONSET',num2str(num)],deblank(VAR),'exact'));
if check == 1
    while check
        eval(['ONSET(num) = ONSET',num2str(num),';']);
        eval(['WCI(num) = WCI', num2str(num),';']);
        eval(['WCF(num) = WCF', num2str(num),';']);
        eval(['FINISH(num) = FINISH', num2str(num),';']);
        eval(['CC(num)=CC', num2str(num),';']);
        num = num + 1;
        check = ~isempty(strmatch(['ONSET',num2str(num)],deblank(VAR),'exact'));
    end
end




%Step 1: Find U,Th,K values for sediment only (before carbonate dilution)
KA = K; UA = U; TA = T;
K = K.*(1+CC(1)/100);
U = U.*(1+CC(1)/100);
T = T.*(1+CC(1)/100);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Step 2: Derive carbonate model
LENUN=length(ONSET);
for J = 1:LENUN
    if J == 1
        t = 1;
    else 
        t = o;
    end
    f=find(TIME == FINISH(LENUN-J+1)); 
    o=find(TIME == ONSET(LENUN-J+1));
    %carb
    C1 = ones(f-t+1,1).*CC(LENUN-J+1);
    if J == LENUN
        C2 = linspace(CC(LENUN-J+1),0,o-f+1)';
    else
        C2 = linspace(CC(LENUN-J+1),CC(LENUN-J),o-f+1)';
    end
    if J == 1
        C = [C1;C2(2:end)];
    else
        C = [C;C1(2:end);C2(2:end)];
    end
    %water
    if J == 1
        WC1 = ones(f-t+1,1).*WCF(LENUN-J+1);
    else
        WC1 = linspace(WCI(LENUN-J+2),WCF(LENUN-J+1),f-t+1)';
    end
    WC2 = linspace(WCF(LENUN-J+1),WCI(LENUN-J+1),o-f+1)';
    if J == 1
        WC = [WC1;WC2(2:end)];
    else
        WC = [WC;WC1(2:end);WC2(2:end)];
    end
end
C = [C;zeros(length(TIME)-length(C),1)]./100+1E-5;
WC = [WC;zeros(length(TIME)-length(WC),1)+WCI(1)]./100+1E-5;
WF = C+WC;
LEN = length(C);

%Step 3: Derive linear uptake mode of uranium (code from Forbes Quarry work)

%Step 4: Calaculate time-dependent dose rate
MK = 1-exp(interp1(log(handles.MEJ(:,1)),log(handles.MEJ(:,2)),log(DIAM/1000)));
MT = 1-exp(interp1(log(handles.MEJ(:,1)),log(handles.MEJ(:,3)),log(DIAM/1000)));
MU = 1-exp(interp1(log(handles.MEJ(:,1)),log(handles.MEJ(:,4)),log(DIAM/1000)));

XKB = griddata(log(handles.WD),log(handles.CD),handles.DATAek,log(WC),log(C));
XTB = griddata(log(handles.WD),log(handles.CD),handles.DATAet,log(WC),log(C));
XUB = griddata(log(handles.WD),log(handles.CD),handles.DATAeu,log(WC),log(C));
XKG = griddata(log(handles.WD),log(handles.CD),handles.DATApk,log(WC),log(C));
XTG = griddata(log(handles.WD),log(handles.CD),handles.DATApt,log(WC),log(C));
XUG = griddata(log(handles.WD),log(handles.CD),handles.DATApu,log(WC),log(C));
XKB = griddata(log(handles.WD),log(handles.CD),handles.DATAek,log(WC),log(C));

DRKB = MK.*K.*handles.KB./(1 + XKB.*WF);
DRTB = MT.*T.*handles.TB./(1 + XTB.*WF);
DRUB = MU.*U.*handles.UB./(1 + XUB.*WF);
DRKG = 1.*K.*handles.KG./(1 + XKG.*WF);
DRTG = 1.*T.*handles.TG./(1 + XTG.*WF);
DRUG = 1.*U.*handles.UG./(1 + XUG.*WF);


DR = DRKB + DRTB + DRUB + DRKG + DRTG + DRUG + COSMIC + INTERNAL;

%Step 5: Calculate date
CUMDR = cumsum([0;(DR(1:end-1)+DR(2:end)).*STEP1./2]);
AGE = interp1(CUMDR,TIME',DE);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

DEN = DE;
DRN = DR;
CUMDRN = CUMDR;
AGEN = AGE;

