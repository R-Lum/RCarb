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
handles.TIMEMAX1 = 10;   %ka
load mejdahl.txt; handles.MEJ = mejdahl;

fclose('all');
fid = fopen('test2.txt','r');
if fid > 0
    tline = [deblank(fgetl(fid)),' '];
    I = find((double(tline) == 9) | (double(tline) == 32));
    handles.SAMP_NAME = cell(1);
    yes = 0;
    n = 0;
    while yes == 0
        n = n + 1;
        handles.SAMP_NAME(n) = {tline(1:I(1)-1)};
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
handles.SAMP_NAME = handles.SAMP_NAME(2:end);

LINE = '[DATA] = textscan(fid,''%s';
for i = 1:n
    LINE = [LINE,'%f'];
end
LINE = [LINE,''');'];
eval(LINE);
handles.VAR = char(DATA{1});
handles.DATA = cell2mat(DATA(:,2:end));

for i = 1:size(handles.VAR,1)
    eval([handles.VAR(i,:),'=[',num2str(handles.DATA(i,:)),'];']);
end

%Step 1: Find U,Th,K values for sediment only (before carbonate dilution)
KA = K; UA = U; TA = T;
K = K.*(1+CC/100);
U = U.*(1+CC/100);
T = T.*(1+CC/100);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Step 2: Derive carbonate model
C = [ones(FINISH./STEP1,1).*CC/100;linspace(CC/100,1E-2,(ONSET-FINISH)/STEP1+1)';zeros((handles.TIMEMAX1-ONSET)/STEP1,1)+1E-2];
WC = [ones(FINISH./STEP1,1).*WCF/100;linspace(WCF/100,WCI/100,(ONSET-FINISH)/STEP1+1)';ones((handles.TIMEMAX1-ONSET)/STEP1,1).*WCI/100];
WF = C+WC;
LEN = length(C);
TIME = [0:STEP1:handles.TIMEMAX1];

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


%%%%%%%%%%%  NOW UNCERTAINTIES %%%%%%%%%%%%%%

VARI = [1:2:size(handles.DATA,1)]; VARI = [VARI,VARI(end)+1];
VARNUM = length(VARI);


AGE_ = zeros(ERROR,VARNUM-2);

for J = 1:VARNUM-2;
    deblank(handles.VAR(VARI(J),:))
    for QQ = 1:ERROR;
        
        %Step 0: Reinitialise variables
        for i = VARI
            data = handles.DATA(i);
            if i == VARI(end);
            else
                data_x = handles.DATA(i+1);
            end
            var = deblank(handles.VAR(i,:));
            if i == VARI(J);
                %perturb
                if strcmp(var, 'K'); data = KA;
                elseif strcmp(var, 'U'); data = UA;
                elseif strcmp(var, 'T'); data = TA;
                end
                data = data+randn*data_x; if data < 0; data =0; end
                if strcmp(var, 'ONSET');
                    [n,p]= min(abs(TIME-data)); data = TIME(p);
                    if data > handles.TIMEMAX1; data = handles.TIMEMAX1; end 
                elseif strcmp(var, 'FINISH');
                    [m,g]= min(abs(TIME-data)); data = TIME(g);
                    if data < STEP1; data = STEP1; end
                elseif strcmp (var, 'CC');
                    if data < 1E-3; data = 1E-3; end
                elseif strcmp(var,'WCF')
                    if data < 1E-3; data = 1E-3; end
                elseif strcmp (var, 'WCI');
                    if data < 1E-3; 
                        data = 1E-3; 
                    end
                end
            end
            eval([var,' = [',num2str(data),'];']);
        end
        if strcmp(deblank(handles.VAR(VARI(J),:)),'ONSET')
            if FINISH >= ONSET; ONSET = FINISH+STEP1; end
        elseif strcmp(deblank(handles.VAR(VARI(J),:)),'FINISH')
            if FINISH >= ONSET; FINISH = ONSET-STEP1; end
        end

        K = K.*(1+CC/100);
        U = U.*(1+CC/100);
        T = T.*(1+CC/100);
 
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %Step 2: Derive carbonate model
        [tmp,f]=min(abs(TIME-FINISH)); 
        [tmp,o]=min(abs(TIME-ONSET)); 
        tm=length(TIME);
        C1 = ones(f,1);
        C3 = zeros(tm-o+1,1);
        C2 = linspace(1,0,o-f+1)';
        C = [C1;C2(2:end-1);C3].*CC./100'+1E-5;
        WC = [(C1(1:end-1).*WCF);linspace(WCF,WCI,o-f+1)';(C3(2:end)+1).*WCI]./100+1E-5;
        WF = C+WC;
       
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

        AGE_(QQ,J) = AGE;
    end
    MEAN = mean(AGE_(1:end,J));
    STD = std(AGE_(1:end,J));
    plot(MEAN, J, 'bo'); hold on
    plot([-2*STD,2*STD]+MEAN,[J J],'b')
end
plot (ones(J+2,1).*AGEN, [0:J+1],'k')
xlabel ('Age')
set(gca,'YTick',[1:J],'YTickLabel',deblank(handles.VAR(VARI,:)))
hold off
