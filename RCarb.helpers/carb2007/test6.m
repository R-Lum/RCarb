function varargout = test6(varargin)
% TEST6 M-file for test6.fig
%      TEST6, by itself, creates a new TEST6 or raises the existing
%      singleton*.
%
%      H = TEST6 returns the handle to a new TEST6 or the handle to
%      the existing singleton*.
%
%      TEST6('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in TEST6.M with the given input arguments.
%
%      TEST6('Property','Value',...) creates a new TEST6 or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before test6_OpeningFunction gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to test6_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Copyright 2002-2003 The MathWorks, Inc.

% Edit the above text to modify the response to help test6

% Last Modified by GUIDE v2.5 14-Sep-2010 17:09:31

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @test6_OpeningFcn, ...
                   'gui_OutputFcn',  @test6_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before test6 is made visible.
function test6_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to test6 (see VARARGIN)

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
handles.TIMEMAX1 = 500;   %ka
load mejdahl.txt; handles.MEJ = mejdahl;

fclose('all');
fid = fopen('SAMPLE_DATA2010a.txt','r');
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

TIME = [0:STEP1:handles.TIMEMAX1];
% for i = 1:n
%     
% handles.VARTXT = char({'K';'Th';'U';'Initial water content';'Final water content';...
%     'Carbonate content';'Grain diameter';'Cosmic dose rate';'Internal dose rate';...
%     'Carbonate onset';'Carbonate completion';...
%     'Palaeodose';'Step length';'Error cycles'});
% handles.PLUSTXT = char({char(177);char(177);char(177);char(177);char(177);char(177);...
%                                 char(177);char(177);char(177);char(177);char(177);char(177)})
% A(1) = {'Hi. Hope you are well'};
% A(2) = {'The lazy illll was asleep'};
% set(handles.listvalues,'String',char(A))

set(handles.viewer,'String',char(handles.SAMP_NAME),'Value',5);
set(handles.variables_txt,'String');
I = [1:2:size(handles.DATA,1)]; I = [I,I(end)+1];
set(handles.data,'String',num2str(handles.DATA(I,1)));
set(handles.plusminus,'String');
I = [2:2:size(handles.DATA,1)-1]; 
set(handles.error,'String',num2str(handles.DATA(I,1)));
set(handles.units,'String',char({'%';'ppm';'ppm';'% dry wt.';'% dry wt.';'% "dry" wt.';[char(181),'m'];'Gy/ka';'Gy/ka';'ka';'ka';'Gy';'ka'}));
set(handles.pop_var,'String',{'K (%)';'Th (ppm)';'U (ppm)';'Initial water content  (% dry wt.)';'Final water content (% dry wt.)';...
    'Carbonate content (% "dry" wt.)';'Grain diameter (um)';'Cosmic dose rate (Gy/ka)';'Internal dose rate (Gy/ka)';...
    'Carbonate onset (ka)';'Carbonate completion (ka)';...
    'Palaeodose (Gy)';'Step length (ka)';'Error cycles'});
set(handles.var_alt,'String',' ');
set(handles.err_alt,'String',' ');
set(handles.pl_mi,'String',char(177));
handles.SAMNUM = 1;
set(handles.text107,'String',char(177));
set(handles.text113,'String',char(177));
set(handles.text118,'String',char(177));
set(handles.text123,'String',char(177));
set(handles.text128,'String',char(177));
% Choose default command line output for test6
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

function varargout = test6_OutputFcn(hObject, eventdata, handles) 
varargout{1} = handles.output;


% --- Executes on selection change in viewer.
function viewer_Callback(hObject, eventdata, handles)
handles.SAMNUM = get(handles.viewer,'Value');
I = [1:2:size(handles.DATA,1)]; 
I = [I,I(end)+1];
set(handles.data,'String',num2str(handles.DATA(I,handles.SAMNUM)));
I = [2:2:size(handles.DATA,1)-1]; 
set(handles.error,'String',num2str(handles.DATA(I,handles.SAMNUM)));
guidata(hObject, handles);

function viewer_CreateFcn(hObject, eventdata, handles)
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end

% --- Executes on selection change in pop_var.
function pop_var_Callback(hObject, eventdata, handles)
function pop_var_CreateFcn(hObject, eventdata, handles)
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

function var_alt_Callback(hObject, eventdata, handles)
function var_alt_CreateFcn(hObject, eventdata, handles)
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

function err_alt_Callback(hObject, eventdata, handles)
function err_alt_CreateFcn(hObject, eventdata, handles)
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

function ins_alt_Callback(hObject, eventdata, handles)
pop = get(handles.pop_var,'Value');
var = get(handles.var_alt,'String');
err = get(handles.err_alt,'String');
I = [1:2:size(handles.DATA,1)];
if (~isempty(deblank(var)))
    I = [1:2:size(handles.DATA,1)]; I = [I,I(end)+1];
    handles.DATA(I(pop),handles.SAMNUM) = str2num(var);
end
if (~isempty(deblank(err))) & (pop < length(I)-2)
    I = [2:2:size(handles.DATA,1)-1]; 
    handles.DATA(I(pop),handles.SAMNUM) = str2num(err);
end
guidata(hObject, handles);
viewer_Callback(hObject, eventdata, handles)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% --- Executes on button press in calculate.
function calculate_Callback(hObject, eventdata, handles)
cla reset
set(handles.axes3,'Visible','off')
set(handles.axes1,'Visible','on')
set(handles.axes2,'Visible','on')

SAMNUM = get(handles.viewer,'Value');
SAMNAME = char(handles.SAMP_NAME(handles.SAMNUM));
DATA = handles.DATA(:,SAMNUM);
VAR = handles.VAR;
TIMEMAX1 = handles.TIMEMAX1;
for i = 1:size(VAR,1)
    eval([VAR(i,:),'=[',num2str(DATA(i)),'];']);
end
TIME = [0:STEP1:TIMEMAX1];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
WFA = WF(1)/100;
LEN = length(C);



% %Step 3: Derive linear uptake mode of uranium (code from Forbes Quarry work)  

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

XKBA = 1.25;
XTBA = 1.25;
XUBA = 1.25;
XKGA = 1.14;
XTGA = 1.14;
XUGA = 1.14;

DRKB = MK.*K.*handles.KB./(1 + XKB.*WF);
DRTB = MT.*T.*handles.TB./(1 + XTB.*WF);
DRUB = MU.*U.*handles.UB./(1 + XUB.*WF);
DRKG = 1.*K.*handles.KG./(1 + XKG.*WF);
DRTG = 1.*T.*handles.TG./(1 + XTG.*WF);
DRUG = 1.*U.*handles.UG./(1 + XUG.*WF);

DRKBA = MK.*KA.*handles.KB./(1 + XKBA.*WFA);
DRTBA = MT.*TA.*handles.TB./(1 + XTBA.*WFA);
DRUBA = MU.*UA.*handles.UB./(1 + XUBA.*WFA);
DRKGA = 1.*KA.*handles.KG./(1 + XKGA.*WFA);
DRTGA = 1.*TA.*handles.TG./(1 + XTGA.*WFA);
DRUGA = 1.*UA.*handles.UG./(1 + XUGA.*WFA);

DR = DRKB + DRTB + DRUB + DRKG + DRTG + DRUG + COSMIC + INTERNAL;
DRA = DRKBA + DRTBA + DRUBA + DRKGA + DRTGA + DRUGA + COSMIC + INTERNAL;

%Step 5: Calculate date
CUMDR = cumsum([0;(DR(1:end-1)+DR(2:end)).*STEP1./2]);
if DE > max(CUMDR)
    msgbox('ERROR: Dates greater than 500ka! Make timemax greater')
    return
end
AGE = interp1(CUMDR,TIME,DE);
AGEA = DE./DRA;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


set(handles.age,'String',num2str(AGE));
set(handles.cage,'String',num2str(DE/DRA));
set(handles.cdr,'String',num2str(DRA));
set(handles.odr,'String',num2str(DR(end)));
set(handles.fdr,'String',num2str(DR(1)));

DEN = DE;
DRN = DR;
CUMDRN = CUMDR;
AGEN = AGE;

%%%%%%%%%%%  NOW UNCERTAINTIES %%%%%%%%%%%%%%
DE_ = zeros(ERROR,1);
DR_ = zeros(LEN,ERROR);
CUMDR_ = zeros(LEN,ERROR);
AGE_ = zeros(ERROR,1);
h = waitbar(0,'Please wait...');
for QQ = 1:ERROR
    %Step 0: Reinitialise variables
    for i = 1:size(handles.VAR,1)
        [VAR(i,:),' = [',num2str(DATA(i)),'];'];
        eval([VAR(i,:),' = [',num2strDATA(i)),'];']);
    end
    
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

    %Step 0: Reinitialise variables
    VARI = [1:2:size(handles.VAR,1)]; 
    VARNUM = length(VARI);
    for J = 1:VARNUM-1; 
        data = handles.DATA(VARI(J),SAMNUM);
        data_x = handles.DATA(VARI(J)+1,SAMNUM);
        var = deblank(handles.VAR(VARI(J),:));
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
        eval([var,' = [',num2str(data),'];']);
        if strcmp(deblank(handles.VAR(VARI(J),:)),'ONSET')
            if FINISH >= ONSET; ONSET = FINISH+STEP1;end
        elseif strcmp(deblank(handles.VAR(VARI(J),:)),'FINISH')
            if FINISH >= ONSET; FINISH = ONSET-STEP1; ONSET = FINISH+STEP1; end
        end
    end
    
    
    DE = DE+randn*DE_X; if DE < 0; DE = 0; end
    COSMIC = COSMIC+randn*COSMIC_X; if COSMIC < 0; COSMIC = 0; end
    INTERNAL = INTERNAL+randn*INTERNAL_X; if INTERNAL < 0; INTERNAL = 0; end
    ONSET = round(ONSET + randn*ONSET_X); if ONSET < 0; ONSET = 0; end; if ONSET > 500; ONSET = 500; end; 
    FINISH = round(FINISH + randn*FINISH_X); if FINISH < 0; FINISH = 0; end; if FINISH > ONSET; FINISH = ONSET; end; 
    DIAM = DIAM+randn*DIAM_X; if DIAM < 0; DIAM = 0; end
    
    %Step 1: Find U,Th,K values for sediment only (before carbonate dilution)
    CC = CC+randn*CC_X; if CC < 1E-3; CC = 1E-3; end
    WCF = WCF+randn*WCF_X; if WCF < 1E-3; WCF = 1E-3; end
    WFA = WCF/100;
    WCI = WCI+randn*WCI_X; if WCI < 1E-3; WCI = 1E-3; end
    K = (KA + randn*K_X); if KA < 0; KA = 0; end
    U = (UA + randn*U_X); if UA < 0; UA = 0; end
    T = (TA + randn*T_X); if TA < 0; TA = 0; end
    K = K.*(1+CC/100); 
    U = U.*(1+CC/100); 
    T = T.*(1+CC/100); 
%     
%     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     %Step 2: Derive carbonate model
%     C = [ones(FINISH./STEP1,1).*CC/100;linspace(CC/100,1E-5,(ONSET-FINISH)/STEP1+1)';zeros((handles.TIMEMAX1-ONSET)/STEP1,1)+1E-5];
%     WC = [ones(FINISH./STEP1,1).*WCF/100;linspace(WCF/100,WCI/100,(ONSET-FINISH)/STEP1+1)';ones((handles.TIMEMAX1-ONSET)/STEP1,1).*WCI/100];
%     WF = C+WC;
%     WFA = WF(1)/100;
%     LEN = length(C);
%     TIME = [0:STEP1:handles.TIMEMAX1'];
%     %Step 3: Derive linear uptake mode of uranium (code from Forbes Quarry work)
% 
%     %Step 4: Calaculate time-dependent dose rate
%     MK = 1-exp(interp1(log(handles.MEJ(:,1)),log(handles.MEJ(:,2)),log(DIAM/1000)));
%     MT = 1-exp(interp1(log(handles.MEJ(:,1)),log(handles.MEJ(:,3)),log(DIAM/1000)));
%     MU = 1-exp(interp1(log(handles.MEJ(:,1)),log(handles.MEJ(:,4)),log(DIAM/1000)));
% 
%     XKB = griddata(log(handles.WD),log(handles.CD),handles.DATAek,log(WC),log(C));
%     XTB = griddata(log(handles.WD),log(handles.CD),handles.DATAet,log(WC),log(C));
%     XUB = griddata(log(handles.WD),log(handles.CD),handles.DATAeu,log(WC),log(C));
%     XKG = griddata(log(handles.WD),log(handles.CD),handles.DATApk,log(WC),log(C));
%     XTG = griddata(log(handles.WD),log(handles.CD),handles.DATApt,log(WC),log(C));
%     XUG = griddata(log(handles.WD),log(handles.CD),handles.DATApu,log(WC),log(C));
%     XKB = griddata(log(handles.WD),log(handles.CD),handles.DATAek,log(WC),log(C));
% 
%     XKBA = 1.25;
%     XTBA = 1.25;
%     XUBA = 1.25;
%     XKGA = 1.14;
%     XTGA = 1.14;
%     XUGA = 1.14;
% 
%     DRKB = MK.*K.*handles.KB./(1 + XKB.*WF);
%     DRTB = MT.*T.*handles.TB./(1 + XTB.*WF);
%     DRUB = MU.*U.*handles.UB./(1 + XUB.*WF);
%     DRKG = 1.*K.*handles.KG./(1 + XKG.*WF);
%     DRTG = 1.*T.*handles.TG./(1 + XTG.*WF);
%     DRUG = 1.*U.*handles.UG./(1 + XUG.*WF);
% 
%     DRKBA = MK.*KA.*handles.KB./(1 + XKBA.*WFA);
%     DRTBA = MT.*TA.*handles.TB./(1 + XTBA.*WFA);
%     DRUBA = MU.*UA.*handles.UB./(1 + XUBA.*WFA);
%     DRKGA = 1.*KA.*handles.KG./(1 + XKGA.*WFA);
%     DRTGA = 1.*TA.*handles.TG./(1 + XTGA.*WFA);
%     DRUGA = 1.*UA.*handles.UG./(1 + XUGA.*WFA);
% 
%     DR = DRKB + DRTB + DRUB + DRKG + DRTG + DRUG + COSMIC + INTERNAL;
%     DRA = DRKBA + DRTBA + DRUBA + DRKGA + DRTGA + DRUGA + COSMIC + INTERNAL;
% 
%     %Step 5: Calculate date
%     CUMDR = cumsum([0;(DR(1:end-1)+DR(2:end)).*STEP1./2]);
%     if DE > max(CUMDR)
%         msgbox('ERROR: Dates greater than 500ka! Make timemax greater')
%         return
%     end
%     AGE = interp1(CUMDR,TIME,DE);
%     AGEA = DE./DRA;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    
    DE_(QQ) = DE;
    DR_(:,QQ) = DR;
    DRA_(QQ) = DRA(1);
    CUMDR_(:,QQ) = CUMDR;
    AGE_(QQ) = AGE;
    AGEA_(QQ) = AGEA;
    if mod(QQ,5) == 0
        set(handles.age_x,'String',num2str(std(AGE_(1:QQ))));
        set(handles.cage_x,'String',num2str(std(AGEA_(1:QQ))));
        set(handles.cdr_x,'String',num2str(std(DRA_(1:QQ))));
        set(handles.odr_x,'String',num2str(std(DR_(end,1:QQ))));
        set(handles.fdr_x,'String',num2str(std(DR_(1,1:QQ))));
    end
    h = waitbar(QQ/ERROR,h);
end
close(h)
set(handles.age_x,'String',num2str(std(AGE_)));
set(handles.cage_x,'String',num2str(std(AGEA_)));
set(handles.cdr_x,'String',num2str(std(DRA_)));
set(handles.odr_x,'String',num2str(std(DR_(end,:))));
set(handles.fdr_x,'String',num2str(std(DR_(1,:))));

% plot it
MAXAGE = ceil((AGEN+std(AGE_))/50*STEP1)*50*STEP1;
axes(handles.axes1)
DRmean = mean(DR_')' ;
DRdiff = DR_ - DRmean(:,ones(1,ERROR));
for i = 1:length(TIME)
    I = find(DRdiff(i,:) == 0); I1 = find(DRdiff(i,:) > 0); I2 = find(DRdiff(i,:) < 0);
    DR1 = [DRdiff(i,I),DRdiff(i,I1),-DRdiff(i,I1)];
    DR2 = [DRdiff(i,I),DRdiff(i,I2),-DRdiff(i,I2)];
    ST1(i,1) = DRmean(i)+std(DR1);
    ST2(i,1) = DRmean(i)-std(DR2);
end    
plot(TIME,DRN,'b',TIME,ST1,'b--',TIME,ST2,'b--')
xlabel('Time (ka)')
ylabel('Dose rate (Gy/ka)')
title(['Sample ',SAMNAME])
V = axis;
axis([V(1) MAXAGE V(3) V(4) ]);


axes(handles.axes2)
CUMDRmean = mean(CUMDR_')'; 
CUMDRdiff = CUMDR_ - CUMDRmean(:,ones(1,ERROR));
for i = 1:length(TIME)
    I = find(CUMDRdiff(i,:) == 0); I1 = find(CUMDRdiff(i,:) > 0); I2 = find(CUMDRdiff(i,:) < 0);
    CUMDR1 = [CUMDRdiff(i,I),CUMDRdiff(i,I1),-CUMDRdiff(i,I1)];
    CUMDR2 = [CUMDRdiff(i,I),CUMDRdiff(i,I2),-CUMDRdiff(i,I2)];
    ST1(i,1) = CUMDRmean(i)+std(CUMDR1);
    ST2(i,1) = CUMDRmean(i)-std(CUMDR2);
end
plot(TIME,CUMDRN,'b',TIME,ST1,'b--',TIME,ST2,'b--',...
     [0 AGEN],[DEN DEN],'r',[AGEN AGEN],[0 DEN],'r',...
     [0 AGEN+std(AGE_)],[DEN+std(DE_) DEN+std(DE_)],'r--',...
     [AGEN+std(AGE_) AGEN+std(AGE_)],[0 DEN+std(DE_)],'r--',...
     [0 AGEN-std(AGE_)],[DEN-std(DE_) DEN-std(DE_)],'r--',...
     [AGEN-std(AGE_) AGEN-std(AGE_)],[0 DEN-std(DE_)],'r--')
xlabel('Time (ka)')
ylabel('Absorbed dose during deposition (Gy)')
title(['Sample ',SAMNAME])
V = axis;
MAXDOSE = ceil(interp1(TIME,ST1,MAXAGE)/10)*10;
axis([V(1) MAXAGE V(3) MAXDOSE ])


save all_data2.mat


guidata(hObject, handles);













function sensitivitytest_Callback(hObject, eventdata, handles)
axes(handles.axes1)
cla 
axes(handles.axes2)
cla 
h = waitbar(0,'Please wait...');

set(handles.axes3,'Visible','on')
set(handles.axes1,'Visible','off')
set(handles.axes2,'Visible','off')

SAMNUM = get(handles.viewer,'Value');
SAMNAME = char(handles.SAMP_NAME(handles.SAMNUM));
for i = 1:size(handles.VAR,1)
    eval([handles.VAR(i,:),'=[',num2str(handles.DATA(i,SAMNUM)),'];']);
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
AGEN = AGE


%%%%%%%%%%%  NOW UNCERTAINTIES %%%%%%%%%%%%%%

VARI = [1:2:size(handles.DATA,1)]; VARI = [VARI,VARI(end)+1];

VARNUM = length(VARI);


AGE_ = zeros(ERROR,VARNUM-2);

for J = 1:VARNUM-2;

    for QQ = 1:ERROR;
        
        %Step 0: Reinitialise variables
        for i = VARI
            data = handles.DATA(i,SAMNUM);
            if i == VARI(end);
            else
                data_x = handles.DATA(i+1,SAMNUM);
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
    MEAN(J) = mean(AGE_(1:end,J));
    STD(J) = std(AGE_(1:end,J));   
    h = waitbar(J/(VARNUM-2),h);

end
close(h);
axes(handles.axes3)
for i = 1:J    
    plot(MEAN(i), i, 'bo'); hold on
    plot([-2*STD(i),2*STD(i)]+MEAN(i),[i i],'b')
end
plot (ones(J+2,1).*AGEN, [0:J+1],'k')
xlabel ('Age (2sig)')
set(gca,'YTick',[1:J],'YTickLabel',deblank(handles.VAR(VARI,:)))
hold off




% --- Executes on selection change in listvalues.
function listvalues_Callback(hObject, eventdata, handles)
function listvalues_CreateFcn(hObject, eventdata, handles)

if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


