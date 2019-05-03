function varargout = carb_2007a(varargin)
% CARB_2007A M-file for carb_2007a.fig
%      CARB_2007A, by itself, creates a new CARB_2007A or raises the existing
%      singleton*.
%
%      H = CARB_2007A returns the handle to a new CARB_2007A or the handle to
%      the existing singleton*.
%
%      CARB_2007A('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in CARB_2007A.M with the given input arguments.
%
%      CARB_2007A('Property','Value',...) creates a new CARB_2007A or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before carb_2007a_OpeningFunction gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to carb_2007a_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Copyright 2002-2003 The MathWorks, Inc.

% Edit the above text to modify the response to help carb_2007a

% Last Modified by GUIDE v2.5 17-Dec-2006 06:15:07

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @carb_2007a_OpeningFcn, ...
                   'gui_OutputFcn',  @carb_2007a_OutputFcn, ...
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


% --- Executes just before carb_2007a is made visible.
function carb_2007a_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to carb_2007a (see VARARGIN)

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
load DATAeu238.txt; handles.DATAeu238 = DATAeu238(2:end,2:end);
load DATAeu234.txt; handles.DATAeu234 = DATAeu234(2:end,2:end);
load DATAet230.txt; handles.DATAet230 = DATAet230(2:end,2:end);
%load DATAeu235.txt; handles.DATAeu235 = DATAeu235(2:end,2:end);
%load DATAep231.txt; handles.DATAep231 = DATAep231(2:end,2:end);
load DATApu238.txt; handles.DATApu238 = DATApu238(2:end,2:end);
load DATApu234.txt; handles.DATApu234 = DATApu234(2:end,2:end);
load DATApt230.txt; handles.DATApt230 = DATApt230(2:end,2:end);
%load DATApu235.txt; handles.DATApu235 = DATApu235(2:end,2:end);
%load DATApp231.txt; handles.DATApp231 = DATApp231(2:end,2:end);
handles.WD = DATAek(2:end,ones(1,size(DATAek,1)-1));
handles.CD = handles.WD';
handles.TIMEMAX1 = 500;   %ka
load mejdahl.txt; handles.MEJ = mejdahl;

fclose('all');
fid = fopen('SAMPLE_DATA.txt','r');
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

set(handles.viewer,'String',char(handles.SAMP_NAME),'Value',5);
set(handles.variables_txt,'String',{'K';'Th';'U';'U238';'U234/U238 activity';'Initial water content';'Final water content';...
    'Carbonate content';'Grain diameter';'Cosmic dose rate';'Internal dose rate';...
    'Carbonate onset';'Carbonate completion';...
    'Palaeodose';'Step length';'Error cycles'});
I = [1:2:size(handles.DATA,1)]; I = [I,I(end)+1];
set(handles.data,'String',num2str(handles.DATA(I,1)));
set(handles.plusminus,'String',{char(177);char(177);char(177);char(177);char(177);char(177);char(177);char(177);...
                                char(177);char(177);char(177);char(177);char(177);char(177)});
I = [2:2:size(handles.DATA,1)-1]; 
set(handles.error,'String',num2str(handles.DATA(I,1)));
set(handles.units,'String',char({'%';'ppm';'ppm';'ppm';'';'% dry wt.';'% dry wt.';'% "dry" wt.';[char(181),'m'];'Gy/ka';'Gy/ka';'ka';'ka';'Gy';'ka'}));
set(handles.pop_var,'String',{'K (%)';'Th (ppm)';'U (ppm)';'U238';'U234/U238 activity';'Initial water content  (% dry wt.)';'Final water content (% dry wt.)';...
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

% Choose default command line output for carb_2007a
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

function varargout = carb_2007a_OutputFcn(hObject, eventdata, handles) 
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
save data1.mat


[DATE] = fminbnd(@(x) daterlu1(x),STEP1,handles.TIMEMAX1,optimset('TolX',STEP1))

load data2.mat
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
        [handles.VAR(i,:),' = [',num2str(handles.DATA(i,SAMNUM)),'];'];
        eval([handles.VAR(i,:),' = [',num2str(handles.DATA(i,SAMNUM)),'];']);
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
    KA = (K + randn*K_X); if KA < 0; KA = 0; end
    UA = (U + randn*U_X); if UA < 0; UA = 0; end
    TA = (T + randn*T_X); if TA < 0; TA = 0; end
    K = KA.*(1+CC/100); 
    U = UA.*(1+CC/100); 
    T = TA.*(1+CC/100); 
    
    save data1.mat
    [DATE] = fminbnd(@(x) daterlu1(x),STEP1,handles.TIMEMAX1,optimset('TolX',STEP1));
    load data2.mat
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
MAXAGE = ceil((AGEN+std(AGE_))/50)*50;
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
save all_data1.mat


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










