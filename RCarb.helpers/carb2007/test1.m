function varargout = test1(varargin)
% TEST1 M-file for test1.fig
%      TEST1, by itself, creates a new TEST1 or raises the existing
%      singleton*.
%
%      H = TEST1 returns the handle to a new TEST1 or the handle to
%      the existing singleton*.
%
%      TEST1('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in TEST1.M with the given input arguments.
%
%      TEST1('Property','Value',...) creates a new TEST1 or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before test1_OpeningFunction gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to test1_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Copyright 2002-2003 The MathWorks, Inc.

% Edit the above text to modify the response to help test1

% Last Modified by GUIDE v2.5 10-Sep-2010 15:30:03

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @test1_OpeningFcn, ...
                   'gui_OutputFcn',  @test1_OutputFcn, ...
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


% --- Executes just before test1 is made visible.
function test1_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to test1 (see VARARGIN)

fid=fopen('test1.txt', 'r');
if fid > 0
    tline = [deblank(fgetl(fid)),' '];
    I = find((double(tline) == 9) | (double(tline) == 32));
    yes = 0;
    n = 0;
    while yes == 0
        n = n + 1;
        handles.test1(n) = {tline(1:I(1)-1)};
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
handles.x = str2num(char(handles.test1));
%set(handles.xvalues,'String',char(handles.test1),'Value',1);
set(handles.xvalues,'String',{'x^2','x^3','1/x'},'Value',1);


% Choose default command line output for test1
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes test1 wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = test1_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on selection change in xvalues.
function xvalues_Callback(hObject, eventdata, handles)
% hObject    handle to xvalues (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns xvalues contents as cell array
%        contents{get(hObject,'Value')} returns selected item from xvalues


% --- Executes during object creation, after setting all properties.
function xvalues_CreateFcn(hObject, eventdata, handles)
% hObject    handle to xvalues (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: listbox controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc
    set(hObject,'BackgroundColor','white');
else
    set(hObject,'BackgroundColor',get(0,'defaultUicontrolBackgroundColor'));
end


% --- Executes on button press in calculate.
function calculate_Callback(hObject, eventdata, handles)
% hObject    handle to calculate (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
X = handles.x;
PICK = get(handles.xvalues,'Value')
if PICK == 1
    Y = X.^2;
elseif PICK == 2
    Y = X.^3;
elseif PICK == 3
    Y = 1./X;
end
axes(handles.axes1)
plot(X,Y)

