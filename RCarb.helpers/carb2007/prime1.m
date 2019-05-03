
tic
n=2;
while n < 1E4
    n=n+1;
    prime = 1;
    j = 1;
    keepgoing = 1;
    while (keepgoing == 1) & (j<n-1)
        j=j+1;
        A=mod(n,j) ;   
        if A==0
            prime=0;
            keepgoing = 0;
        end
    end
    if prime==1
        disp(num2str(n))
    end
end
toc




% clear; 
% N=30;
% ni=0;
% for i=1:2:N
%     ni=ni+1;
%     nj=0;
%     for j=1:2:N
%         nj=nj+1;
%         if mod(i*j,9)==0
%             z(ni,nj)=0;
%         else 
%             z(ni,nj)=i*j;      
%         end
%     end
% end
% 
% z
% 
