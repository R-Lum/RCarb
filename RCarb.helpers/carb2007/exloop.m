clear
a=1:5;
for i=1:length(a);
    if i==1
        b(i)=a(i); 
    else
        b(i)=a(i)+ b(i-1); 
    end
end
clear
a=1:5;
for i=1:length(a);
    b(i)=sum(a(1:i)); 
end
