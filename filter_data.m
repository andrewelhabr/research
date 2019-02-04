function [smooth_data] = filter_data(FO,hz,cut_length)

%if~exist('timeStart')
%    timeStart=1; %seizure onset at 2547
    %timeStart=2560; %seizure onset at 2564
%end
if~exist('FO')
    load clip37 FO
end
if ~exist('hz')
    hz=512;
end

indR=[1:2:7];
indL=indR+1;
for i=1:4
    Channel(indR(i)).Name=['RFO' num2str(i)];
    Channel(indL(i)).Name=['LFO' num2str(i)];
end

%FO bipolar
bipolar=[1 3; 3 5; 5 7; 2 4; 4 6; 6 8];

%create labels
for n=1:length(bipolar)
    i=bipolar(n,1); j=bipolar(n,2);
    label(n).text=[Channel(i).Name '-' Channel(j).Name];
    %disp(label(n).text)
end

%get 10 s segment
%ind=timeStart*hz+1:(timeStart+cut_length)*hz;
ind=1:cut_length*hz;
%get data
%[b, a]=butter(9,[2 70]/(hz/2));
%[b, a]=butter(9,[1 50]/(hz/2));
[b, a]=butter(5,1/(hz/2),'high');
for n=1:length(bipolar)
    i=bipolar(n,1); j=bipolar(n,2);
    %data(n,:)=F(i,ind)-F(j,ind);
    data(n,:)=FO(i,ind)-FO(j,ind);
    
    %plot(t,-data(n,:)*multiplier/offset-n);
    smooth_data(n,:)=filtfilt(b,a,data(n,:));
end