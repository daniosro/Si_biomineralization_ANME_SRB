clear all
clc
%close all

x=round(linspace(11,55,5));
%x=5;
t=365*3;
x0=[linspace(0,1,21),round(linspace(1,20,380))];

for i=1:length(x)
tau(i,:)=t./(log2(x(i)./x0));
end

for i=1:length(x)
tau_max(i)=max(tau(i,:));
end

for i=1:length(x)
    counter=1;
    while tau(i,counter)<tau_max
        counter=counter+1;
    end
    counter_vector(i)=counter;
end

for i=1:length(x)
tau(i,:)=[tau(i,1:counter_vector(i)),NaN(1,length(x0)-counter_vector(i))];
end
    
lower_tau=30.4*1.1*ones(1,length(x0));
upper_tau=30.4*7.5*ones(1,length(x0));

[~,threshold_ind]=ind2sub(size(x0),find(x0==1));

vertx=[0 0];
verty=[0 365];

figure(1)
plot(log2(x0),tau(1,:),log2(x0),tau(2,:),log2(x0),tau(3,:),log2(x0),tau(4,:),...
    log2(x0),tau(5,:),log2(x0),lower_tau,'k--',log2(x0),upper_tau,'k--',vertx,verty,'k:','LineWidth',1.5);
ylim([0,365])
set(gca,'XTick',0:1:20,'YTick',0:30:365,'XTickLabel',[1 2 4 8 16 32],'YTickLabel',[0:1:12])
xlabel('Initial Aggregate Number','FontSize',12)
ylabel('Doubling Time (months)','FontSize',12)
%print('SimpleDoublingModel4','-depsc')

%%
mtx_columns=linspace(1 );

figure(2)
contour(log10(x0),log10(tau),x)