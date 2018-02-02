clc; clear all; close all;

%% Hulme Result

fid = fopen('HulmeResult.dat');
fgetl(fid);
fgetl(fid);
tmp = fscanf(fid, '%f %f %f ',[3  inf])';

Hulme.ka        = tmp(:,1);
Hulme.addedMass = tmp(:,2);
Hulme.damping   = tmp(:,3);

%% YoungMyung Result

fid = fopen('../radiationForce.dat');
fgetl(fid);
tmp = fscanf(fid, '%f %f %f %f %f %f',[6  inf])';

YM.ka        = tmp(:,2);
YM.addedMass = tmp(:,3);
YM.damping   = tmp(:,4);

YM.c0   = tmp(:,5) + i * tmp(:,6);

%% Plot
markersize = 8;
fontsize   = 16;
%%

figure(1)
plot(Hulme.ka, Hulme.addedMass, 'ko','markerfacecolor','y','markersize',markersize)
hold on
plot(YM.ka, YM.addedMass,'k')

xlabel('ka','fontsize',fontsize)
ylabel('a_{33}','fontsize',fontsize)
% set(gca,'xlim',[0 5])
set(gca, 'fontsize',fontsize)
set(gca, 'xgrid','on', 'ygrid', 'on', 'gridlinestyle','-')
legend('Hulme (1982)', 'YoungMyung')

%%

figure(2)
plot(Hulme.ka, Hulme.damping, 'ko','markerfacecolor','y','markersize',markersize)
hold on
plot(YM.ka, YM.damping,'k')

xlabel('ka','fontsize',fontsize)
ylabel('b_{33}','fontsize',fontsize)
set(gca, 'fontsize',fontsize)
set(gca, 'xgrid','on', 'ygrid', 'on', 'gridlinestyle','-')
% set(gca,'xlim',[0 5])
legend('Hulme (1982)', 'YoungMyung')
