%% Wavelet analysis — reconstruction from the final thesis appendix
% Undergraduate thesis: "Análise de séries temporais multivariadas via Wavelet"
%
% This script transcribes the MATLAB code printed in Appendix A.
% It assumes a table named `dados` is already available with the variables
% referenced below (Date, SeP500, bitcoin and DJIA).

%% Multiresolution decomposition
x = dados.SeP500;
t = dados.Date;

mra = modwtmra(modwt(x,8));
helperMRAPlot(x,mra,t,'wavelet','Wavelet ARM - S&P500');

%% Multiscale correlation between two series
x = dados.bitcoin;
y = dados.DJIA;

[rho,pval] = corrcoef(x,y);

wtPI = modwt(x,'db1',8,'reflection');
wtGE = modwt(y,'db1',8,'reflection');

wcorrtable = modwtcorr(wtPI,wtGE,'db2',0.95,'reflection','table');
display(wcorrtable)

piwt = modwt(x,'fk8',8);
pcwt = modwt(y,'fk8',8);

figure;
modwtcorr(piwt,pcwt,'fk8');

%% Wavelet coherence map
t = dados.Date;
x = dados.DJIA;
y = dados.bitcoin;

[wcoh,~,period,coi] = wcoherence(x,y);

figure
h = pcolor(t,log2(period),wcoh);
h.EdgeColor = 'none';

ax = gca;
ytick = round(pow2(ax.YTick),3);
ax.YTickLabel = ytick;
ax.XLabel.String = 'Data';
ax.YLabel.String = 'Frequência';
ax.Title.String = 'Coerência Wavelet - DJIA vs Bitcoin';

hcol = colorbar;
hcol.Label.String = 'Magnitude Quadrática da Coerência';

hold on;
plot(ax,t,log2(coi),'w--','linewidth',2)
