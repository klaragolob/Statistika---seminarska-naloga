%naloga 3 - STATISTIKA

podatki = readtable('ZarkiGama.csv');
podatki = table2array(podatki);

%a) Histogram

n = length(podatki);
podatki = sort(podatki);
l = 2*(podatki(round(n*(3/4)))-podatki(round(n*(1/4))))/(n^(1/3));
sirina = round(n/l);
st = zeros(round(max(podatki)/l)+1,1);
for i=1:n
    cas = podatki(i);
    indeks = round(cas/l)+1;
    st(indeks) = st(indeks)+1;
end


bar(0:l:(k*l-1), st/sum(st*l));
% saveas(gcf,'histogram3.png')

%b) Računanje parametrov
%metoda momentov
povprecje = sum(podatki)/n;
varianca = sum((podatki-povprecje*ones(size(podatki))).^2)/(n-1);
alpha1 = povprecje.^2/varianca
lambda1 = povprecje/varianca
%metoda njavečjega verjetja
vsotalogaritmov = sum(log(podatki));
enacba = @(x) log(x)-log(povprecje) + vsotalogaritmov/n - psi(x);
alpha2 = fzero(enacba, 1)
lambda2 = alpha2 / povprecje

%c) Porazdelitve na histogramu
gama1 = @(x) ((lambda1^alpha1)*(log(x).^(alpha1-1)).*exp(-lambda1*x))/gamma(alpha1);
gama2 = @(x) ((lambda2^alpha2)*(x.^(alpha2-1)).*exp(-lambda2*x))/gamma(alpha2);
% figure
% hold on
% bar(0:l:(k*l-1), st/sum(st*l));
% plot(1:.25:400, gama1((1:.25:400)), 'm','LineWidth',2);
% plot(1:.25:400, gama2((1:.25:400)), 'b','LineWidth',2);
% hold off
% saveas(gcf,'histogram_porazdelitve3.png')

%d) Histogram na logaritemski lestvici
logpodatki = sort(log(podatki));
logl = 2*(logpodatki(round(n*(3/4)))-logpodatki(round(n*(1/4))))/(n^(1/3));
logst = zeros(round((max(logpodatki)-min(logpodatki))/logl)+1,1);
for i=1:n
    cas = logpodatki(i);
    indeks = round((cas-min(logpodatki))/logl)+1;
    logst(indeks) = logst(indeks)+1;
end
logk = size(logl);
x = 0:l:(logk*l-1);
gama1 = @(x) ((lambda1^alpha1)*(x.^(alpha1-1)).*exp(-lambda1*x))/gamma(alpha1);
figure
hold on
bar(x,logst/sum(logst*logl), 'b')
%saveas(gcf,'log_histogram3.png')
