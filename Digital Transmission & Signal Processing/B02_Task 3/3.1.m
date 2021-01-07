%3.1. Plot the autocorrelation of this signal for small and large y.

t = 0:1:10;
y = awgn(t,20);
autocorr(y)

t = 0:1:1000;
y = awgn(t,20);
autocorr(y)