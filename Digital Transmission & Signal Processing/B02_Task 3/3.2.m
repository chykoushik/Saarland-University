%Create Complete audio file [ref. Matlab documentation]
load handel.mat
file = 'handel.wav';
audiowrite(file,y,Fs);
clear y Fs

[y,Fs] = audioread('handel.wav');

factor = 32; 
y_quan = round(y*32)/32; 
y = interp(y,factor);

awgn_noise = awgn(y,25);

%downsample by factor
audiowrite('quan.wav', downsample(y_quan,factor),Fs);
audiowrite('awgn.wav',downsample(awgn_noise, factor),Fs);

%decrese by factor
audiowrite('decre_samp_freq_quan.wav',decimate(y_quan,factor),Fs);
audiowrite('decre_samp_freq_awgn.wav',decimate(awgn_noise,factor),Fs);

% Qn. What happens to the thermal noise? What happens to the quantization noise?
% Ans. quantization noise is same but noise decreases for awgn. 

