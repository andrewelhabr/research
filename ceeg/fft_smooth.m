function [P1, freq_buckets] = fft_smooth(filtered_data,hz,cut_length)
Y = zeros(6,cut_length*512);
freq_buckets = hz*(0:((cut_length*hz)/2))/(cut_length*hz);
for i = 1:6
    Y(i,1:cut_length*hz) = fft(filtered_data(i,1:cut_length*hz));
    P2(i,:) = abs(Y(i,:)/(cut_length*hz));
    P1(i,:) = P2(i,1:(cut_length*hz)/2+1);
    P1(i,2:end-1) = 2*P1(i,2:end-1);
end
end
