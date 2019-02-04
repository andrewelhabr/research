function [transformed_data] = cut_and_append(fft_data)
averaged_freq_data = zeros(6,50);
transformed_data = zeros(1,6*50);
for i = 1:6
    for k = 1:50
        averaged_freq_data(i,k) = mean(fft_data(i,10*(k-1)+1:10*k));
    end
end
for i = 1:6
    transformed_data(50*(i-1)+1:50*i) = averaged_freq_data(i,:);
end
end