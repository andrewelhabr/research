%% Gathering observations before transformation

clear all
close all

load('trendReview.mat')

% Computing basic seizure statistics.
clips_with_sz = sum(isSz);
total_sz = sum(szTime1(:,2)~=0)+sum(szTime2(:,2)~=0);

% Storing clip numbers with 1 and 2 seizures, respectively.
clip_nums_with_1_sz = [];
clip_nums_with_2_sz = [];
for i = 1:length(isSz)
    if szTime1(i,2)~=0
        clip_nums_with_1_sz = [clip_nums_with_1_sz; i];
        if szTime2(i,2)~=0
            clip_nums_with_2_sz = [clip_nums_with_2_sz; i];
        else ;
        end
    else
        ;
    end
end

true_clip_nums = [1;2;3;4;5;6;7;8;9;10;11;13;14;15;16;17;18;19;20;22;23;24;
                  25;26;27;28;31; 32;33;34;35;36;37;38;39;40;41;44;45;47;
                  50;52;53;56;57;58;61;62;64;65;67;69;70;71;74;77;78;79;80;
                  82;86;88;93;94;95;96];
              
true_clip_nums_with_1_sz = true_clip_nums(clip_nums_with_1_sz);
true_clip_nums_with_2_sz = true_clip_nums(clip_nums_with_2_sz);

%% (Takes about 20 seconds to load.)
% Storing FO and hz data for clips with seizures.
FO_all = zeros(8,60*60*512,length(true_clip_nums_with_1_sz));
hz_all = zeros(1,length(true_clip_nums_with_1_sz));
for i = 1:length(true_clip_nums_with_1_sz)
    source=[cd '/Clips/' 'clip' num2str(true_clip_nums_with_1_sz(i))];
    load(source,'FO','hz')
    FO_all(:,1:(60*60*hz),i) = FO;
    hz_all(:,i) = hz;
end
%%
% Cut the clips to give desired observations. First, cut the seizure clips.
% For now, we are only using data from the first seizures.
length_of_first_sz = szTime1(clip_nums_with_1_sz,2) - szTime1(clip_nums_with_1_sz,1);
length_of_second_sz = szTime1(clip_nums_with_2_sz,2) - szTime1(clip_nums_with_2_sz,1);

% Arbitrary method of making cuts.
% If cut_length is set to 10 second, use back to back cuts.
% If cut_length is set to 1 second, take clips for every other second.
cut_length = 10;
%dummy_num = sum(floor(length_of_first_sz/10))+sum(floor(length_of_second_sz/10));
dummy_num = sum(floor(length_of_first_sz/10));
if cut_length == 10
    unfiltered_sz_observations = zeros(8,cut_length*512,dummy_num);
else
    unfiltered_sz_observations = zeros(8,cut_length*512,dummy_num*5);
end

i = 1;
[eight, seconds_hz, num_of_sz_obs] = size(unfiltered_sz_observations);
time_start_sz = zeros(num_of_sz_obs, 1);
hz_all_sz = zeros(num_of_sz_obs, 1);
% 10 second observation case
if cut_length == 10
    for j = 1:clips_with_sz
        for k = 1:floor(length_of_first_sz(j)/10)
            unfiltered_sz_observations(:,1:cut_length*hz_all(j),i) = FO_all(:,(szTime1(clip_nums_with_1_sz(j),1)+cut_length*(k-1))*hz_all(j):(szTime1(clip_nums_with_1_sz(j),1)+cut_length*k)*hz_all(j)-1,j);
            time_start_sz(i) = (szTime1(clip_nums_with_1_sz(j),1)+cut_length*(k-1));
            hz_all_sz(i) = hz_all(j);
            i = i + 1;
        end
    end
% 1 second observation case
else
    for j = 1:clips_with_sz
        for k = 1:floor(length_of_first_sz(j)/10)*5
            unfiltered_sz_observations(:,1:cut_length*hz_all(j),i) = FO_all(:,(szTime1(clip_nums_with_1_sz(j),1)+cut_length*(2*k))*hz_all(j):(szTime1(clip_nums_with_1_sz(j),1)+cut_length*(2*k-1))*hz_all(j)-1,j);
            time_start_sz(i) = (szTime1(clip_nums_with_1_sz(j),1)+cut_length*(2*k));
            hz_all_sz(i) = hz_all(j);
            i = i + 1;
        end
    end
end

% Now, cut the non-seizure clips.
arb_num = 50;
num_of_non_sz_obs = arb_num*clips_with_sz;
unfiltered_non_sz_observations = zeros(8,cut_length*512,num_of_non_sz_obs);

seed = rng(42);
non_sz_obs_start = round(rand(clips_with_sz,arb_num)*(60*59+50));

for j = 1:clips_with_sz
    for k = 1:arb_num
        if non_sz_obs_start(j,k) <= szTime1(clip_nums_with_1_sz(j),1)-10 || non_sz_obs_start(j,k) >= szTime1(clip_nums_with_1_sz(j),2)
            non_sz_obs_start(j,k) = non_sz_obs_start(j,k);
        else
            non_sz_obs_start(j,k) = rand(1)*(szTime1(clip_nums_with_1_sz(j),1)-10);
        end
        i = i + 1;
    end
end

i = 1;
time_start_non_sz = zeros(num_of_non_sz_obs, 1);
hz_all_non_sz = zeros(num_of_non_sz_obs, 1);
% 1 and 10 second observation case (I think?)
% Note that there is a possibility that some of these observations overlap.
for j = 1:clips_with_sz
    for k = 1:arb_num
        unfiltered_non_sz_observations(:,1:cut_length*hz_all(j),i) = FO_all(:,non_sz_obs_start(j,k)*hz_all(j):(non_sz_obs_start(j,k)+cut_length)*hz_all(j)-1,j);
        time_start_non_sz(i) = non_sz_obs_start(j,k);
        hz_all_non_sz(i) = hz_all(j);
        i = i + 1;
    end
end

unfiltered_all_observations = cat(3, unfiltered_sz_observations, unfiltered_non_sz_observations);
time_start = [time_start_sz; time_start_non_sz];
hz_all_obs = [hz_all_sz; hz_all_non_sz];
[eight, seconds_hz, tot_num_of_obs] = size(unfiltered_all_observations);
%%
% Filter the desired observations.
filtered_data_all = zeros(6,cut_length*512,tot_num_of_obs);

% I do not think time_start is necessary anymore, but I just left it in
% there to make sure I couldn't mess anything up.
for i = 1:tot_num_of_obs
    filtered_data_all(:,1:cut_length*hz_all_obs(i),i) = filter_data(unfiltered_all_observations(:,1:cut_length*hz_all_obs(i),i),hz_all_obs(i),cut_length);
end
%%
% Apply FFT on all observations.
fft_data_all = zeros(6,cut_length*512/2+1,tot_num_of_obs);
freq_buckets = zeros(tot_num_of_obs,length(512*(0:((cut_length*hz)/2))/(cut_length*512)));
%%
for i = 1:tot_num_of_obs
    [fft_data_all(:,1:(cut_length*hz_all_obs(i))/2+1,i), freq_buckets(i,1:cut_length*hz_all_obs(i)/2+1)] = fft_smooth(filtered_data_all(:,1:cut_length*hz_all_obs(i),i),hz_all_obs(i),cut_length);
end
%%
% Cut out 0 frequency bin.
fft_data_all = fft_data_all(:,2:end,:);
freq_buckets = freq_buckets(:,2:end);
%%
% Cut out all bins with frequencies greater than 50 Hz.
fft_data_all = fft_data_all(:,1:500,:);
freq_buckets = freq_buckets(:,1:500);
%%
% Cut, average, and append.
tidy_data = zeros(1,6*50);
for i = 1:tot_num_of_obs
    tidy_data(i,:) = cut_and_append(fft_data_all(:,:,i));
end
%%
% Append classifcations to the pseudo_tidy_data to make the tidy_data.

response_column = [ones(num_of_sz_obs,1); zeros(num_of_non_sz_obs,1)];
tidy_data(:,301) = response_column;
csvwrite('tidy_data.csv', tidy_data)

