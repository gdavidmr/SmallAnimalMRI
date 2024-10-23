clear all;

%% load in dataframe
% specify excel file
FILE = 'table_ad_gm.csv';
FOLDER = 'F:\projects\small_animal_mri\05_results\01_atlas_based\DTI\eroded_1x';

df = table2cell(readtable([FOLDER filesep FILE]));

%% create merged dataframe
% header
df_new(1,:)    = df(1,:);

% MLD-212
df_new(2,:)    = df(2,:);
df_new(2,2:10) = df(3,2:10);

% MLD-216
df_new(3,:)    = df(4,:);
df_new(3,2:8)  = df(5,2:8);

% MLD-217
df_new(4,:)    = df(6,:);

% MLD-220
df_new(5,:)    = df(7,:);
df_new(5,2:9)  = df(8,2:9);

% MLD-229
df_new(6,:)    = df(9,:);
df_new(6,2:9)  = df(10,2:9);

% MLD-238
df_new(7,:)    = df(11,:);
df_new(7,2:9)  = df(12,2:9);

% MLD-241
df_new(8,:)    = df(13,:);
df_new(8,2:9)  = df(14,2:9);

% MOD-210
df_new(9,:)    = df(15,:);

% MOD-213
df_new(10,:)   = df(16,:);
df_new(10,2:8) = df(17,2:8);

% MOD-214
df_new(11,:)   = df(18,:);
df_new(11,2:9) = df(19,2:9);

% MOD-219
df_new(12,:)   = df(20,:);
df_new(12,2:6) = df(21,2:6);

% MOD-230
df_new(13,:)   = df(22,:);
df_new(13,2:8) = df(23,2:8);

% MOD-231
df_new(14,:)   = df(24,:);
df_new(14,2:9) = df(25,2:9);

% MOD-234
df_new(15,:)   = df(26,:);
df_new(15,2:9) = df(27,2:9);

% MOD-240
df_new(16,:)   = df(28,:);
df_new(16,2:8) = df(29,2:8);

% SEV-193
df_new(17,:)   = df(30,:);

% SEV-198
df_new(18,:)   = df(31,:);

% SEV-236
df_new(19,:)   = df(32,:);
df_new(19,2:7) = df(33,2:7);

% SHM-194
df_new(20,:)   = df(34,:);
df_new(20,2:10)= df(35,2:10);

% SHM-199
df_new(21,:)   = df(36,:);

% SHM-203
df_new(22,:)   = df(37,:);
df_new(22,2:9) = df(38,2:9);

% SHM-221
df_new(23,:)   = df(39,:);
df_new(23,2:8) = df(40,2:8);

% SHM-239
df_new(24,:)   = df(41,:);
df_new(24,2:9) = df(42,2:9);

%% do further minor modifications
df_new(:,17:end) = [];

df_new2(:,1) = {'id',212,216,217,220,229,238,241,210,213,214,219,230,231,234,240,193,198,236,194,199,203,221,239};
df_new2(:,2) = {'group','MLD','MLD','MLD','MLD','MLD','MLD','MLD','MOD','MOD','MOD','MOD','MOD','MOD','MOD','MOD','SEV','SEV','SEV','SHM','SHM','SHM','SHM','SHM'};
df_new2(:,3) = {'group_bin',1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0};
df_new2(:,4:18) = df_new(:,2:16);

%% save new dataframe
% convert cell array to a table and use first row as variable names
for i = 1:size(df_new2,2)
    varnames(i) = string(df_new2{1,i}); 
end
df_new3 = cell2table(df_new2(2:end,:),'VariableNames',varnames);
 
% write the table to a csv file
writetable(df_new3,[FOLDER filesep FILE(1:5) '-merged' FILE(6:end)])