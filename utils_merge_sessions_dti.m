clc; clear;

load('utils_merge_sessions_dti_filelist.mat')

%for i = 1:length(sub_scan.path)
%    sub_scan.path{i} = sprintf('/media/chkuendig/chkuendig/%s', sub_scan.path{i}(37:74));
%end

subject = table;
k = 1;
subject.name = cell(26,1);
subject.two = false(26,1);

subject.name{k} = sprintf('%s', sub_scan.path{1}(53:63));
for i = 2:length(sub_scan.path)
    subject.name{k+1} = sprintf('%s', sub_scan.path{i}(53:63));

    if strcmp(subject.name{k}, subject.name{k+1})
        subject.two(k) = 1;
    else
        k = k+1;
    end
end
subject(k+1:26,:) = [];
subject_num = length(subject.name);

subject.last1slice = [10 8 1 9 9 9 9 1 8 9 5 7 9 9 7 6 1 1 10 1 8 8 9]'-1;
subject.last0slice = 31-subject.last1slice;

images = {'AD' 'FA' 'MD' 'RD'};
images_num = length(images);

% subject_num = 1; 
% images_num = 1;

pathindex = 1;


for b = 1:subject_num
    disp(subject.name{b})
    for a = 1:images_num
        disp(images{a})
        try
            X0 = spm_vol([sub_scan.path{pathindex} filesep 'dti_wls' filesep 'DTI_EPI_30dir_sat_' images{a} '_swap_sl_swap_reorient_reg_space-template.nii']);
            Y0 = spm_read_vols(X0);
            indexadd = 1;
    
            if subject.two(b)
                X1 = spm_vol([sub_scan.path{pathindex+1} filesep 'dti_wls' filesep 'DTI_EPI_30dir_sat_' images{a} '_swap_sl_swap_reorient_reg_space-template.nii']);
                Y1 = spm_read_vols(X1);
                for c = subject.last0slice+1:31
                    Y0(:,:,c) = Y1(:,:,c);
                end
                indexadd = 2;
                
            end
            X0.fname = ['F:\projects\small_animal_mri\01_data\03_derivatives\' filesep subject.name{b} filesep 'dwi_short\DTI_EPI_30dir_sat_' images{a} '_swap_sl_swap_reorient_reg_space-template_merged.nii'];
            subject.merged{b} = ['F:\projects\small_animal_mri\01_data\03_derivatives' filesep subject.name{b} filesep 'dwi_short'];
            spm_write_vol(X0, Y0);
        catch
          %  fprintf('%s\dti_wls\DTI_EPI_30dir_sat_%s_swap_sl_swap_reorient_reg_space-template.nii failed', sub_scan.path{pathindex}, images{a})
            disp(' ')
            indexadd = 1;
            if subject.two(b)
                indexadd = 2;
            end
        end
    end
    pathindex = pathindex+indexadd;
    disp(' ')

end

