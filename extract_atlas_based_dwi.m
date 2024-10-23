% The script extracts mean values of DTI metrics within GM and WM regions and returns a
% table with the results.

% load in folders to analyze
folders = table2cell(readtable('F:\projects\small_animal_mri\01_data\03_derivatives\list_of_folders_dwi_short.txt'));

% specify images and masks used for analysis
images = {'AD';'FA';'MD';'RD'};
images_full = compose('DTI_EPI_30dir_sat_%s_swap_sl_swap_reorient_reg', string(images));

masks = {'AtlasRat_swapped_mask_gm_eroded1x_space-native';
         'AtlasRat_swapped_mask_gm_dor_eroded1x_space-native';
         'AtlasRat_swapped_mask_gm_ven_eroded1x_space-native';
         'AtlasRat_swapped_mask_wm_eroded1x_space-native';
         'AtlasRat_swapped_mask_wm_dor_eroded1x_space-native';
         'AtlasRat_swapped_mask_wm_lat_eroded1x_space-native';
         'AtlasRat_swapped_mask_wm_ven_eroded1x_space-native'};

for i = 1:size(masks,1)
    disp(masks{i})
    
    for j = 1:size(images,1)
        disp(images{j})

        result = table;
        result.SpinalLevels = (1:31)';

        for k = 1:size(folders,1)
            disp(folders{k})
            
            % load in spinal levels
            I_levels = spm_read_vols(spm_vol([folders{k} filesep 'masked_eroded1x' filesep 'masks_space-native' filesep 'spinallevel_space-native.nii']));
            maxs = NaN(size(I_levels,3),1);

            for z = 1:size(I_levels,3)
                maxs(z) = max(nonzeros(I_levels(:,:,z)));
            end
            
            % load in masked image
            I_masked = spm_read_vols(spm_vol([folders{k} filesep 'masked_eroded1x' filesep  images_full{j} '_' masks{i} '.nii']));
            means = NaN(31,1);
            for sl = 1:size(I_levels,3)
                bool = maxs == sl;
                if max(bool) > 0
                    sl_mat = zeros(size(I_levels));
                    for b = 1:size(I_levels,3)
                        sl_mat(:,:,b) = sl_mat(:,:,b) + bool(b).*I_masked(:,:,b);
                    end
                    means(sl) = mean(nonzeros(sl_mat));
                end
            end

            name = [folders{k}(53:63) '_' folders{k}(75)];
            result.(name) = means;
            csv_name = ['F:\projects\small_animal_mri\05_results\01_atlas_based\DTI' filesep 'eroded_1x' filesep 'table_' images{j} '_' masks{i} 'ero.csv'];
            writetable(rows2vars(result), csv_name)
        end
    end
end

