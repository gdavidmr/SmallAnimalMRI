#!/bin/bash
echo $BASH_VERSION

# folder with the binary masks and spinal levels
template_dir=/media/gdavid/Seagate_Expansion_Drive/projects/small_animal_mri/01_data/AtlasRat

# warping field from template to native space (dti)
warp=warp_AtlasRat_MVF_swapped2DTI_EPI_30dir_sat_b0_swap_sl_swap_reorient_reg.nii.gz

# get list from file and remove/add to get proper paths, results needs to be
# a list of dir in which the images are.
sub_scan_list=/media/gdavid/Seagate_Expansion_Drive/projects/small_animal_mri/01_data/03_derivatives/list_of_folders_dwi_short_linux_v2.txt
readarray -t sub_scan < $sub_scan_list
sub_scan_num="${#sub_scan[@]}"

images=("AD" "FA" "MD" "RD")
images_num="${#images[@]}"

masks=("AtlasRat_swapped_mask_gm" "AtlasRat_swapped_mask_gm_dor" "AtlasRat_swapped_mask_gm_ven" \
"AtlasRat_swapped_mask_wm" "AtlasRat_swapped_mask_wm_dor" "AtlasRat_swapped_mask_wm_lat" "AtlasRat_swapped_mask_wm_ven")

masks_eroded1x=("AtlasRat_swapped_mask_gm_eroded1x" "AtlasRat_swapped_mask_gm_dor_eroded1x" "AtlasRat_swapped_mask_gm_ven_eroded1x" \
"AtlasRat_swapped_mask_wm_eroded1x" "AtlasRat_swapped_mask_wm_dor_eroded1x" "AtlasRat_swapped_mask_wm_lat_eroded1x" "AtlasRat_swapped_mask_wm_ven_eroded1x")

masks_num="${#masks[@]}"

for (( l = 0; l < sub_scan_num; l++ )); do #runs over the sub and scan

	echo ${sub_scan[$l]::-1}

	# create folder for masks in the native space
	mkdir -p ${sub_scan[$l]::-1}/masked/masks_space-native
	mkdir -p ${sub_scan[$l]::-1}/masked_eroded1x/masks_space-native

	# transform spinal levels into the native space
	sct_apply_transfo -i $template_dir/AtlasRat_spinal_levels.nii \
		-d ${sub_scan[$l]::-1}/dti_wls/DTI_EPI_30dir_sat_AD_swap_sl_swap_reorient_reg.nii \
		-w ${sub_scan[$l]::-1}/$warp \
		-x nn \
		-o ${sub_scan[$l]::-1}/masked/masks_space-native/spinallevel_space-native.nii

	sct_apply_transfo -i $template_dir/AtlasRat_spinal_levels.nii \
		-d ${sub_scan[$l]::-1}/dti_wls/DTI_EPI_30dir_sat_AD_swap_sl_swap_reorient_reg.nii \
		-w ${sub_scan[$l]::-1}/$warp \
		-x nn \
		-o ${sub_scan[$l]::-1}/masked_eroded1x/masks_space-native/spinallevel_space-native.nii

	for (( i = 0; i < masks_num; i++ )); do #runs over the 5 masks
		echo ${masks[$i]}

		# transform masks into the native space
		sct_apply_transfo \
			-i $template_dir/${masks[$i]}.nii \
			-d ${sub_scan[$l]::-1}/dti_wls/DTI_EPI_30dir_sat_AD_swap_sl_swap_reorient_reg.nii \
			-w ${sub_scan[$l]::-1}/$warp \
			-x nn \
			-o ${sub_scan[$l]::-1}/masked/masks_space-native/${masks[$i]}_space-native.nii

		sct_apply_transfo \
			-i $template_dir/${masks_eroded1x[$i]}.nii \
			-d ${sub_scan[$l]::-1}/dti_wls/DTI_EPI_30dir_sat_AD_swap_sl_swap_reorient_reg.nii \
			-w ${sub_scan[$l]::-1}/$warp \
			-x nn \
			-o ${sub_scan[$l]::-1}/masked_eroded1x/masks_space-native/${masks_eroded1x[$i]}_space-native.nii

		# create masked images using the masks in the native space
		for (( k = 0; k < images_num; k++ )); do #runs over the 4 images in each sub_scan
			name=DTI_EPI_30dir_sat_${images[$k]}_swap_sl_swap_reorient_reg.nii
			echo $name

			sct_maths \
				-i ${sub_scan[$l]::-1}/dti_wls/$name \
				-mul ${sub_scan[$l]::-1}/masked/masks_space-native/${masks[$i]}_space-native.nii \
				-o ${sub_scan[$l]::-1}/masked/DTI_EPI_30dir_sat_${images[$k]}_swap_sl_swap_reorient_reg_${masks[$i]}_space-native.nii

			sct_maths \
				-i ${sub_scan[$l]::-1}/dti_wls/$name \
				-mul ${sub_scan[$l]::-1}/masked_eroded1x/masks_space-native/${masks_eroded1x[$i]}_space-native.nii \
				-o ${sub_scan[$l]::-1}/masked_eroded1x/DTI_EPI_30dir_sat_${images[$k]}_swap_sl_swap_reorient_reg_${masks_eroded1x[$i]}_space-native.nii
		done

	done

done

echo "======================================"
echo "!!!!!!!!!!!!===> done <===!!!!!!!!!!!!"
echo "======================================"
