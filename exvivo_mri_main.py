""" import modules """
import ants
import glob
import graphviz
import gzip
import hvplot.pandas
import importlib
import matplotlib.pyplot as plt
import nibabel as nib
import numpy as np
import os
import pandas as pd
import re
import seaborn as sns
import shutil
import subprocess
import sys

from nipy import load_image
from os.path import join as opj
from os.path import split as ops
from pathlib import Path
from shutil import copyfile

import dipy.reconst.dti as dti
import dipy.reconst.fwdti as fwdti
from dipy.io.gradients import read_bvals_bvecs
from dipy.core.gradients import gradient_table
#from dipy.segment.mask import median_otsu
#from dmipy.core.acquisition_scheme import gtab_dipy2dmipy
#from dmipy.core.acquisition_scheme import acquisition_scheme_from_bvalues
#from dmipy.core.modeling_framework import MultiCompartmentModel
#from dmipy.distributions.distribute_models import SD1WatsonDistributed
#from dmipy.signal_models import cylinder_models, gaussian_models



# unused modules (commented out)
"""
# add spinal cord toolbox modules
mymodules = os.fspath('../modules')
if mymodules not in sys.path:
    sys.path.append(mymodules)
mymodules = '/home/felix/Repos/sct/scripts'
if mymodules not in sys.path:
    sys.path.append(mymodules)
mymodules = '/home/felix/Repos/sct'
if mymodules not in sys.path:
    sys.path.append(mymodules)
import sct_utils as sct
"""
"""
#import general as g, bruker as b, relaxometry as r
#b = importlib.reload(b)
#g = importlib.reload(g)
#r = importlib.reload(r)
"""
"""
from ipywidgets import interact, fixed
"""


""" PATHS (PLEASE SPECIFY)"""
# project folder
PATH = '/media/gdavid/Seagate_Expansion_Drive/projects/small_animal_mri'
#PATH = 'F:\projects\small_animal_mri'

PATH_scripts = os.path.join(PATH, '04_scripts')
if PATH_scripts not in sys.path:
    sys.path.append(PATH_scripts)

""" Function definitions """
def register_atlasVBM(DIR_dw, DIR_t1):
    """
    Assumes scan has already been correctly flipped in I-S direction, so that slice 0 is inferior.
    Additional options for voxel-based morphometry (experimental)
    """
    os.chdir(DIR_dw)
    
    # get files
    # source
    epi_b0 = os.path.join(DIR_dw, 'DTI_EPI_30dir_sat_b0_swap_sl_swap.nii')
    fa     = os.path.join(DIR_dw, 'dti_wls', 'DTI_EPI_30dir_sat_FA_swap_sl_swap.nii')
    md     = os.path.join(DIR_dw, 'dti_wls', 'DTI_EPI_30dir_sat_MD_swap_sl_swap.nii')
    ad     = os.path.join(DIR_dw, 'dti_wls', 'DTI_EPI_30dir_sat_AD_swap_sl_swap.nii')
    rd     = os.path.join(DIR_dw, 'dti_wls', 'DTI_EPI_30dir_sat_RD_swap_sl_swap.nii')

    # target (t1)
    #fslroi $t1 "$(dirname $t1)"/"$(basename $t1 '.nii.gz')"_0.nii.gz 0 -1 0 -1 0 -1 0 1
    t1 = os.path.join(DIR_t1, 'T1map_RARE_cor_sl_swap_t0.nii.gz')
       
    #improved below:
#    parameters = ("step=0,type=label,dof=Tx_Ty_Tz_Sz:"
#              "step=1,type=imseg,algo=centermassrot,rot_method=pcahog:"
#              "step=2,type=im,algo=affine,metric=CC,slicewise=0:"
#              "step=3,type=im,algo=syn,metric=CC,iter=15,gradStep=0.3,slicewise=0")
         
    # below "gold standard" of 2019
 #   parameters = ("step=0,type=label,dof=Tx_Ty_Tz_Sz:"
  #            "step=1,type=im,algo=slicereg,metric=CC,iter=15,gradStep=0.3,smooth=0.1:"
  #            "step=2,type=im,algo=affine,metric=CC,slicewise=0:"
  #            "step=3,type=im,algo=bsplinesyn,metric=CC,iter=15,gradStep=0.3,slicewise=1")
    
    
    parameters = (
              "step=0,type=im,algo=affine,metric=CC,slicewise=0:"
              "step=1,type=im,algo=bsplinesyn,metric=CC,iter=15,gradStep=0.3,slicewise=1")

         
    # below without slicereg, causes big image jumps sometimes... but rarely
#    parameters = ("step=0,type=label,dof=Tx_Ty_Tz_Sz:"
#              "step=1,type=seg,algo=centermassrot,pca_eigenratio_th=1.3,poly=5:"
#              "step=2,type=seg,algo=columnwise:"
#              "step=3,type=im,algo=affine,metric=CC,slicewise=0:"
#              "step=4,type=im,algo=bsplinesyn,metric=CC,iter=15,gradStep=0.3,slicewise=1")        
       
    subprocess.run(['sct_register_multimodal', '-i', epi_b0, '-d', t1, '-ofolder', DIR_dw, '-param', parameters])
    
    
    return None





""" T1/T2 processing """

scanpath = 'D:\projects\small_animal_mri\01_data\03_processed\sub-SEV-193'

# T1 = list(Path(scanpath).glob('T1/**/T1map.nii.gz'))  #_subscan_0
# T2 = list(Path(scanpath).glob('T2/**/T2map.nii.gz'))
T1 = list(Path(scanpath).glob('T1/**/T1.nii.gz'))  # _subscan_0
T2 = list(Path(scanpath).glob('T2/**/T2.nii.gz'))


df1 = pd.DataFrame(T1, columns=['T1'])
df2 = pd.DataFrame(T2, columns=['T2'])
df = pd.concat([df1, df2], axis=1)
df

# df = pd.read_csv(Path(scanpath) / 'filepaths_T1T2.csv')
df


""" DTI (native space) """

# load in list of dwi folders to process
folders_dwi = os.path.join(
    PATH, '01_data', '03_derivatives', 'list_of_folders_dwi_short_linux.txt')
df_folders_dwi = pd.read_csv(folders_dwi, delimiter='\t', header=None)
df_folders_dwi.colname = 'folder name'

for i in range(df_folders_dwi.shape[0]):

    print(f"Processed subject: {df_folders_dwi.iloc[i,0]}")
    # load in dwi file
    dwi = os.path.join(PATH, '01_data', '03_processed',
                       df_folders_dwi.iloc[i, 0], 'DTI_EPI_30dir_sat.nii')
    dwi_img = nib.load(dwi)
    dwi_data = dwi_img.get_fdata()
    dwi_affine = dwi_img.affine
    dwi_header = dwi_img.header

    # load in bvals and bvecs
    bval_fname = os.path.join(PATH, '01_data', '03_processed',
                              df_folders_dwi.iloc[i, 0], 'DTI_EPI_30dir_sat_DwEffBval.txt')
    bvec_fname = os.path.join(PATH, '01_data', '03_processed',
                              df_folders_dwi.iloc[i, 0], 'DTI_EPI_30dir_sat_DwGradVec.txt')
    bvals, bvecs = read_bvals_bvecs(bval_fname, bvec_fname)

    # round bvals to nearest hundreds (necessary for creating the acquisition scheme for NODDI)
    bvals = bvals.round(-2)

    # normalize bvecs
    tmp = bvecs[:, 0]**2 + bvecs[:, 1]**2 + bvecs[:, 2]**2
    tmp = np.column_stack((tmp, tmp, tmp))
    bvecs = np.divide(bvecs, np.sqrt(tmp))
    bvecs[np.isnan(bvecs)] = 0

    # create gradient tables
    gtab = gradient_table(bvals, bvecs)
    gtab_b0800 = gradient_table(bvals[(bvals == 0) | (
        bvals == 800)], bvecs[(bvals == 0) | (bvals == 800)])
    gtab_b1600 = gradient_table(bvals[(bvals == 0) | (
        bvals == 1600)], bvecs[(bvals == 0) | (bvals == 1600)])

    """" fit DTI model """
    import dipy.reconst.dti as dti
    dti_wls = dti.TensorModel(gtab)
    fit_wls = dti_wls.fit(dwi_data)

    # fit using both shells
    evals = fit_wls.evals
    evecs = fit_wls.evecs
    fa = fit_wls.fa
    md = (evals[:, :, :, 0]+evals[:, :, :, 1]+evals[:, :, :, 2])/3
    ad = evals[:, :, :, 0]
    rd = (evals[:, :, :, 1]+evals[:, :, :, 2])/2

    # save DTI metric map
    if not os.path.exists(os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'dti_wls')):
        os.mkdir(os.path.join(PATH, '01_data', '03_processed',
                 df_folders_dwi.iloc[i, 0], 'dti_wls'))

    img_ = nib.Nifti1Image(fa, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls', 'DTI_EPI_30dir_sat_FA.nii'))
    img_ = nib.Nifti1Image(md, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls', 'DTI_EPI_30dir_sat_MD.nii'))
    img_ = nib.Nifti1Image(ad, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls', 'DTI_EPI_30dir_sat_AD.nii'))
    img_ = nib.Nifti1Image(rd, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls', 'DTI_EPI_30dir_sat_RD.nii'))

    # fit using b=800 shell only
    dti_wls = dti.TensorModel(gtab_b0800)
    fit_wls = dti_wls.fit(dwi_data[:, :, :, (bvals == 0) | (bvals == 800)])
    evals = fit_wls.evals
    evecs = fit_wls.evecs
    fa = fit_wls.fa
    md = (evals[:, :, :, 0]+evals[:, :, :, 1]+evals[:, :, :, 2])/3
    ad = evals[:, :, :, 0]
    rd = (evals[:, :, :, 1]+evals[:, :, :, 2])/2
    if not os.path.exists(os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'dti_wls_b0800')):
        os.mkdir(os.path.join(PATH, '01_data', '03_processed',
                 df_folders_dwi.iloc[i, 0], 'dti_wls_b0800'))
    img_ = nib.Nifti1Image(fa, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls_b0800', 'DTI_EPI_30dir_sat_FA.nii'))
    img_ = nib.Nifti1Image(md, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls_b0800', 'DTI_EPI_30dir_sat_MD.nii'))
    img_ = nib.Nifti1Image(ad, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls_b0800', 'DTI_EPI_30dir_sat_AD.nii'))
    img_ = nib.Nifti1Image(rd, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls_b0800', 'DTI_EPI_30dir_sat_RD.nii'))

    # fit using b=1600 shell only
    dti_wls = dti.TensorModel(gtab_b1600)
    fit_wls = dti_wls.fit(dwi_data[:, :, :, (bvals == 0) | (bvals == 1600)])
    evals = fit_wls.evals
    evecs = fit_wls.evecs
    fa = fit_wls.fa
    md = (evals[:, :, :, 0]+evals[:, :, :, 1]+evals[:, :, :, 2])/3
    ad = evals[:, :, :, 0]
    rd = (evals[:, :, :, 1]+evals[:, :, :, 2])/2
    img_ = nib.Nifti1Image(fa, dwi_affine)
    if not os.path.exists(os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'dti_wls_b1600')):
        os.mkdir(os.path.join(PATH, '01_data', '03_processed',
                 df_folders_dwi.iloc[i, 0], 'dti_wls_b1600'))
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls_b1600', 'DTI_EPI_30dir_sat_FA.nii'))
    img_ = nib.Nifti1Image(md, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls_b1600', 'DTI_EPI_30dir_sat_MD.nii'))
    img_ = nib.Nifti1Image(ad, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls_b1600', 'DTI_EPI_30dir_sat_AD.nii'))
    img_ = nib.Nifti1Image(rd, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'dti_wls_b1600', 'DTI_EPI_30dir_sat_RD.nii'))

    """ fit NODDI model (NODDI-Watson (Zhang et al., 2012)) """

    # generate acquisition scheme
    from dmipy.core.acquisition_scheme import acquisition_scheme_from_bvalues
    bvals = bvals*1e6  # dmipy requires bvals in units of s/m^2
    dmipy_scheme = acquisition_scheme_from_bvalues(bvals, bvecs)
    dmipy_scheme.print_acquisition_info

    # instantiation of the model
    from dmipy.signal_models import cylinder_models, gaussian_models
    ball = gaussian_models.G1Ball()  # ball for the CSF compartment
    stick = cylinder_models.C1Stick()  # stick for the intra-axonal compartment
    # Zeppelin for the extra-axonal compartment
    zeppelin = gaussian_models.G2Zeppelin()

    # Watson-disperse the stick and zeppelin together, making a representation for a dispersed single axon bundle
    from dmipy.distributions.distribute_models import SD1WatsonDistributed
    watson_dispersed_bundle = SD1WatsonDistributed(models=[stick, zeppelin])

    watson_dispersed_bundle.parameter_names

    # setting parameters
    watson_dispersed_bundle.set_tortuous_parameter(
        'G2Zeppelin_1_lambda_perp', 'C1Stick_1_lambda_par', 'partial_volume_0')
    watson_dispersed_bundle.set_equal_parameter(
        'G2Zeppelin_1_lambda_par', 'C1Stick_1_lambda_par')
    watson_dispersed_bundle.set_fixed_parameter(
        'G2Zeppelin_1_lambda_par', 1.7e-9)

    # putting together the model
    from dmipy.core.modeling_framework import MultiCompartmentModel
    from dmipy.core.modeling_framework import MultiCompartmentSphericalHarmonicsModel
    NODDI_watson = MultiCompartmentModel(
        models=[ball, watson_dispersed_bundle])
  #  MultiCompartmentSphericalHarmonicsModel(models=[wm])
    NODDI_watson.parameter_names

    # fix the diffusivity of the Ball compartment to static values
    NODDI_watson.set_fixed_parameter('G1Ball_1_lambda_iso', 3e-9)

    # visualize the model
 #   from IPython.display import Image
 #   NODDI_mod.visualize_model_setup(view=False, cleanup=False)
 #   Image('Model Setup.png')

    # fit the model
    import pathos
    NODDI_watson_fit = NODDI_watson.fit(
        dmipy_scheme, dwi_data, mask=dwi_data[..., 0] > 800, use_parallel_processing=True, number_of_processors=6)

    # get the parameters
    mu = NODDI_watson_fit.fitted_parameters['SD1WatsonDistributed_1_SD1Watson_1_mu']
    odi = NODDI_watson_fit.fitted_parameters['SD1WatsonDistributed_1_SD1Watson_1_odi']
    watson_partial_volume_0 = NODDI_watson_fit.fitted_parameters[
        'SD1WatsonDistributed_1_partial_volume_0']
    partial_volume_0 = NODDI_watson_fit.fitted_parameters['partial_volume_0']
    partial_volume_1 = NODDI_watson_fit.fitted_parameters['partial_volume_1']

    # save NODDI maps
    img_ = nib.Nifti1Image(mu, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'noddi_watson', 'DTI_EPI_30dir_sat_mu.nii'))
    img_ = nib.Nifti1Image(odi, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'noddi_watson', 'DTI_EPI_30dir_sat_odi.nii'))
    img_ = nib.Nifti1Image(watson_partial_volume_0, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'noddi_watson', 'DTI_EPI_30dir_sat_watson_partial_volume_0.nii'))
    img_ = nib.Nifti1Image(partial_volume_0, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'noddi_watson', 'DTI_EPI_30dir_sat_partial_volume_0.nii'))
    img_ = nib.Nifti1Image(partial_volume_1, dwi_affine)
    nib.save(img_, os.path.join(PATH, '01_data', '03_processed',
             df_folders_dwi.iloc[i, 0], 'noddi_watson', 'DTI_EPI_30dir_sat_partial_volume_1.nii'))

   

    """ reorient DTI metrics, similar to the T1 and T2 images """

    # grab files
    t1 = os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0][:-12], 'T1map_RARE', df_folders_dwi.iloc[i, 0][-1], 'T1map_RARE_cor_sl_swap_t0.nii.gz')
    t1_flash = os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0][:-12], 'T1_FLASH_3D', df_folders_dwi.iloc[i, 0][-1], 'T1_FLASH_3D_iso.nii')
    epi_b0 = os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'DTI_EPI_30dir_sat_b0.nii')
    fa = os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'dti_wls', 'DTI_EPI_30dir_sat_FA.nii')
    md = os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'dti_wls', 'DTI_EPI_30dir_sat_MD.nii')
    ad = os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'dti_wls', 'DTI_EPI_30dir_sat_AD.nii')
    rd = os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'dti_wls', 'DTI_EPI_30dir_sat_RD.nii')
    
    # Step 1: reorder dimensions
    subprocess.run(['fslswapdim', f"{t1_flash[:-4]}.nii", 'x', '-z', 'y', f"{t1_flash[:-4]}_swap.nii"])
    subprocess.run(['fslswapdim', f"{epi_b0[:-4]}.nii", 'x', '-z', 'y', f"{epi_b0[:-4]}_swap.nii"])
    subprocess.run(['fslswapdim', f"{fa[:-4]}.nii", 'x', '-z', 'y', f"{fa[:-4]}_swap.nii"])
    subprocess.run(['fslswapdim', f"{md[:-4]}.nii", 'x', '-z', 'y', f"{md[:-4]}_swap.nii"])
    subprocess.run(['fslswapdim', f"{ad[:-4]}.nii", 'x', '-z', 'y', f"{ad[:-4]}_swap.nii"])
    subprocess.run(['fslswapdim', f"{rd[:-4]}.nii", 'x', '-z', 'y', f"{rd[:-4]}_swap.nii"])
    
    # Step 2: remove padding slices in the anterior-posterior directions (to speed up computation)
    from exvivo_mri_func_general import slice_AP
    slice_AP(f"{t1_flash[:-4]}_swap.nii.gz", f"{t1_flash[:-4]}_swap_sl.nii")
    slice_AP(f"{epi_b0[:-4]}_swap.nii.gz", f"{epi_b0[:-4]}_swap_sl.nii")
    slice_AP(f"{fa[:-4]}_swap.nii.gz", f"{fa[:-4]}_swap_sl.nii")
    slice_AP(f"{md[:-4]}_swap.nii.gz", f"{md[:-4]}_swap_sl.nii")
    slice_AP(f"{ad[:-4]}_swap.nii.gz", f"{ad[:-4]}_swap_sl.nii")
    slice_AP(f"{rd[:-4]}_swap.nii.gz", f"{rd[:-4]}_swap_sl.nii")

    
    # Step 3: change the order of dimensions
    copyfile(f"{t1_flash[:-4]}_swap_sl.nii", f"{t1_flash[:-4]}_swap_sl_swap.nii")
    copyfile(f"{epi_b0[:-4]}_swap_sl.nii", f"{epi_b0[:-4]}_swap_sl_swap.nii")
    copyfile(f"{fa[:-4]}_swap_sl.nii", f"{fa[:-4]}_swap_sl_swap.nii")
    copyfile(f"{md[:-4]}_swap_sl.nii", f"{md[:-4]}_swap_sl_swap.nii")
    copyfile(f"{ad[:-4]}_swap_sl.nii", f"{ad[:-4]}_swap_sl_swap.nii")
    copyfile(f"{rd[:-4]}_swap_sl.nii", f"{rd[:-4]}_swap_sl_swap.nii")
    
    subprocess.run(['fslorient', '-deleteorient', f"{t1_flash[:-4]}_swap_sl_swap.nii"])
    subprocess.run(['fslorient', '-deleteorient', f"{epi_b0[:-4]}_swap_sl_swap.nii"])
    subprocess.run(['fslorient', '-deleteorient', f"{fa[:-4]}_swap_sl_swap.nii"])
    subprocess.run(['fslorient', '-deleteorient', f"{md[:-4]}_swap_sl_swap.nii"])
    subprocess.run(['fslorient', '-deleteorient', f"{ad[:-4]}_swap_sl_swap.nii"])
    subprocess.run(['fslorient', '-deleteorient', f"{rd[:-4]}_swap_sl_swap.nii"])
  
    orient = 'LPI'
    subprocess.run(['sct_image', '-i', f"{t1_flash[:-4]}_swap_sl_swap.nii",'-setorient', orient, '-o', f"{t1_flash[:-4]}_swap_sl_swap.nii"])
    subprocess.run(['sct_image', '-i', f"{epi_b0[:-4]}_swap_sl_swap.nii",'-setorient', orient, '-o', f"{epi_b0[:-4]}_swap_sl_swap.nii"])
    subprocess.run(['sct_image', '-i', f"{fa[:-4]}_swap_sl_swap.nii",'-setorient', orient, '-o', f"{fa[:-4]}_swap_sl_swap.nii"])
    subprocess.run(['sct_image', '-i', f"{md[:-4]}_swap_sl_swap.nii",'-setorient', orient, '-o', f"{md[:-4]}_swap_sl_swap.nii"])
    subprocess.run(['sct_image', '-i', f"{ad[:-4]}_swap_sl_swap.nii",'-setorient', orient, '-o', f"{ad[:-4]}_swap_sl_swap.nii"])
    subprocess.run(['sct_image', '-i', f"{rd[:-4]}_swap_sl_swap.nii",'-setorient', orient, '-o', f"{rd[:-4]}_swap_sl_swap.nii"])
    
    # Step 4: manually reorient images in SPM
    
    # Step 5: co-register DTI to T1 (T1_cor_sl.nii)
    #subprocess.run(['flirt3D.sh', os.path.dirname(epi_b0), os.path.dirname(t1)])
    #subprocess.run(['flirt2D.sh', os.path.dirname(epi_b0), os.path.dirname(t1)])
    
    parameters = ("step=0,type=im,algo=rigid,slicewise=1:"
                  "step=1,type=im,algo=centermassrot:"
                  "step=2,type=im,algo=slicereg,iter=15,gradStep=0.2,smooth=0.1")
    
    subprocess.run(['sct_register_multimodal', '-i', f"{epi_b0[:-4]}_swap_sl_swap_reorient.nii", '-d', t1,
                    '-param', parameters, '-ofolder', os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0])])
    
    os.chdir(os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'dti_wls'))
    subprocess.run(['sct_apply_transfo', '-i', f"{fa[:-4]}_swap_sl_swap_reorient.nii", '-d', t1,
                    '-w', os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'warp_DTI_EPI_30dir_sat_b0_swap_sl_swap_reorient2T1map_RARE_cor_sl_swap_t0.nii.gz')])
    subprocess.run(['sct_apply_transfo', '-i', f"{md[:-4]}_swap_sl_swap_reorient.nii", '-d', t1,
                    '-w', os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'warp_DTI_EPI_30dir_sat_b0_swap_sl_swap_reorient2T1map_RARE_cor_sl_swap_t0.nii.gz')])
    subprocess.run(['sct_apply_transfo', '-i', f"{ad[:-4]}_swap_sl_swap_reorient.nii", '-d', t1,
                    '-w', os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'warp_DTI_EPI_30dir_sat_b0_swap_sl_swap_reorient2T1map_RARE_cor_sl_swap_t0.nii.gz')])
    subprocess.run(['sct_apply_transfo', '-i', f"{rd[:-4]}_swap_sl_swap_reorient.nii", '-d', t1,
                    '-w', os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0], 'warp_DTI_EPI_30dir_sat_b0_swap_sl_swap_reorient2T1map_RARE_cor_sl_swap_t0.nii.gz')])
    
    os.remove(os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i,0], 'T1map_RARE_cor_sl_swap_t0_reg.nii.gz'))
    os.remove(os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i,0], 'warp_T1map_RARE_cor_sl_swap_t02DTI_EPI_30dir_sat_b0_swap_sl_swap_reorient.nii.gz'))
    
    
    
    # Step 6: normalize to template

    # destination image (template)
    file_destination = os.path.join(PATH, '01_data', 'AtlasRat', 'AtlasRat_MVF_swapped.nii')
    
    # warp fields to initialize
    file_warp_init = os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0][:-12],
                             'T1map_RARE',
                             df_folders_dwi.iloc[i, 0][-1],
                             'coreg',
                             'warp_T1map_RARE_cor_sl_swap_t02AtlasRat_MVF_ISswapped.nii.gz')
    
    file_warp_inv_init = os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0][:-12],
                             'T1map_RARE',
                             df_folders_dwi.iloc[i, 0][-1],
                             'coreg',
                             'warp_AtlasRat_MVF_ISswapped2T1map_RARE_cor_sl_swap_t0.nii.gz')
    
    # registration parameters
    parameters = ("step=0,type=im,dof=Tx_Ty_Tz_Sz:"
                  "step=1,type=im,algo=centermassrot:"
                  "step=2,type=im,algo=columnwise:"
                  "step=3,type=im,algo=bsplinesyn,metric=CC,iter=15,gradStep=0.3,smooth=0.1,slicewise=1")

    
    # run normalization
    subprocess.run(['sct_register_multimodal', '-i', f"{epi_b0[:-4]}_swap_sl_swap_reorient_reg.nii",
                    '-d', file_destination, '-initwarp', file_warp_init, '-initwarpinv', file_warp_inv_init,
                    '-o', f"{epi_b0[:-4]}_swap_sl_swap_reorient_reg_norm.nii",'-ofolder',
                    os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i,0]), '-param', parameters])
    

    # apply normalization
    file_warp = os.path.join(PATH, '01_data', '03_processed', df_folders_dwi.iloc[i, 0],'warp_DTI_EPI_30dir_sat_b0_swap_sl_swap_reorient_reg2AtlasRat_MVF_swapped.nii.gz')
    
    subprocess.run(['sct_apply_transfo', '-i', f"{fa[:-4]}_swap_sl_swap_reorient_reg.nii", '-d', file_destination, '-w', file_warp, '-o', f"{fa[:-4]}_swap_sl_swap_reorient_reg_space-template.nii"])
    subprocess.run(['sct_apply_transfo', '-i', f"{md[:-4]}_swap_sl_swap_reorient_reg.nii", '-d', file_destination, '-w', file_warp, '-o', f"{md[:-4]}_swap_sl_swap_reorient_reg_space-template.nii"])
    subprocess.run(['sct_apply_transfo', '-i', f"{ad[:-4]}_swap_sl_swap_reorient_reg.nii", '-d', file_destination, '-w', file_warp, '-o', f"{ad[:-4]}_swap_sl_swap_reorient_reg_space-template.nii"])
    subprocess.run(['sct_apply_transfo', '-i', f"{rd[:-4]}_swap_sl_swap_reorient_reg.nii", '-d', file_destination, '-w', file_warp, '-o', f"{rd[:-4]}_swap_sl_swap_reorient_reg_space-template.nii"])
