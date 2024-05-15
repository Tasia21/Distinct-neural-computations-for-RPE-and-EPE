
%% separate svm model for separate PE pattern

% RPE
clear all
basedir = 'fmri\RPE_1stdat\';
gray_matter_mask = 'fmri\myGM_mask.nii';
puc_imgs = spm_select('FPListRec', basedir, 'con_0002.nii'); %under punishment decision
acc_imgs = spm_select('FPListRec', basedir, 'con_0008.nii');%under accept decision
data = fmri_data([puc_imgs; acc_imgs], gray_matter_mask);
data.Y = [ones(43,1); -ones(43,1)]; %P: 1, A: -1
[~, stats] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr');
n_folds = [(1:43) (1:43)];
n_folds = n_folds(:); 
[~, stats_loso] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', n_folds, 'error_type', 'mcr');
ROC_loso = roc_plot(stats_loso.dist_from_hyperplane_xval, data.Y == 1, 'threshold', 0);
cd('fmri\mvparesults\')
save('All_Prediction_results_PA_RPE.mat','ROC_loso','stats','stats_loso','data')
%%save unthresholded figures
obj=fmri_data(stats_loso.weight_obj)
write(obj,'fname','wholeprediction_unthresholded_PA_RPE.nii')
%threshold visualization
[~, stats_boot] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr', 'bootweights', 'bootsamples', 5000,'useparallel',4);
save('All_boot5000_results_PA_RPE.mat','stats_boot','data')
filedir='fmri\mvparesults\'
data_threshold = threshold(stats_boot.weight_obj, .001, 'unc'); 
data_threshold.fullpath = fullfile(filedir, 'svm_bootstrap5000_results_unc001_PA_RPE.nii'); 
write(data_threshold, 'thresh');

filedir='fmri\mvparesults\'
data_threshold = threshold(stats_boot.weight_obj, .05, 'fdr'); 
data_threshold.fullpath = fullfile(filedir, 'svm_bootstrap5000_results_fdr05_PA_RPE.nii'); 
write(data_threshold, 'thresh');
%figure plot
clear,clc;
close all


ROC_loso = roc_plot(stats_loso.dist_from_hyperplane_xval, data.Y == 1,  'color', 'r');
% txt=['ACC=',num2str(ROC_loso.accuracy),' |p<0.0001']
% text(0.4,0.4,txt)
set(gca, 'fontsize', 12, 'linewidth', 1.5, 'ticklength', [.01 .01]);
set(gcf, 'color', 'w', 'position', [200   158   288  258]);
% box off;

export_fig SEPUC-ACC_RPE_rocplot -tiff -r400

col=[1,0.38,0.27;0.0588,0.80,1]
out = plot_specificity_box(stats_loso.dist_from_hyperplane_xval(1:43,:), stats_loso.dist_from_hyperplane_xval(44:86,:), 'color',col);
xticklabels({'P(Reject)', 'P(Accept)'});
box on;
set(gca, 'fontsize', 18, 'linewidth', 2, 'ticklength', [.01 .01],'tickdir', 'in');
set(gcf, 'position', [200   258   388  358]);

[h,p,ci,stats]=ttest(stats_loso.dist_from_hyperplane_xval(1:43,:), stats_loso.dist_from_hyperplane_xval(44:86,:))

export_fig SEPUC-ACC_RPE_boxplot -tiff -r400


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% APE
clear all, close all
basedir = 'fmri\APE_1stdat\';
gray_matter_mask = 'fmri\myGM_mask.nii';
puc_imgs = spm_select('FPListRec', basedir, 'con_0004.nii');
acc_imgs = spm_select('FPListRec', basedir, 'con_0010.nii');
data = fmri_data([puc_imgs; acc_imgs], gray_matter_mask);
data.Y = [ones(43,1); -ones(43,1)]; %P: 1, A: -1
[~, stats] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr');
n_folds = [(1:43) (1:43)];
n_folds = n_folds(:); 
[~, stats_loso] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', n_folds, 'error_type', 'mcr');
ROC_loso = roc_plot(stats_loso.dist_from_hyperplane_xval, data.Y == 1, 'threshold', 0,'noplot');
cd('fmri\mvparesults\')
save('All_Prediction_results_PA_APE.mat','ROC_loso','stats','stats_loso','data')
%%save unthresholded figures
obj=fmri_data(stats_loso.weight_obj)
write(obj,'fname','wholeprediction_unthresholded_PA_APE.nii')

%not significant ACC, thus do not need do bootstrapping-sig test
% txt=['ACC=',num2str(ROC_loso.accuracy),' p=',num2str(ROC_loso.accuracy_p  )]
% text(0.4,0.4,txt)
ROC_loso = roc_plot(stats_loso.dist_from_hyperplane_xval, data.Y == 1,  'color', 'black');
set(gca, 'fontsize', 18, 'linewidth', 2, 'ticklength', [.01 .01], 'tickdir', 'out');
set(gcf, 'color', 'w', 'position', [200   258   388  358]);
box off;
export_fig separampucacc_svc_ape -tiff -r400

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VPE
clear all, close all
basedir = 'fmri\VPE_1stdat\';
gray_matter_mask = 'fmri\myGM_mask.nii';
puc_imgs = spm_select('FPListRec', basedir, 'con_0006.nii');
acc_imgs = spm_select('FPListRec', basedir, 'con_0012.nii');
data = fmri_data([puc_imgs; acc_imgs], gray_matter_mask);
data.Y = [ones(43,1); -ones(43,1)]; %P: 1, A: -1
[~, stats] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr');
n_folds = [(1:43) (1:43)];
n_folds = n_folds(:); 
[~, stats_loso] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', n_folds, 'error_type', 'mcr');
ROC_loso = roc_plot(stats_loso.dist_from_hyperplane_xval, data.Y == 1, 'threshold', 0,'noplot');
cd('fmri\mvparesults\')
save('All_Prediction_results_PA_VPE.mat','ROC_loso','stats','stats_loso','data')
%%save unthresholded figures
obj=fmri_data(stats_loso.weight_obj)
write(obj,'fname','wholeprediction_unthresholded_PA_VPE.nii')
%not significant ACC, thus do not need do bootstrapping-sig test
% txt=['ACC=',num2str(ROC_loso.accuracy),' p=',num2str(ROC_loso.accuracy_p  )]
% text(0.4,0.4,txt)
ROC_loso = roc_plot(stats_loso.dist_from_hyperplane_xval, data.Y == 1,  'color', 'black');
set(gca, 'fontsize', 18, 'linewidth', 2, 'ticklength', [.01 .01], 'tickdir', 'out');
set(gcf, 'color', 'w', 'position', [200   258   388  358]);
box off;
export_fig separampucacc_svc_vpe -tiff -r400


%threshold visualization for APE AND VPE
clear,clc;
load All_Prediction_results_PA_APE
[~, stats_boot] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr', 'bootweights', 'bootsamples', 5000,'useparallel',4);
save('All_boot5000_results_PA_APE.mat','stats_boot','data')
filedir='fmri\mvparesults\'
data_threshold = threshold(stats_boot.weight_obj, .01, 'unc'); 
data_threshold.fullpath = fullfile(filedir, 'svm_bootstrap5000_results_UNC01_PA_APE.nii'); 
write(data_threshold, 'thresh');


%threshold visualization for APE AND VPE
clear,clc;
load All_Prediction_results_PA_VPE
[~, stats_boot] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr', 'bootweights', 'bootsamples', 5000,'useparallel',4);
save('All_boot5000_results_PA_VPE.mat','stats_boot','data')
filedir='fmri\mvparesults\'
data_threshold = threshold(stats_boot.weight_obj, .01, 'unc'); 
data_threshold.fullpath = fullfile(filedir, 'svm_bootstrap5000_results_UNC01_PA_VPE.nii'); 
write(data_threshold, 'thresh')



%% distinct pattern representation analyses with unthreshod map
%%true corr
cd('fmri\pattern_similarity')
clear all
basedir = 'fmri\RPE_1stdat\';
gray_matter_mask = 'fmri\myGM_mask.nii';
puc_imgs = spm_select('FPListRec', basedir, 'con_0002.nii');
acc_imgs = spm_select('FPListRec', basedir, 'con_0008.nii');
data = fmri_data([puc_imgs; acc_imgs], gray_matter_mask);
data.Y = [ones(43,1); -ones(43,1)]; %PUC: 1, ACC: -1
[~, stats] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr');
n_folds = [(1:43) (1:43)];
n_folds = n_folds(:); 
[~, stats_loso] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', n_folds, 'error_type', 'mcr');
datrpe=stats_loso.weight_obj.dat 

basedir2 = 'fmri\APE_1stdat\';
puc_imgs = spm_select('FPListRec', basedir2, 'con_0004.nii');
acc_imgs = spm_select('FPListRec', basedir2, 'con_0010.nii');
data = fmri_data([puc_imgs; acc_imgs], gray_matter_mask);
data.Y = [ones(43,1); -ones(43,1)]; 
[~, stats] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr');
n_folds = [(1:43) (1:43)];
n_folds = n_folds(:); 
[~, stats_loso] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', n_folds, 'error_type', 'mcr');
datape=stats_loso.weight_obj.dat 

basedir3 = 'fmri\VPE_1stdat\';
puc_imgs = spm_select('FPListRec', basedir3, 'con_0006.nii');
acc_imgs = spm_select('FPListRec', basedir3, 'con_0012.nii');
data = fmri_data([puc_imgs; acc_imgs], gray_matter_mask);
data.Y = [ones(43,1); -ones(43,1)]; 
[~, stats] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr');
n_folds = [(1:43) (1:43)];
n_folds = n_folds(:); 
[~, stats_loso] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', n_folds, 'error_type', 'mcr');
datvpe=stats_loso.weight_obj.dat 


[r_rv,p_rv]=corr(datrpe,datvpe,'type','Pearson')
[r_ar,p_ar]=corr(datape,datrpe,'type','Pearson')
[r_av,p_av]=corr(datape,datvpe,'type','Pearson')


%%bootstrapping corr
cd('fmri\pattern_similarity')
clear all
basedir = 'fmri\RPE_1stdat';
gray_matter_mask = 'fmri\myGM_mask.nii';
puc_imgs = spm_select('FPListRec', basedir, 'con_0002.nii');
acc_imgs = spm_select('FPListRec', basedir, 'con_0008.nii');
data = fmri_data([puc_imgs; acc_imgs], gray_matter_mask);
for ii=1:500
seq=[ones(43,1); -ones(43,1)]
newseq=seq(randperm(86))
data.Y = newseq; 
[~, stats] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr');
n_folds = [(1:43) (1:43)];
n_folds = n_folds(:); 
[~, stats_loso] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', n_folds, 'error_type', 'mcr');
datrpe(:,ii)=stats_loso.weight_obj.dat 
end
save svm_rpe_corr.mat datrpe

clear all
basedir = 'fmri\APE_1stdat';
gray_matter_mask = 'fmri\myGM_mask.nii';
puc_imgs = spm_select('FPListRec', basedir, 'con_0004.nii');
acc_imgs = spm_select('FPListRec', basedir, 'con_0010.nii');
data = fmri_data([puc_imgs; acc_imgs], gray_matter_mask);
for ii=1:500
seq=[ones(43,1); -ones(43,1)]
newseq=seq(randperm(86))
data.Y = newseq; %OUT: 1, PREDIC: -1
[~, stats] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr');
n_folds = [(1:43) (1:43)];
n_folds = n_folds(:); 
[~, stats_loso] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', n_folds, 'error_type', 'mcr');
datape(:,ii)=stats_loso.weight_obj.dat 
end
save svm_ape_corr.mat datape

clear all
basedir = 'fmri\VPE_1stdat';
gray_matter_mask = 'fmri\myGM_mask.nii';
puc_imgs = spm_select('FPListRec', basedir, 'con_0006.nii');
acc_imgs = spm_select('FPListRec', basedir, 'con_0012.nii');
data = fmri_data([puc_imgs; acc_imgs], gray_matter_mask);
for ii=1:500
seq=[ones(43,1); -ones(43,1)]
newseq=seq(randperm(86))
data.Y = newseq; %OUT: 1, PREDIC: -1
[~, stats] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', 1, 'error_type', 'mcr');
n_folds = [(1:43) (1:43)];
n_folds = n_folds(:); 
[~, stats_loso] = predict(data, 'algorithm_name', 'cv_svm', 'nfolds', n_folds, 'error_type', 'mcr');
datvpe(:,ii)=stats_loso.weight_obj.dat 
end
save svm_vpe_corr.mat datvpe

%%STATTSTIC COMPARISON
clear,clc;
cd('fmri\pattern_similarity')
load('svm_rpe_corr.mat')
load('svm_ape_corr.mat')
load('svm_vpe_corr.mat')

for ii=1:500
    [r(ii,:),p(ii,:)]= corr(datrpe(:,ii),datvpe(:,ii),'type','Pearson')
    [r1(ii,:),p1(ii,:)]= corr(datape(:,ii),datvpe(:,ii),'type','Pearson')
    [r2(ii,:),p2(ii,:)]= corr(datape(:,ii),datrpe(:,ii),'type','Pearson')
end

cir=[mean(r)-1.96*std(r),mean(r)+1.96*std(r)]
cir1=[mean(r1)-1.96*std(r1),mean(r1)+1.96*std(r1)]
cir2=[mean(r2)-1.96*std(r2),mean(r2)+1.96*std(r2)]

[h,p,ci,stats]=ttest(r2);
cols =  [0.39 0.58 0.93;0.94 0.50 0.50;1 0.87 0.68];
lcols = [1 1 1;1 1 1;1 1 1];

boxplot_wani_2016([r,r1,r2], 'color',lcols,'boxlinecolor', cols, 'linewidth', 1.5, 'boxlinewidth', 2, 'mediancolor', 'k', 'xtick', ({'R&VPE', 'A&VPE','A&RPE'}),'refline',0,'reflinewidth',2,'reflinestyle','--','reflinecolor',[0.55 0.54 0.54]);
set(gca, 'xtickLabel', ({'R&VPE', 'A&VPE','A&RPE'}),'fontsize', 15);
set(gca, 'fontsize', 15, 'ticklength', [.01 .01], 'ylim',[-0.2 0.2]);
set(gcf, 'position', [1000  468 418 338]);
line([0.95,1.05],[0.0843,0.0843],'color','red','LineWidth',2)% line([xmin,xmax],[ymin,ymax])
line([0.95,1.05],[-0.0839,-0.0839],'color','red','LineWidth',2)

line([1.95,2.05],[0.0830,0.0830],'color','red','LineWidth',2)% line([xmin,xmax],[ymin,ymax])
line([1.95,2.05],[-0.0805,-0.0805],'color','red','LineWidth',2)

line([2.95,3.05],[0.0759,0.0759],'color','red','LineWidth',2)% line([xmin,xmax],[ymin,ymax])
line([2.95,3.05],[-0.0793,-0.0793],'color','red','LineWidth',2)

export_fig svm_allpe_corr -tiff -r400

%%%%%%%%%%%univariate image
clear all
basedir = 'fmri\RPE_1stdat';
gray_matter_mask = 'fmri\myGM_mask.nii';
rpe_imgs = spm_select('FPListRec', basedir, 'con_0016.nii');
data = fmri_data(rpe_imgs, gray_matter_mask);
rpedata = data.dat;

basedir2 = 'fmri\VPE_1stdat';
vpe_imgs = spm_select('FPListRec', basedir2, 'con_0018.nii');
data2 = fmri_data(vpe_imgs, gray_matter_mask);
vpedata = data2.dat;

basedir3 = 'fmri\APE_1stdat';
ape_imgs = spm_select('FPListRec', basedir3, 'con_0017.nii');
data3 = fmri_data(ape_imgs, gray_matter_mask);
apedata = data3.dat;

for ii=1:43
    [rr(ii,:),pp(ii,:)]= corr(rpedata(:,ii),vpedata(:,ii),'type','Pearson')
end
for ii=1:43
    [rr1(ii,:),pp(ii,:)]= corr(apedata(:,ii),vpedata(:,ii),'type','Pearson')
end
for ii=1:43
    [rr2(ii,:),pp(ii,:)]= corr(apedata(:,ii),rpedata(:,ii),'type','Pearson')
end

[h,p]=ttest(rr2);
disp('One-Sample T-test:');
disp(['t = ',num2str(stats.tstat,'%0.2f')]);     
disp(['p = ',num2str(p,'%0.2e')]);
cols =  [0.39 0.58 0.93;0.94 0.50 0.50;1 0.87 0.68];
lcols = [1 1 1;1 1 1;1 1 1];
boxplot_wani_2016([rr,rr1,rr2], 'color',lcols,'boxlinecolor', cols, 'linewidth', 1.5, 'boxlinewidth', 2, 'mediancolor', 'k', 'xtick', ({'R&VPE', 'A&VPE','A&RPE'}),'refline',0,'reflinewidth',2,'reflinestyle','--','reflinecolor',[0.55 0.54 0.54]);
set(gca, 'xtickLabel', ({'R&VPE', 'A&VPE','A&RPE'}),'fontsize', 15);
set(gca, 'fontsize', 15, 'ticklength', [.01 .01]);
set(gcf, 'position', [1000  468 418 338]);
export_fig R_uni_allpe -tiff -r400

%%
%%


