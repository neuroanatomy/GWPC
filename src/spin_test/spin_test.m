[~, output] = system('git rev-parse --show-toplevel');
project_dir1 = strtrim(output);
fs_dir1 = [project_dir1 '/data/derived/fs-6.0.0'];
project_dir2 = strtrim(output);
fs_dir2 = [project_dir2 '/data/derived/eu-aims/timepoint_1/fs-6.0.0'];
fshome = getenv('FREESURFER_HOME');

% origleft1 = [project_dir1 '/data/derived/spin_test/output/lh.ASDr10197716.mgh'];
% origright1 = [project_dir1 '/data/derived/spin_test/output/rh.ASDr10197716.mgh'];
% origleft1 = [fs_dir1 '/glm_mod2/lh.w-g.pct.30.fwhm10.mod/contrast_ASD/gamma.mgh'];
% origright1 = [fs_dir1 '/glm_mod2/rh.w-g.pct.30.fwhm10.mod/contrast_ASD/gamma.mgh'];
origleft2 = [fs_dir2 '/glm_mod2/lh.w-g.pct.30.fwhm10.mod/contrast_ASD/gamma.mgh'];
origright2 = [fs_dir2 '/glm_mod2/rh.w-g.pct.30.fwhm10.mod/contrast_ASD/gamma.mgh'];
permno = 100;
wsname = [project_dir1 '/data/derived/spin_test/rotationFS.mat'];

readleft1 = [origleft1(1:end-4) '.fsaverage5.csv'];
if ~exist(readleft1, 'file')
    resa = [readleft1(1:end-4) '.mgh'];
    command = ['mri_surf2surf --hemi lh --srcsubject fsaverage --srcsurfval ' origleft1 ' --src_type curv ' ...
                '--trgsubject fsaverage5 --trgsurfval ' resa ' --trg_type mgh'];
    system(command);
    data = load_mgh(resa);
    dlmwrite(readleft1, data);
end

readright1 = [origright1(1:end-4) '.fsaverage5.csv'];
if ~exist(readright1, 'file')
    resa = [readright1(1:end-4) '.mgh'];
    command = ['mri_surf2surf --hemi rh --srcsubject fsaverage --srcsurfval ' origright1 ' --src_type curv ' ...
                '--trgsubject fsaverage5 --trgsurfval ' resa ' --trg_type mgh'];
    system(command);
    data = load_mgh(resa);
    dlmwrite(readright1, data);
end

readleft2 = [origleft2(1:end-4) '.fsaverage5.csv'];
if ~exist(readleft2, 'file')
    resa = [readleft2(1:end-4) '.mgh'];
    command = ['mri_surf2surf --hemi lh --srcsubject fsaverage --srcsurfval ' origleft2 ' --src_type curv ' ...
                '--trgsubject fsaverage5 --trgsurfval ' resa ' --trg_type mgh'];
    system(command);
    data = load_mgh(resa);
    dlmwrite(readleft2, data);
end

readright2 = [origright2(1:end-4) '.fsaverage5.csv'];
if ~exist(readright2, 'file')
    resa = [readright2(1:end-4) '.mgh'];
    command = ['mri_surf2surf --hemi rh --srcsubject fsaverage --srcsurfval ' origright2 ' --src_type curv ' ...
                '--trgsubject fsaverage5 --trgsurfval ' resa ' --trg_type mgh'];
    system(command);
    data = load_mgh(resa);
    dlmwrite(readright2, data);
end
    
SpinPermuFS(readleft1,readright1,permno,wsname)

% indicate (with 0's and 1's) which vertices in the left and right
% hemispheres are part of the medial wall
[vl, left_labels, ctl] = read_annotation(fullfile(fshome,'/subjects/fsaverage5/label/lh.aparc.a2009s.annot'));
v_exclude_left = left_labels==1644825; % label of vertices in the medial wall is 1644825
[vr,right_labels,ctr] = read_annotation(fullfile(fshome,'/subjects/fsaverage5/label/rh.aparc.a2009s.annot'));
v_exclude_right = right_labels==1644825;

[pval, realrho, meanrho, stdrho, pval_interp] = pvalvsNull(readleft1,readright1,readleft2,readright2,permno,wsname,v_exclude_left,v_exclude_right)
[filepath,name,ext] = fileparts(origleft1);
result_file = [filepath '/' name(4:end) '.txt'];
header = {'pval', 'permno', 'realrho', 'meanrho', 'stdrho', 'pval_interp'};
values = cellfun(@eval, header, 'UniformOutput', false);
delimiter = '\t';
rfile = fopen(result_file, 'w');
fprintf(rfile, '%s\n', strjoin(header, delimiter));
fprintf(rfile, '%s\n', strjoin(cellfun(@num2str, values, 'UniformOutput', false), delimiter));
fclose(rfile);
