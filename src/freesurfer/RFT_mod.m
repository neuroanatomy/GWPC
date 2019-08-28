function RFT_mod(smdata_mgh, glmdir, avgdir, label, clust_th, FWE, side, spaces)
%
%Performs Random Field Theory-based multiple comparison correction for 
%Freesurfer data. You need to have previously generated a Freesurfer's 
%glmdir. //Jorge Luis Bernal-Rusiel 2010.
%// mod by Nicolas Traut 2018
%
%Arguments:
%smdata_mgh: Complete path and name of the .mgh file containing the 
%smoothed data.
%glmdir: Directory of the freesurfer glmdir.
%avgdir: Directory of the freesurfer generated average subject for the
%study.
%label: Name of the label where the analysis is to be done. It must be in
%the average subject's label directory.
%clust_th: Cluster-forming p-value threshold.
%FWE: Family wise error.
%side: It is 0 for left-sided tests, 1 for right-sided tests or 2 for 
%two-sided tests.
%spaces: Number of spaces to make an additional Bonferroni correction
%
%
%Output:
%Save cluster and peak-based corrected significations as .mgh files to 
%contrast dirs.

%% Type conversion
if ischar(clust_th)
    clust_th = str2double(clust_th);
end
if ischar(FWE)
    FWE = str2double(FWE);
end
if ischar(side)
    side = str2double(side);
end
if ischar(spaces)
    spaces = str2double(spaces);
end

%% Readings
%read smoothed data
[smdata,Mvox2ras,mr_parms] = load_mgh(smdata_mgh);
smdata = squeeze(smdata)';
%read design matrix
fs_X = load([glmdir '/X.mat']);
%read label
[nv,vlabel] = ReadLabelData([avgdir '/label/' label]);
%read annotation
annot = [label(1:end-6) '.annot'];
[~, L, ct] = read_annotation([avgdir '/label/' annot]);
%hemisphere
if label(1) == 'l'
    hemi = 'lh';
else
    hemi = 'rh';
end
%read average surf
avgsurf = SurfStatReadSurf([avgdir '/surf/', hemi, '.white'],'b');
bonf = spaces;

%% Statistical analysis

disp(['Cluster and peak-based RFT correction with cluster-forming' ...
    ' threshold: ' num2str(clust_th) ' and FWE: ' num2str(FWE), ' ...']);
   
%Cluster-based and peak-based RFT correction
ndata = size(smdata);
mask = zeros(1, ndata(2));
mask(vlabel+1) = ones(1, nv);
mask = logical(mask);
slm_mod = SurfStatLinMod(smdata, fs_X.X, avgsurf);
%slm_mod.k = 1;

%loop in contrast dirs
[folders] = dir(glmdir);
for folder = {folders.name}
    %read contrast
    contrastdir = [glmdir '/' folder{1}];
    if folder{1}(1) == '.' || exist([contrastdir '/sig.mgh'], 'file') ~= 2
        continue
    end
    contrastmat = dlmread([contrastdir '/C.dat'])';
    contrastvect = sum(contrastmat, 2);
    X0 = fs_X.X(:, ~contrastvect);
    slm_mod0 = SurfStatLinMod(smdata, X0);
    slm_f = SurfStatF(slm_mod, slm_mod0);
    [pval_f,peak_f,clus_f,clusid_f] = SurfStatP2(slm_f, mask, clust_th);
    if size(contrastmat, 2) == 1
        contrast = fs_X.X*contrastmat;
        slm_t = SurfStatT(slm_mod, contrast);
        if side == 0
            slm_t0 = slm_t;
            slm_t0.t = -slm_t0.t;
            [pval,peak,clus,clusid] = SurfStatP2(slm_t0, mask, clust_th);
        elseif side == 1
            [pval,peak,clus,clusid] = SurfStatP2(slm_t, mask, clust_th);
        elseif side == 2
            slm_t0 = slm_t;
            slm_t0.t = -slm_t0.t;
            [side1.pval,side1.peak,side1.clus,side1.clusid] = SurfStatP2(slm_t0, mask, clust_th/2);
            [side2.pval,side2.peak,side2.clus,side2.clusid] = SurfStatP2(slm_t, mask, clust_th/2);
            [pval,peak,clus,clusid] = combine_sides(side1, side2);
        else
            disp(['Side = ' side]);
            error(['Side must be 0 for left-sided test, 1 for right-sided tests' ...
                ' or 2 for two-sided tests']);
        end
    end
    % significant clusters for t-values
    if size(contrastmat, 2) == 1
        sg = sign(slm_t.t(vlabel+1));
        pval.thresh = FWE;
        corr_cw_sig = zeros(1,ndata(2));
        corr_pw_sig = zeros(1,ndata(2));
        pval.C = min(1, pval.C * bonf);
        pval.P = min(1, pval.P * bonf);
        clus.P = min(1, clus.P * bonf);
        corr_cw_sig(vlabel+1) = min(-log10(pval.C(vlabel+1)),100).*sg;
        corr_cw_sig(abs(corr_cw_sig) < -log10(FWE)) = 0;
        corr_pw_sig(vlabel+1) = -log10(pval.P(vlabel+1)).*sg;
        corr_pw_sig(abs(corr_pw_sig) < -log10(FWE)) = 0;
        disp(['Saving cluster wise corrected significations to ' contrastdir ...
          '/cw_sig.mgh and peak-based corrected significations to ' contrastdir ...
          '/pw_sig.mgh']);
        save_mgh(corr_cw_sig,[contrastdir '/cw_sig.mgh'],Mvox2ras,mr_parms);
        save_mgh(corr_pw_sig,[contrastdir '/pw_sig.mgh'],Mvox2ras,mr_parms);
        disp(['Saving t-values to ' contrastdir ...
          '/t.mgh and effects (beta coefficient for contrast) to ' contrastdir ...
          '/ef.mgh']);
        save_mgh(slm_t.t,[contrastdir '/t.mgh'],Mvox2ras,mr_parms);
        save_mgh(slm_t.ef,[contrastdir '/ef.mgh'],Mvox2ras,mr_parms);
        disp(['Saving t-values for significant clusters to ' contrastdir ...
          '/cw_t.mgh and effects for significant clusters to ' contrastdir ...
          '/cw_ef.mgh']);
        corr_cw_t = zeros(1,ndata(2));
        corr_cw_ef = zeros(1,ndata(2));
        corr_cw_t(corr_cw_sig ~= 0) = slm_t.t(corr_cw_sig ~= 0);
        corr_cw_ef(corr_cw_sig ~= 0) = slm_t.ef(corr_cw_sig ~= 0);
        save_mgh(corr_cw_t,[contrastdir '/cw_t.mgh'],Mvox2ras,mr_parms);
        save_mgh(corr_cw_ef,[contrastdir '/cw_ef.mgh'],Mvox2ras,mr_parms);
        disp(['Saving standardized effects to ' contrastdir ...
          '/sef.mgh and standardized effects for significant clusters to ' contrastdir ...
          '/cw_sef.mgh']);
        rsd = sqrt(slm_t.SSE / slm_t.df);
        slm_t.sef = zeros(1,ndata(2));
        corr_cw_sef = zeros(1,ndata(2));
        slm_t.sef(rsd>0) = slm_t.ef(rsd>0) ./ rsd(rsd>0);
        corr_cw_sef(corr_cw_sig ~= 0) = slm_t.sef(corr_cw_sig ~= 0);
        save_mgh(slm_t.sef,[contrastdir '/sef.mgh'],Mvox2ras,mr_parms);
        save_mgh(corr_cw_sef,[contrastdir '/cw_sef.mgh'],Mvox2ras,mr_parms);
        header = {'cluster', 'region labels', 'hemisphere', 'vertices', ...
                  'resels', 'p_cluster', 'tmax', 'x', 'y', 'z', ...
                  'ef_mean', 'ef_sd', 'ef_min', 'ef_max', 'sef_mean'};
        
        disp(['Saving cluster summary to ' contrastdir '/rft.summary.tsv']);
        delimiter = '\t';
        datei = fopen([contrastdir '/rft.summary.tsv'], 'w');
        fprintf(datei, strjoin(header, delimiter));
        fprintf(datei, '\n');
        nc = 0;
        for c = clus.clusid'
            if clus.P(c) >= FWE
                break
            end
            nreg = sum(L(clusid==c) == ct.table(:,5)', 1);
            [~, rank] = sort(nreg, 'descend');
            rank = rank(1:sum(nreg>0));
            regions = strjoin(ct.struct_names(rank), ', ');
            nverts = clus.nverts(c);
            resels = clus.resels(c);
            pclus = clus.P(c);
            ipeak = find(peak.clusid == c, 1);
            tmax = peak.t(ipeak);
            ivert = peak.vertid(ipeak);
            tx = avgsurf.coord(1, ivert);
            ty = avgsurf.coord(2, ivert);
            tz = avgsurf.coord(3, ivert);
            efs = slm_t.ef(clusid==c);
            mean_ef = mean(efs);
            sd_ef = std(efs);
            min_ef = min(efs);
            max_ef = max(efs);
            sefs = slm_t.sef(clusid==c);
            mean_sef = mean(sefs);
            fprintf(datei, '%d', c);
            fprintf(datei, delimiter);
            fprintf(datei, regions);
            fprintf(datei, delimiter);
            fprintf(datei, hemi);
            fprintf(datei, delimiter);
            fprintf(datei, '%d', nverts);
            fprintf(datei, delimiter);
            fprintf(datei, '%.03f', resels);
            fprintf(datei, delimiter);
            fprintf(datei, '%.02e', pclus);
            fprintf(datei, delimiter);
            fprintf(datei, '%.02f', tmax);
            fprintf(datei, delimiter);
            fprintf(datei, '%d', tx);
            fprintf(datei, delimiter);
            fprintf(datei, '%d', ty);
            fprintf(datei, delimiter);
            fprintf(datei, '%d', tz);
            fprintf(datei, delimiter);
            fprintf(datei, '%.03f', mean_ef);
            fprintf(datei, delimiter);
            fprintf(datei, '%.03f', sd_ef);
            fprintf(datei, delimiter);
            fprintf(datei, '%.03f', min_ef);
            fprintf(datei, delimiter);
            fprintf(datei, '%.03f', max_ef);
            fprintf(datei, delimiter);
            fprintf(datei, '%.03f', mean_sef);
            fprintf(datei, '\n');
            nc = c;
        end
        fclose(datei);
        disp(['Saving cluster image to ' contrastdir '/rft.cluster.mgh']);
        save_mgh(clusid,[contrastdir '/rft.cluster.mgh'],Mvox2ras,mr_parms);
        disp(['Saving mean value per cluster and individual to ' contrastdir '/rft.cluster_mean.tsv']);
        cluster_mean = zeros(ndata(1), nc);
        for c = 1:nc
            cluster_mean(:,c) = mean(smdata(:, clusid == c), 2);
        end
        dlmwrite([contrastdir '/rft.cluster_mean.tsv'], cluster_mean, '\t');
    end
    
    % significant cluster for f-values
    corr_cw_sig_f = zeros(1,ndata(2));
    corr_pw_sig_f = zeros(1,ndata(2));
    pval_f.C = min(1, pval_f.C * bonf);
    pval_f.P = min(1, pval_f.P * bonf);
    clus_f.P = min(1, clus_f.P * bonf);
    corr_cw_sig_f(vlabel+1) = min(-log10(pval.C(vlabel+1)),100);
    corr_cw_sig_f(abs(corr_cw_sig_f) < -log10(FWE)) = 0;
    corr_pw_sig_f(vlabel+1) = -log10(pval_f.P(vlabel+1));
    corr_pw_sig_f(abs(corr_pw_sig_f) < -log10(FWE)) = 0;
    disp(['Saving cluster wise corrected significations to ' contrastdir ...
      '/cw_sig_F.mgh and peak-based corrected significations to ' contrastdir ...
      '/pw_sig_F.mgh']);
    save_mgh(corr_cw_sig_f,[contrastdir '/cw_sig_F.mgh'],Mvox2ras,mr_parms);
    save_mgh(corr_pw_sig_f,[contrastdir '/pw_sig_F.mgh'],Mvox2ras,mr_parms);
    MSE = slm_f.SSE / slm_f.df(2);
    % slm_f.t is in fact an F-value
    SSB = slm_f.t .* MSE * slm_f.df(1);
    SST = sum((smdata - mean(smdata)).^2);
    slm_f.epsq = zeros(1,ndata(2));
    slm_f.epsq(MSE>0) = (SSB(MSE>0) - slm_f.df(1) * MSE(MSE>0)) ./ SST(MSE>0);
    disp(['Saving F-values to ' contrastdir ...
      '/surfstat_F.mgh and effects (epsilon squared for contrast) to ' contrastdir ...
      '/epsq.mgh']);
    save_mgh(slm_f.t,[contrastdir '/surfstat_F.mgh'],Mvox2ras,mr_parms);
    save_mgh(slm_f.epsq,[contrastdir '/epsq.mgh'],Mvox2ras,mr_parms);
    disp(['Saving F-values for significant clusters to ' contrastdir ...
      '/cw_F.mgh and effects for significant clusters to ' contrastdir ...
      '/cw_epsq.mgh']);
    corr_cw_f = zeros(1,ndata(2));
    corr_cw_epsq = zeros(1,ndata(2));
    corr_cw_f(corr_cw_sig_f ~= 0) = slm_f.t(corr_cw_sig_f ~= 0);
    corr_cw_epsq(corr_cw_sig_f ~= 0) = slm_f.epsq(corr_cw_sig_f ~= 0);
    save_mgh(corr_cw_f,[contrastdir '/cw_F.mgh'],Mvox2ras,mr_parms);
    save_mgh(corr_cw_epsq,[contrastdir '/cw_epsq.mgh'],Mvox2ras,mr_parms);
    disp(['Saving partial epsilon squared to ' contrastdir ...
      '/pepsq.mgh and partial epsilon squared for significant clusters to ' contrastdir ...
      '/cw_pepsq.mgh']);
    slm_f.pepsq = zeros(1,ndata(2));
    corr_cw_pepsq = zeros(1,ndata(2));
    slm_f.pepsq(MSE>0) = (SSB(MSE>0) - slm_f.df(1) * MSE(MSE>0)) ./ (SSB(MSE>0) + slm_f.SSE(MSE>0));
    corr_cw_pepsq(corr_cw_sig_f ~= 0) = slm_f.pepsq(corr_cw_sig_f ~= 0);
    save_mgh(slm_f.pepsq,[contrastdir '/pepsq.mgh'],Mvox2ras,mr_parms);
    save_mgh(corr_cw_pepsq,[contrastdir '/cw_pepsq.mgh'],Mvox2ras,mr_parms);
    header = {'cluster', 'region labels', 'hemisphere', 'vertices', ...
              'resels', 'p_cluster', 'Fmax', 'x', 'y', 'z', ...
              'epsq_mean', 'epsq_sd', 'epsq_min', 'epsq_max', 'pepsq_mean'};
    
    disp(['Saving cluster summary to ' contrastdir '/rft_f.summary.tsv']);
    delimiter = '\t';
    datei = fopen([contrastdir '/rft_f.summary.tsv'], 'w');
    fprintf(datei, strjoin(header, delimiter));
    fprintf(datei, '\n');
    nc = 0;
    for c = clus_f.clusid'
        if clus_f.P(c) >= FWE
            break
        end
        nreg = sum(L(clusid_f==c) == ct.table(:,5)', 1);
        [~, rank] = sort(nreg, 'descend');
        rank = rank(1:sum(nreg>0));
        regions = strjoin(ct.struct_names(rank), ', ');
        nverts = clus_f.nverts(c);
        resels = clus_f.resels(c);
        pclus = clus_f.P(c);
        ipeak = find(peak_f.clusid == c, 1);
        fmax = peak_f.t(ipeak);
        ivert = peak_f.vertid(ipeak);
        fx = avgsurf.coord(1, ivert);
        fy = avgsurf.coord(2, ivert);
        fz = avgsurf.coord(3, ivert);
        epsqs = slm_f.epsq(clusid==c);
        mean_epsq = mean(epsqs);
        sd_epsq = std(epsqs);
        min_epsq = min(epsqs);
        max_epsq = max(epsqs);
        pepsqs = slm_f.pepsq(clusid_f==c);
        mean_pepsq = mean(pepsqs);
        fprintf(datei, '%d', c);
        fprintf(datei, delimiter);
        fprintf(datei, regions);
        fprintf(datei, delimiter);
        fprintf(datei, hemi);
        fprintf(datei, delimiter);
        fprintf(datei, '%d', nverts);
        fprintf(datei, delimiter);
        fprintf(datei, '%.03f', resels);
        fprintf(datei, delimiter);
        fprintf(datei, '%.02e', pclus);
        fprintf(datei, delimiter);
        fprintf(datei, '%.02f', fmax);
        fprintf(datei, delimiter);
        fprintf(datei, '%d', fx);
        fprintf(datei, delimiter);
        fprintf(datei, '%d', fy);
        fprintf(datei, delimiter);
        fprintf(datei, '%d', fz);
        fprintf(datei, delimiter);
        fprintf(datei, '%.03f', mean_epsq);
        fprintf(datei, delimiter);
        fprintf(datei, '%.03f', sd_epsq);
        fprintf(datei, delimiter);
        fprintf(datei, '%.03f', min_epsq);
        fprintf(datei, delimiter);
        fprintf(datei, '%.03f', max_epsq);
        fprintf(datei, delimiter);
        fprintf(datei, '%.03f', mean_pepsq);
        fprintf(datei, '\n');
        nc = c;
    end
    fclose(datei);
    disp(['Saving cluster image to ' contrastdir '/rft_f.cluster.mgh']);
    save_mgh(clusid_f,[contrastdir '/rft_f.cluster.mgh'],Mvox2ras,mr_parms);
    disp(['Saving mean value per cluster and individual to ' contrastdir '/rft_f.cluster_mean.tsv']);
    cluster_mean = zeros(ndata(1), nc);
    for c = 1:nc
        cluster_mean(:,c) = mean(smdata(:, clusid_f == c), 2);
    end
    dlmwrite([contrastdir '/rft_f.cluster_mean.tsv'], cluster_mean, '\t');
end
end

function [pval, peak, clus, clusid] = combine_sides(side1, side2)
    pval.C = min(side1.pval.C*2, side2.pval.C*2);
    pval.C = min(pval.C, 1);
    pval.P = min(side1.pval.P*2, side2.pval.P*2);
    pval.P = min(pval.P, 1);
    pval.mask = side1.pval.mask | side2.pval.mask;
    clus.resels = [side1.clus.resels; side2.clus.resels];
    [clus.resels, I] = sort(clus.resels, 'descend');
    [~, Ir] = sort(I);
    n1 = size(side1.clus.clusid, 1);
    clus.nverts = [side1.clus.nverts; side2.clus.nverts];
    clus.nverts = clus.nverts(I,:);
    clus.P = [side1.clus.P*2; side2.clus.P*2];
    clus.P = clus.P(I,:);
    clus.P = min(clus.P, 1);
    clus.clusid = (1:size(I, 1))';
    clusid = side1.clusid + side2.clusid;
    clusid(side2.clusid>0) = clusid(side2.clusid>0) + n1;
    clusid(clusid>0) = Ir(clusid(clusid>0));
    peak.P = [side1.peak.P*2; side2.peak.P*2];
    [peak.P, I2] = sort(peak.P, 'ascend');
    peak.t = [-side1.peak.t; side2.peak.t];
    peak.t = peak.t(I2,:);
    peak.clusid = [side1.peak.clusid; side2.peak.clusid + n1];
    peak.clusid = Ir(peak.clusid);
    peak.clusid = peak.clusid(I2,:);
    peak.vertid = [side1.peak.vertid; side2.peak.vertid];
    peak.vertid = peak.vertid(I2,:);
end

% same as SurfStatP but create empty fields if they don't exist
function [pval, peak, clus, clusid] = SurfStatP2(slm_t, mask, clust_th)
    [pval, peak, clus, clusid] = SurfStatP(slm_t, mask, clust_th);
    if ~isfield(pval, 'C')
        pval.C = ones(size(pval.mask));
    end
    if ~isfield(peak, 't')
        peak.t = [];
    end
    if ~isfield(peak, 'vertid')
        peak.vertid = [];
    end
    if ~isfield(peak, 'clusid')
        peak.clusid = [];
    end
    if ~isfield(peak, 'P')
        peak.P = [];
    end
    if ~isfield(clus, 'clusid')
        clus.clusid = [];
    end
    if ~isfield(clus, 'nverts')
        clus.nverts = [];
    end
    if ~isfield(clus, 'resels')
        clus.resels = [];
    end
    if ~isfield(clus, 'P')
        clus.P = [];
    end
    if isempty(clusid)
        clusid = zeros(size(pval.mask));
    end
    
end
