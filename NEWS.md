# FLR4MFCL 1.6.7 (development)

* New functions: diffIniStepwise(), firstYear(), summary_ini().

# FLR4MFCL 1.5.0 (2023-05-25)

* read.MFCLLenFit() and read.MFCLLenFit2() are now the same function. In recent
  versions of FLR4MFCL, read.MFCLLenFit2() was working and read.MFCLLenFit() was
  obsolete and broken. As of version 1.5.0, users can pick whichever feels
  right, as they are the same.

# FLR4MFCL 1.4.7 (2023-05-18)

-

# FLR4MFCL 1.4.6 (2023-04-24)

* New functions: flagSort().

# FLR4MFCL 1.4.5 (2023-04-12)

* New methods: age_length.

* Improved read.MFCLLikelihood() so it reads in age_length likelihood.

* Improved summary(MFCLLikelihood) so it reports neglogL instead of abs(neglogL)
  and includes cpue and age components.

# FLR4MFCL 1.4.4 (2023-04-06)

* New methods: weight_fish.

# FLR4MFCL 1.4.3 (2023-03-27)

-

# FLR4MFCL 1.4.2 (2023-03-22)

-

# FLR4MFCL 1.4.1 (2023-02-02)

-

# FLR4MFCL 1.4.0 (2023-02-02)

-

# FLR4MFCL 1.3.28 (2022-11-20)

As at January 2023 - latest version in repository following adoption of skipjack MP (actual version used for evaluations will be an earlier version - see HTCondor backup folder).

* New functions: diffFlags, diffFlagsStepwise, finalPar, finalRep, flagMeaning, flagSummary, hcr_hillary_step_constrained.

# FLR4MFCL 1.3.27 (2022-10-06)

-

# FLR4MFCL 1.3.26 (2022-10-06)

* New functions: corFilter, corLabel, mat2MFCLCor, read.MFCLCor.

# FLR4MFCL 1.3.25 (2022-08-21)

-

# FLR4MFCL 1.3.24 (2022-08-10)

-

# FLR4MFCL 1.3.23 (2022-07-30)

* New functions: hcr_asymptotic_hillary_step.

# FLR4MFCL 1.3.22 (2022-07-28)

* New functions: read.MFCLPseudoCatch, read.MFCLPseudoEffort.

* New methods: mohns_rho.

# FLR4MFCL 1.3.21 (2022-05-28)

-

# FLR4MFCL 1.3.20 (2022-05-22)

* New methods: eq_rec, eq_rec<-, eq_rec_obs, eq_rec_obs<-, eq_ssb, eq_ssb<-, eq_ssb_obs, eq_ssb_obs<-.

# FLR4MFCL 1.3.19 (2022-05-11)

* New function: catchWfromcatchN.

# FLR4MFCL 1.3.18 (2022-05-04)

-

# FLR4MFCL 1.3.17 (2022-04-29)

-

# FLR4MFCL 1.3.16 (2022-04-29)

-

# FLR4MFCL 1.3.15 (2022-04-22)

-

# FLR4MFCL 1.3.13 (2022-03-10)

* New methods: survey_index, survey_index<-.

# FLR4MFCL 1.3.12 (2022-03-03)

-

# FLR4MFCL 1.3.11 (2022-03-03)

-

# FLR4MFCL 1.3.10 (2022-03-03)

* New functions: read.MFCLPseudoCatchEffort, read.MFCLPseudoSizeComp.

* New methods: tag_shed_rate, tag_shed_rate<-.

# FLR4MFCL 1.3.8 (2022-02-15)

-

# FLR4MFCL 1.3.7 (2022-02-05)

-

# FLR4MFCL 1.3.6 (2022-01-17)

-

# FLR4MFCL 1.3.5 (2022-01-11)

-

# FLR4MFCL 1.3.4 (2021-11-08)

* New functions: read.MFCLCatchSim, read.MFCLEffortSim.

# FLR4MFCL 1.3.3 (2021-09-17)

* New functions: read.MFCLLenFit2.

# FLR4MFCL 1.3.2 (2021-06-27)

* New methods: fm_level_regression_pars, fm_level_regression_pars<-, kludged_eq_coffs, kludged_eq_coffs<-, kludged_eq_level_coffs, kludged_eq_level_coffs<-, new_orth_coffs, new_orth_coffs<-, rec_orthogonal, rec_orthogonal<-, rec_standard, rec_standard<-, rec_standard_dim, rec_standard_dim<-, y1diff_coffs, y1diff_coffs<-, y2diff_coffs, y2diff_coffs<-, zdiff_coffs, zdiff_coffs<-.

# FLR4MFCL 1.2.8 (2021-06-16)

* New functions: read.SBSBF0.

* New methods: calc_diff_coffs_age_period.

# FLR4MFCL 1.2.7 (2021-03-29)

-

# FLR4MFCL 1.2.6 (2021-02-04)

* New functions: ats.

* New methods: recYears.

# FLR4MFCL 1.2.5 (2020-12-15)

-

# FLR4MFCL 1.2.4 (2020-10-29)

* New functions: hcr_asymptotic, hcr_asymptotic_constrained, hcr_constrained.

* New methods: mean_waa, mean_waa<-.

# FLR4MFCL 1.2.3 (2020-10-28)

* New functions: MFCLEquilibrium, read.MFCLEquilibrium.

* New classes: MFCLEquilibrium.

* New methods: Eq_calcs, Eq_calcs<-, fm_aggregated, fm_aggregated<-, YPR, YPR<-.

# FLR4MFCL 1.2.2 (2020-07-24)

* New functions: checkMissingRealisations, checkUnitDimnames, hcr_constant, hcr_hillary_step, hcr_threshold, hcr_threshold_constrained, MFCLLikelihood, read.MFCLLikelihood.

* New classes: MFCLLikelihood.

* New methods: bh_steep_contrib, bh_steep_contrib<-, catch_fish, catch_fish<-, effort_dev_penalty, effort_dev_penalty<-, fprojyr<-, length_fish, length_fish<-, q_dev_pen_fish, q_dev_pen_fish<-, q_dev_pen_fish_grp, q_dev_pen_fish_grp<-, summary, tag_rel_fish, tag_rel_fish<-, total_catch_fish, total_catch_fish<-, total_length_fish, total_length_fish<-, total_weight_fish, total_weight_fish<-.

# FLR4MFCL 1.2.1 (2020-05-05)

* New functions: MFCLFrq2, MFCLLenFreq2.

* New methods: cateffpen, cateffpen<-, lnfrq, lnfrq<-, SB, SBF0recent, SBlatest, SBrecent, SBSBF0latest, SBSBF0recent, wtfrq, wtfrq<-.

# FLR4MFCL 1.2.0 (2020-04-30)

* New functions: read.MFCLSimulatedNatAge.

* New classes: MFCLFrq2, MFCLLenFreq2.

* New methods: as.MFCLFrq, as.MFCLFrq2, ini_version, ini_version<-, modifyRRini, region_flags, region_flags<-.

# FLR4MFCL 1.1.6 (2020-01-29)

* New methods: as.MFCLIni.

# FLR4MFCL 1.1.5 (2019-09-26)

* New methods: ABBMSY, ABBMSY<-, ABBMSY_ts, ABBMSY_ts<-, FFMSY_ts, FFMSY_ts<-, TBBMSY, TBBMSY<-.

# FLR4MFCL 1.1.4 (2019-06-17)

* New methods: effort_creep, effort_creep<-, effort_creep_fish, effort_creep_fish<-, xdiff_coffs, xdiff_coffs<-.

# FLR4MFCL 1.1.3 (2019-05-14)

* New functions: condor_submit_set_iters.

# FLR4MFCL 1.1.2 (2019-04-26)

* New functions: contributions.plot, hcr_constant, hcr_threshold, hcr_threshold_constrained, read.MFCLProjectedSpawningBiomass.

* New methods: eval_hcr, hcr_params, hcr_params<-, SBF0Alt, SBSBF0Alt.

# FLR4MFCL 1.1.1 (2019-01-22)

* New functions: MFCLEMControl, MFCLMSEControl, MFCLPseudoControl, read.MFCLCatch, read.MFCLCatchN, read.MFCLPseudoAlt, write.simyears.

* New classes: MFCLEMControl, MFCLMSEControl, MFCLPseudoControl.

* New methods: as.MFCLLenFreq, catch_sd, catch_sd<-, doitall, doitall<-, effort_sd, effort_sd<-, hcr, hcr<-, iter, itn, itn<-, random_seeds, random_seeds<-, realisations, reduce, tag_controls, tag_controls<-.

# FLR4MFCL 1.1.0 (2018-10-05)

* New methods: mat_at_length, mat_at_length<-.

# FLR4MFCL 1.0.2 (2018-09-03)

* New methods: rep_rate_proj, rep_rate_proj<-, totalBiomass_nofish, totalBiomass_nofish<-.

# FLR4MFCL 1.0.1 (2018-07-14)

* New functions: availableMFCLversions, check.mfcl.cfg, contributions.plot, flagDiff, flagSummary, getMFCLversion, gradients, MFCLLenFit, MFCLPseudo, MFCLTagProj, qts, read.MFCLLenFit, read.MFCLProjectedNatAge, read.MFCLPseudo, read.temporary_tag_report, recPeriod, write.simNumAge.

* New classes: MFCLCatch, MFCLLenFit, MFCLPseudo, MFCLTagProj.

* New methods: aal, AggregateF, AggregateF<-, BBMSY, BBMSY<-, BMSY, BMSY<-, catch_obs, catch_obs<-, catch_pred, catch_pred<-, catcheff, catcheff<-, controls, controls<-, cpue_obs, cpue_obs<-, cpue_pred, cpue_pred<-, ess, ess<-, FFMSY, FFMSY<-, FMSY, FMSY<-, Fmult, Fmult<-, fprojyr, l_frq, l_frq<-, laa, lenagefits, lenagefits<-, lenfits, lenfits<-, MSY, MSY<-, orth_coffs, orth_coffs<-, plot, release_groups_proj, release_groups_proj<-, releases_proj, releases_proj<-, spp_params, spp_params<-, SPR0, subset, w_frq, w_frq<-, waa.

# FLR4MFCL 0.0.2 (2016-11-14)

* New functions: availableMFCLversions, check.mfcl.cfg, getMFCLversion, MFCLBase, MFCLBiol, MFCLCatch, MFCLFlags, MFCLFrq, MFCLFrqStats, MFCLIni, MFCLIniBits, MFCLLenFreq, MFCLPar, MFCLParBits, MFCLprojControl, MFCLRec, MFCLRegion, MFCLRep, MFCLSel, MFCLTag, MFCLTagRep, read.MFCLBiol, read.MFCLCatch, read.MFCLFlags, read.MFCLFrq, read.MFCLFrqStats, read.MFCLIni, read.MFCLLenFreq, read.MFCLPar, read.MFCLParBits, read.MFCLRec, read.MFCLRegion, read.MFCLRep, read.MFCLSel, read.MFCLTag, read.MFCLTagRep, setMFCLversion, setSimYrs, write.simNumAge.

* New classes: MFCLBase, MFCLBiol, MFCLCatch, MFCLFlags, MFCLFrq, MFCLFrqStats, MFCLIni, MFCLIniBits, MFCLLenFreq, MFCLPar, MFCLParBits, MFCLprojControl, MFCLRec, MFCLRegion, MFCLRep, MFCLSel, MFCLTag, MFCLTagRep.

* New methods: adultBiomass, adultBiomass<-, adultBiomass_nofish, adultBiomass_nofish<-, age_nage, age_nage<-, age_pars, age_pars<-, av_fish_mort_age, av_fish_mort_age<-, av_fish_mort_inst, av_fish_mort_inst<-, av_fish_mort_year, av_fish_mort_year<-, av_q_coffs, av_q_coffs<-, availability_coffs, availability_coffs<-, avyrs, avyrs<-, caeff, caeff<-, catch_dev_coffs, catch_dev_coffs<-, catch_dev_coffs_flag, catch_dev_coffs_flag<-, common_len_bias_coffs, common_len_bias_coffs<-, common_len_bias_pars, common_len_bias_pars<-, control_flags, control_flags<-, data_flags, data_flags<-, diff_coffs, diff_coffs<-, diff_coffs_age, diff_coffs_age<-, diff_coffs_age_period, diff_coffs_age_period<-, diff_coffs_age_priors, diff_coffs_age_priors<-, diff_coffs_age_ssn, diff_coffs_age_ssn<-, diff_coffs_mat, diff_coffs_mat<-, diff_coffs_nl, diff_coffs_nl<-, diff_coffs_nl_priors, diff_coffs_nl_priors<-, diff_coffs_priors, diff_coffs_priors<-, dimensions, dimensions<-, effort_dev_coffs, effort_dev_coffs<-, eq_biomass, eq_biomass<-, eq_yield, eq_yield<-, fish_params, fish_params<-, fishery_catch, fishery_catch<-, fishery_realizations, fishery_realizations<-, fishery_sel, fishery_sel<-, fishery_sel_age_comp, fishery_sel_age_comp<-, flags, flags<-, flagval, flagval<-, fm, fm<-, fm_level_devs, fm_level_devs<-, freq, freq<-, frq_age_len, frq_age_len<-, frq_version, frq_version<-, generate, generic_diffusion, generic_diffusion<-, growth, growth<-, growth_curve_devs, growth_curve_devs<-, growth_devs_age, growth_devs_age<-, growth_devs_cohort, growth_devs_cohort<-, growth_var_pars, growth_var_pars<-, ini_q_coffs, ini_q_coffs<-, lagrangian, lagrangian<-, len_bias_pars, len_bias_pars<-, lf_range, lf_range<-, log_m, log_m<-, logistic_normal_params, logistic_normal_params<-, lw_params, lw_params<-, m, m<-, m_at_age, m_at_age<-, m_devs_age, m_devs_age<-, mat, mat<-, max_grad, max_grad<-, mean_laa, mean_laa<-, mfcl, mn_len_pen, mn_len_pen<-, move_map, move_map<-, move_matrix, move_matrix<-, move_weeks, move_weeks<-, n_fisheries, n_fisheries<-, n_mean_constraints, n_mean_constraints<-, n_move_yr, n_move_yr<-, n_pars, n_pars<-, n_recs_yr, n_recs_yr<-, n_regions, n_regions<-, n_tag_groups, n_tag_groups<-, nsims, nsims<-, nyears, nyears<-, obj_fun, obj_fun<-, popN, popN<-, q0_miss, q0_miss<-, q_dev_coffs, q_dev_coffs<-, q_effdev, q_effdev<-, q_fishery, q_fishery<-, range, range<-, rec, rec<-, rec_dist, rec_dist<-, rec_init_pop_diff, rec_init_pop_diff<-, rec_month, rec_month<-, rec_obs, rec_obs<-, rec_region, rec_region<-, rec_times, rec_times<-, recaptures, recaptures<-, recoveries, recoveries<-, region_fish, region_fish<-, region_pars, region_pars<-, region_rec_var, region_rec_var<-, region_size, region_size<-, rel_ini_pop, rel_ini_pop<-, rel_rec, rel_rec<-, release_groups, release_groups<-, release_lengths, release_lengths<-, releases, releases<-, rep_rate_dev_coffs, rep_rate_dev_coffs<-, richards, richards<-, SBF0, SBSBF0, scaler, scaler<-, sd_laa, sd_laa<-, sd_length_at_age, sd_length_at_age<-, sd_length_dep, sd_length_dep<-, season_flags, season_flags<-, season_growth_pars, season_growth_pars<-, season_q_pars, season_q_pars<-, sel, sel<-, sel_dev_coffs, sel_dev_coffs2, sel_dev_coffs2<-, sel_dev_coffs<-, sel_dev_corr, sel_dev_corr<-, srr, srr<-, ssb, ssb<-, ssb_obs, ssb_obs<-, steepness, steepness<-, sv, sv<-, tag_fish_rep_flags, tag_fish_rep_flags<-, tag_fish_rep_grp, tag_fish_rep_grp<-, tag_fish_rep_pen, tag_fish_rep_pen<-, tag_fish_rep_rate, tag_fish_rep_rate<-, tag_fish_rep_target, tag_fish_rep_target<-, tag_lik, tag_lik<-, tot_pop, tot_pop<-, tot_pop_implicit, tot_pop_implicit<-, total_catch, total_catch<-, totalBiomass, totalBiomass<-, unused, unused<-, update, version, vulnBiomass, vulnBiomass<-, window, write.

# FLR4MFCL 0.0.1 (2015-03-17)

* New functions: read.MFCLVitalStats.

---

General note: Unless specific news entries were written, the above entries are
based on changes in the `NAMESPACE` file between versions, specifically exported
functions, classes, and methods. A simple '-' indicates that no new exports were
introduced in that version.
