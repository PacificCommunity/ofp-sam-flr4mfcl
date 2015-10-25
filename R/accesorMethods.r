# Automatically generated Generic Methods 
# Generated from 'createGenericMethods' 
# Generated on  Mon Oct 26 10:32:23 2015 
 
# class  MFCLFrqStats 
#'n_regions
#'@export n_regions
setGeneric('n_regions', function(object, ...) standardGeneric('n_regions')) 
setMethod('n_regions', signature('MFCLFrqStats'),function(object) return(slot(object, 'n_regions'))) 
#'n_regions
#'@export 
setGeneric('n_regions<-', function(object, ..., value) standardGeneric('n_regions<-')) 
setReplaceMethod('n_regions', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['n_regions'])),
                                function(object, value){slot(object, 'n_regions') <- value; return(object)}) 
#'n_fisheries
#'@export n_fisheries
setGeneric('n_fisheries', function(object, ...) standardGeneric('n_fisheries')) 
setMethod('n_fisheries', signature('MFCLFrqStats'),function(object) return(slot(object, 'n_fisheries'))) 
#'n_fisheries
#'@export 
setGeneric('n_fisheries<-', function(object, ..., value) standardGeneric('n_fisheries<-')) 
setReplaceMethod('n_fisheries', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['n_fisheries'])),
                                function(object, value){slot(object, 'n_fisheries') <- value; return(object)}) 
#'n_tag_groups
#'@export n_tag_groups
setGeneric('n_tag_groups', function(object, ...) standardGeneric('n_tag_groups')) 
setMethod('n_tag_groups', signature('MFCLFrqStats'),function(object) return(slot(object, 'n_tag_groups'))) 
#'n_tag_groups
#'@export 
setGeneric('n_tag_groups<-', function(object, ..., value) standardGeneric('n_tag_groups<-')) 
setReplaceMethod('n_tag_groups', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['n_tag_groups'])),
                                function(object, value){slot(object, 'n_tag_groups') <- value; return(object)}) 
#'n_recs_yr
#'@export n_recs_yr
setGeneric('n_recs_yr', function(object, ...) standardGeneric('n_recs_yr')) 
setMethod('n_recs_yr', signature('MFCLFrqStats'),function(object) return(slot(object, 'n_recs_yr'))) 
#'n_recs_yr
#'@export 
setGeneric('n_recs_yr<-', function(object, ..., value) standardGeneric('n_recs_yr<-')) 
setReplaceMethod('n_recs_yr', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['n_recs_yr'])),
                                function(object, value){slot(object, 'n_recs_yr') <- value; return(object)}) 
#'rec_month
#'@export rec_month
setGeneric('rec_month', function(object, ...) standardGeneric('rec_month')) 
setMethod('rec_month', signature('MFCLFrqStats'),function(object) return(slot(object, 'rec_month'))) 
#'rec_month
#'@export 
setGeneric('rec_month<-', function(object, ..., value) standardGeneric('rec_month<-')) 
setReplaceMethod('rec_month', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['rec_month'])),
                                function(object, value){slot(object, 'rec_month') <- value; return(object)}) 
#'generic_diffusion
#'@export generic_diffusion
setGeneric('generic_diffusion', function(object, ...) standardGeneric('generic_diffusion')) 
setMethod('generic_diffusion', signature('MFCLFrqStats'),function(object) return(slot(object, 'generic_diffusion'))) 
#'generic_diffusion
#'@export 
setGeneric('generic_diffusion<-', function(object, ..., value) standardGeneric('generic_diffusion<-')) 
setReplaceMethod('generic_diffusion', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['generic_diffusion'])),
                                function(object, value){slot(object, 'generic_diffusion') <- value; return(object)}) 
#'frq_age_len
#'@export frq_age_len
setGeneric('frq_age_len', function(object, ...) standardGeneric('frq_age_len')) 
setMethod('frq_age_len', signature('MFCLFrqStats'),function(object) return(slot(object, 'frq_age_len'))) 
#'frq_age_len
#'@export 
setGeneric('frq_age_len<-', function(object, ..., value) standardGeneric('frq_age_len<-')) 
setReplaceMethod('frq_age_len', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['frq_age_len'])),
                                function(object, value){slot(object, 'frq_age_len') <- value; return(object)}) 
#'frq_version
#'@export frq_version
setGeneric('frq_version', function(object, ...) standardGeneric('frq_version')) 
setMethod('frq_version', signature('MFCLFrqStats'),function(object) return(slot(object, 'frq_version'))) 
#'frq_version
#'@export 
setGeneric('frq_version<-', function(object, ..., value) standardGeneric('frq_version<-')) 
setReplaceMethod('frq_version', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['frq_version'])),
                                function(object, value){slot(object, 'frq_version') <- value; return(object)}) 
#'region_size
#'@export region_size
setGeneric('region_size', function(object, ...) standardGeneric('region_size')) 
setMethod('region_size', signature('MFCLFrqStats'),function(object) return(slot(object, 'region_size'))) 
#'region_size
#'@export 
setGeneric('region_size<-', function(object, ..., value) standardGeneric('region_size<-')) 
setReplaceMethod('region_size', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['region_size'])),
                                function(object, value){slot(object, 'region_size') <- value; return(object)}) 
#'region_fish
#'@export region_fish
setGeneric('region_fish', function(object, ...) standardGeneric('region_fish')) 
setMethod('region_fish', signature('MFCLFrqStats'),function(object) return(slot(object, 'region_fish'))) 
#'region_fish
#'@export 
setGeneric('region_fish<-', function(object, ..., value) standardGeneric('region_fish<-')) 
setReplaceMethod('region_fish', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['region_fish'])),
                                function(object, value){slot(object, 'region_fish') <- value; return(object)}) 
#'move_matrix
#'@export move_matrix
setGeneric('move_matrix', function(object, ...) standardGeneric('move_matrix')) 
setMethod('move_matrix', signature('MFCLFrqStats'),function(object) return(slot(object, 'move_matrix'))) 
#'move_matrix
#'@export 
setGeneric('move_matrix<-', function(object, ..., value) standardGeneric('move_matrix<-')) 
setReplaceMethod('move_matrix', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['move_matrix'])),
                                function(object, value){slot(object, 'move_matrix') <- value; return(object)}) 
#'data_flags
#'@export data_flags
setGeneric('data_flags', function(object, ...) standardGeneric('data_flags')) 
setMethod('data_flags', signature('MFCLFrqStats'),function(object) return(slot(object, 'data_flags'))) 
#'data_flags
#'@export 
setGeneric('data_flags<-', function(object, ..., value) standardGeneric('data_flags<-')) 
setReplaceMethod('data_flags', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['data_flags'])),
                                function(object, value){slot(object, 'data_flags') <- value; return(object)}) 
#'season_flags
#'@export season_flags
setGeneric('season_flags', function(object, ...) standardGeneric('season_flags')) 
setMethod('season_flags', signature('MFCLFrqStats'),function(object) return(slot(object, 'season_flags'))) 
#'season_flags
#'@export 
setGeneric('season_flags<-', function(object, ..., value) standardGeneric('season_flags<-')) 
setReplaceMethod('season_flags', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['season_flags'])),
                                function(object, value){slot(object, 'season_flags') <- value; return(object)}) 
#'n_move_yr
#'@export n_move_yr
setGeneric('n_move_yr', function(object, ...) standardGeneric('n_move_yr')) 
setMethod('n_move_yr', signature('MFCLFrqStats'),function(object) return(slot(object, 'n_move_yr'))) 
#'n_move_yr
#'@export 
setGeneric('n_move_yr<-', function(object, ..., value) standardGeneric('n_move_yr<-')) 
setReplaceMethod('n_move_yr', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['n_move_yr'])),
                                function(object, value){slot(object, 'n_move_yr') <- value; return(object)}) 
#'move_weeks
#'@export move_weeks
setGeneric('move_weeks', function(object, ...) standardGeneric('move_weeks')) 
setMethod('move_weeks', signature('MFCLFrqStats'),function(object) return(slot(object, 'move_weeks'))) 
#'move_weeks
#'@export 
setGeneric('move_weeks<-', function(object, ..., value) standardGeneric('move_weeks<-')) 
setReplaceMethod('move_weeks', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['move_weeks'])),
                                function(object, value){slot(object, 'move_weeks') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLFrqStats'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 
# class  MFCLLenFreq 
#'lf_range
#'@export lf_range
setGeneric('lf_range', function(object, ...) standardGeneric('lf_range')) 
setMethod('lf_range', signature('MFCLLenFreq'),function(object) return(slot(object, 'lf_range'))) 
#'lf_range
#'@export 
setGeneric('lf_range<-', function(object, ..., value) standardGeneric('lf_range<-')) 
setReplaceMethod('lf_range', signature(object='MFCLLenFreq', value=unname(getSlots('MFCLLenFreq')['lf_range'])),
                                function(object, value){slot(object, 'lf_range') <- value; return(object)}) 
#'age_nage
#'@export age_nage
setGeneric('age_nage', function(object, ...) standardGeneric('age_nage')) 
setMethod('age_nage', signature('MFCLLenFreq'),function(object) return(slot(object, 'age_nage'))) 
#'age_nage
#'@export 
setGeneric('age_nage<-', function(object, ..., value) standardGeneric('age_nage<-')) 
setReplaceMethod('age_nage', signature(object='MFCLLenFreq', value=unname(getSlots('MFCLLenFreq')['age_nage'])),
                                function(object, value){slot(object, 'age_nage') <- value; return(object)}) 
#'freq
#'@export freq
setGeneric('freq', function(object, ...) standardGeneric('freq')) 
setMethod('freq', signature('MFCLLenFreq'),function(object) return(slot(object, 'freq'))) 
#'freq
#'@export 
setGeneric('freq<-', function(object, ..., value) standardGeneric('freq<-')) 
setReplaceMethod('freq', signature(object='MFCLLenFreq', value=unname(getSlots('MFCLLenFreq')['freq'])),
                                function(object, value){slot(object, 'freq') <- value; return(object)}) 
# class  MFCLBiol 
#'m
#'@export m
setGeneric('m', function(object, ...) standardGeneric('m')) 
setMethod('m', signature('MFCLBiol'),function(object) return(slot(object, 'm'))) 
#'m
#'@export 
setGeneric('m<-', function(object, ..., value) standardGeneric('m<-')) 
setReplaceMethod('m', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['m'])),
                                function(object, value){slot(object, 'm') <- value; return(object)}) 
#'m_devs_age
#'@export m_devs_age
setGeneric('m_devs_age', function(object, ...) standardGeneric('m_devs_age')) 
setMethod('m_devs_age', signature('MFCLBiol'),function(object) return(slot(object, 'm_devs_age'))) 
#'m_devs_age
#'@export 
setGeneric('m_devs_age<-', function(object, ..., value) standardGeneric('m_devs_age<-')) 
setReplaceMethod('m_devs_age', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['m_devs_age'])),
                                function(object, value){slot(object, 'm_devs_age') <- value; return(object)}) 
#'log_m
#'@export log_m
setGeneric('log_m', function(object, ...) standardGeneric('log_m')) 
setMethod('log_m', signature('MFCLBiol'),function(object) return(slot(object, 'log_m'))) 
#'log_m
#'@export 
setGeneric('log_m<-', function(object, ..., value) standardGeneric('log_m<-')) 
setReplaceMethod('log_m', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['log_m'])),
                                function(object, value){slot(object, 'log_m') <- value; return(object)}) 
#'mat
#'@export mat
setGeneric('mat', function(object, ...) standardGeneric('mat')) 
setMethod('mat', signature('MFCLBiol'),function(object) return(slot(object, 'mat'))) 
#'mat
#'@export 
setGeneric('mat<-', function(object, ..., value) standardGeneric('mat<-')) 
setReplaceMethod('mat', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['mat'])),
                                function(object, value){slot(object, 'mat') <- value; return(object)}) 
#'growth
#'@export growth
setGeneric('growth', function(object, ...) standardGeneric('growth')) 
setMethod('growth', signature('MFCLBiol'),function(object) return(slot(object, 'growth'))) 
#'growth
#'@export 
setGeneric('growth<-', function(object, ..., value) standardGeneric('growth<-')) 
setReplaceMethod('growth', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['growth'])),
                                function(object, value){slot(object, 'growth') <- value; return(object)}) 
#'richards
#'@export richards
setGeneric('richards', function(object, ...) standardGeneric('richards')) 
setMethod('richards', signature('MFCLBiol'),function(object) return(slot(object, 'richards'))) 
#'richards
#'@export 
setGeneric('richards<-', function(object, ..., value) standardGeneric('richards<-')) 
setReplaceMethod('richards', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['richards'])),
                                function(object, value){slot(object, 'richards') <- value; return(object)}) 
#'growth_var_pars
#'@export growth_var_pars
setGeneric('growth_var_pars', function(object, ...) standardGeneric('growth_var_pars')) 
setMethod('growth_var_pars', signature('MFCLBiol'),function(object) return(slot(object, 'growth_var_pars'))) 
#'growth_var_pars
#'@export 
setGeneric('growth_var_pars<-', function(object, ..., value) standardGeneric('growth_var_pars<-')) 
setReplaceMethod('growth_var_pars', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['growth_var_pars'])),
                                function(object, value){slot(object, 'growth_var_pars') <- value; return(object)}) 
#'n_mean_constraints
#'@export n_mean_constraints
setGeneric('n_mean_constraints', function(object, ...) standardGeneric('n_mean_constraints')) 
setMethod('n_mean_constraints', signature('MFCLBiol'),function(object) return(slot(object, 'n_mean_constraints'))) 
#'n_mean_constraints
#'@export 
setGeneric('n_mean_constraints<-', function(object, ..., value) standardGeneric('n_mean_constraints<-')) 
setReplaceMethod('n_mean_constraints', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['n_mean_constraints'])),
                                function(object, value){slot(object, 'n_mean_constraints') <- value; return(object)}) 
#'growth_devs_age
#'@export growth_devs_age
setGeneric('growth_devs_age', function(object, ...) standardGeneric('growth_devs_age')) 
setMethod('growth_devs_age', signature('MFCLBiol'),function(object) return(slot(object, 'growth_devs_age'))) 
#'growth_devs_age
#'@export 
setGeneric('growth_devs_age<-', function(object, ..., value) standardGeneric('growth_devs_age<-')) 
setReplaceMethod('growth_devs_age', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['growth_devs_age'])),
                                function(object, value){slot(object, 'growth_devs_age') <- value; return(object)}) 
#'growth_curve_devs
#'@export growth_curve_devs
setGeneric('growth_curve_devs', function(object, ...) standardGeneric('growth_curve_devs')) 
setMethod('growth_curve_devs', signature('MFCLBiol'),function(object) return(slot(object, 'growth_curve_devs'))) 
#'growth_curve_devs
#'@export 
setGeneric('growth_curve_devs<-', function(object, ..., value) standardGeneric('growth_curve_devs<-')) 
setReplaceMethod('growth_curve_devs', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['growth_curve_devs'])),
                                function(object, value){slot(object, 'growth_curve_devs') <- value; return(object)}) 
#'growth_devs_cohort
#'@export growth_devs_cohort
setGeneric('growth_devs_cohort', function(object, ...) standardGeneric('growth_devs_cohort')) 
setMethod('growth_devs_cohort', signature('MFCLBiol'),function(object) return(slot(object, 'growth_devs_cohort'))) 
#'growth_devs_cohort
#'@export 
setGeneric('growth_devs_cohort<-', function(object, ..., value) standardGeneric('growth_devs_cohort<-')) 
setReplaceMethod('growth_devs_cohort', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['growth_devs_cohort'])),
                                function(object, value){slot(object, 'growth_devs_cohort') <- value; return(object)}) 
#'season_growth_pars
#'@export season_growth_pars
setGeneric('season_growth_pars', function(object, ...) standardGeneric('season_growth_pars')) 
setMethod('season_growth_pars', signature('MFCLBiol'),function(object) return(slot(object, 'season_growth_pars'))) 
#'season_growth_pars
#'@export 
setGeneric('season_growth_pars<-', function(object, ..., value) standardGeneric('season_growth_pars<-')) 
setReplaceMethod('season_growth_pars', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['season_growth_pars'])),
                                function(object, value){slot(object, 'season_growth_pars') <- value; return(object)}) 
#'len_bias_pars
#'@export len_bias_pars
setGeneric('len_bias_pars', function(object, ...) standardGeneric('len_bias_pars')) 
setMethod('len_bias_pars', signature('MFCLBiol'),function(object) return(slot(object, 'len_bias_pars'))) 
#'len_bias_pars
#'@export 
setGeneric('len_bias_pars<-', function(object, ..., value) standardGeneric('len_bias_pars<-')) 
setReplaceMethod('len_bias_pars', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['len_bias_pars'])),
                                function(object, value){slot(object, 'len_bias_pars') <- value; return(object)}) 
#'common_len_bias_pars
#'@export common_len_bias_pars
setGeneric('common_len_bias_pars', function(object, ...) standardGeneric('common_len_bias_pars')) 
setMethod('common_len_bias_pars', signature('MFCLBiol'),function(object) return(slot(object, 'common_len_bias_pars'))) 
#'common_len_bias_pars
#'@export 
setGeneric('common_len_bias_pars<-', function(object, ..., value) standardGeneric('common_len_bias_pars<-')) 
setReplaceMethod('common_len_bias_pars', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['common_len_bias_pars'])),
                                function(object, value){slot(object, 'common_len_bias_pars') <- value; return(object)}) 
#'common_len_bias_coffs
#'@export common_len_bias_coffs
setGeneric('common_len_bias_coffs', function(object, ...) standardGeneric('common_len_bias_coffs')) 
setMethod('common_len_bias_coffs', signature('MFCLBiol'),function(object) return(slot(object, 'common_len_bias_coffs'))) 
#'common_len_bias_coffs
#'@export 
setGeneric('common_len_bias_coffs<-', function(object, ..., value) standardGeneric('common_len_bias_coffs<-')) 
setReplaceMethod('common_len_bias_coffs', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['common_len_bias_coffs'])),
                                function(object, value){slot(object, 'common_len_bias_coffs') <- value; return(object)}) 
#'dimensions
#'@export dimensions
setGeneric('dimensions', function(object, ...) standardGeneric('dimensions')) 
setMethod('dimensions', signature('MFCLBiol'),function(object) return(slot(object, 'dimensions'))) 
#'dimensions
#'@export 
setGeneric('dimensions<-', function(object, ..., value) standardGeneric('dimensions<-')) 
setReplaceMethod('dimensions', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['dimensions'])),
                                function(object, value){slot(object, 'dimensions') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLBiol'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 
# class  MFCLFlags 
#'flags
#'@export flags
setGeneric('flags', function(object, ...) standardGeneric('flags')) 
setMethod('flags', signature('MFCLFlags'),function(object) return(slot(object, 'flags'))) 
#'flags
#'@export 
setGeneric('flags<-', function(object, ..., value) standardGeneric('flags<-')) 
setReplaceMethod('flags', signature(object='MFCLFlags', value=unname(getSlots('MFCLFlags')['flags'])),
                                function(object, value){slot(object, 'flags') <- value; return(object)}) 
#'unused
#'@export unused
setGeneric('unused', function(object, ...) standardGeneric('unused')) 
setMethod('unused', signature('MFCLFlags'),function(object) return(slot(object, 'unused'))) 
#'unused
#'@export 
setGeneric('unused<-', function(object, ..., value) standardGeneric('unused<-')) 
setReplaceMethod('unused', signature(object='MFCLFlags', value=unname(getSlots('MFCLFlags')['unused'])),
                                function(object, value){slot(object, 'unused') <- value; return(object)}) 
# class  MFCLTagRep 
#'tag_fish_rep_rate
#'@export tag_fish_rep_rate
setGeneric('tag_fish_rep_rate', function(object, ...) standardGeneric('tag_fish_rep_rate')) 
setMethod('tag_fish_rep_rate', signature('MFCLTagRep'),function(object) return(slot(object, 'tag_fish_rep_rate'))) 
#'tag_fish_rep_rate
#'@export 
setGeneric('tag_fish_rep_rate<-', function(object, ..., value) standardGeneric('tag_fish_rep_rate<-')) 
setReplaceMethod('tag_fish_rep_rate', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['tag_fish_rep_rate'])),
                                function(object, value){slot(object, 'tag_fish_rep_rate') <- value; return(object)}) 
#'tag_fish_rep_grp
#'@export tag_fish_rep_grp
setGeneric('tag_fish_rep_grp', function(object, ...) standardGeneric('tag_fish_rep_grp')) 
setMethod('tag_fish_rep_grp', signature('MFCLTagRep'),function(object) return(slot(object, 'tag_fish_rep_grp'))) 
#'tag_fish_rep_grp
#'@export 
setGeneric('tag_fish_rep_grp<-', function(object, ..., value) standardGeneric('tag_fish_rep_grp<-')) 
setReplaceMethod('tag_fish_rep_grp', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['tag_fish_rep_grp'])),
                                function(object, value){slot(object, 'tag_fish_rep_grp') <- value; return(object)}) 
#'tag_fish_rep_flags
#'@export tag_fish_rep_flags
setGeneric('tag_fish_rep_flags', function(object, ...) standardGeneric('tag_fish_rep_flags')) 
setMethod('tag_fish_rep_flags', signature('MFCLTagRep'),function(object) return(slot(object, 'tag_fish_rep_flags'))) 
#'tag_fish_rep_flags
#'@export 
setGeneric('tag_fish_rep_flags<-', function(object, ..., value) standardGeneric('tag_fish_rep_flags<-')) 
setReplaceMethod('tag_fish_rep_flags', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['tag_fish_rep_flags'])),
                                function(object, value){slot(object, 'tag_fish_rep_flags') <- value; return(object)}) 
#'tag_fish_rep_target
#'@export tag_fish_rep_target
setGeneric('tag_fish_rep_target', function(object, ...) standardGeneric('tag_fish_rep_target')) 
setMethod('tag_fish_rep_target', signature('MFCLTagRep'),function(object) return(slot(object, 'tag_fish_rep_target'))) 
#'tag_fish_rep_target
#'@export 
setGeneric('tag_fish_rep_target<-', function(object, ..., value) standardGeneric('tag_fish_rep_target<-')) 
setReplaceMethod('tag_fish_rep_target', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['tag_fish_rep_target'])),
                                function(object, value){slot(object, 'tag_fish_rep_target') <- value; return(object)}) 
#'tag_fish_rep_pen
#'@export tag_fish_rep_pen
setGeneric('tag_fish_rep_pen', function(object, ...) standardGeneric('tag_fish_rep_pen')) 
setMethod('tag_fish_rep_pen', signature('MFCLTagRep'),function(object) return(slot(object, 'tag_fish_rep_pen'))) 
#'tag_fish_rep_pen
#'@export 
setGeneric('tag_fish_rep_pen<-', function(object, ..., value) standardGeneric('tag_fish_rep_pen<-')) 
setReplaceMethod('tag_fish_rep_pen', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['tag_fish_rep_pen'])),
                                function(object, value){slot(object, 'tag_fish_rep_pen') <- value; return(object)}) 
#'rep_rate_dev_coffs
#'@export rep_rate_dev_coffs
setGeneric('rep_rate_dev_coffs', function(object, ...) standardGeneric('rep_rate_dev_coffs')) 
setMethod('rep_rate_dev_coffs', signature('MFCLTagRep'),function(object) return(slot(object, 'rep_rate_dev_coffs'))) 
#'rep_rate_dev_coffs
#'@export 
setGeneric('rep_rate_dev_coffs<-', function(object, ..., value) standardGeneric('rep_rate_dev_coffs<-')) 
setReplaceMethod('rep_rate_dev_coffs', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['rep_rate_dev_coffs'])),
                                function(object, value){slot(object, 'rep_rate_dev_coffs') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLTagRep'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 
# class  MFCLRec 
#'rec_init_pop_diff
#'@export rec_init_pop_diff
setGeneric('rec_init_pop_diff', function(object, ...) standardGeneric('rec_init_pop_diff')) 
setMethod('rec_init_pop_diff', signature('MFCLRec'),function(object) return(slot(object, 'rec_init_pop_diff'))) 
#'rec_init_pop_diff
#'@export 
setGeneric('rec_init_pop_diff<-', function(object, ..., value) standardGeneric('rec_init_pop_diff<-')) 
setReplaceMethod('rec_init_pop_diff', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['rec_init_pop_diff'])),
                                function(object, value){slot(object, 'rec_init_pop_diff') <- value; return(object)}) 
#'rec_times
#'@export rec_times
setGeneric('rec_times', function(object, ...) standardGeneric('rec_times')) 
setMethod('rec_times', signature('MFCLRec'),function(object) return(slot(object, 'rec_times'))) 
#'rec_times
#'@export 
setGeneric('rec_times<-', function(object, ..., value) standardGeneric('rec_times<-')) 
setReplaceMethod('rec_times', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['rec_times'])),
                                function(object, value){slot(object, 'rec_times') <- value; return(object)}) 
#'rel_rec
#'@export rel_rec
setGeneric('rel_rec', function(object, ...) standardGeneric('rel_rec')) 
setMethod('rel_rec', signature('MFCLRec'),function(object) return(slot(object, 'rel_rec'))) 
#'rel_rec
#'@export 
setGeneric('rel_rec<-', function(object, ..., value) standardGeneric('rel_rec<-')) 
setReplaceMethod('rel_rec', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['rel_rec'])),
                                function(object, value){slot(object, 'rel_rec') <- value; return(object)}) 
#'tot_pop
#'@export tot_pop
setGeneric('tot_pop', function(object, ...) standardGeneric('tot_pop')) 
setMethod('tot_pop', signature('MFCLRec'),function(object) return(slot(object, 'tot_pop'))) 
#'tot_pop
#'@export 
setGeneric('tot_pop<-', function(object, ..., value) standardGeneric('tot_pop<-')) 
setReplaceMethod('tot_pop', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['tot_pop'])),
                                function(object, value){slot(object, 'tot_pop') <- value; return(object)}) 
#'tot_pop_implicit
#'@export tot_pop_implicit
setGeneric('tot_pop_implicit', function(object, ...) standardGeneric('tot_pop_implicit')) 
setMethod('tot_pop_implicit', signature('MFCLRec'),function(object) return(slot(object, 'tot_pop_implicit'))) 
#'tot_pop_implicit
#'@export 
setGeneric('tot_pop_implicit<-', function(object, ..., value) standardGeneric('tot_pop_implicit<-')) 
setReplaceMethod('tot_pop_implicit', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['tot_pop_implicit'])),
                                function(object, value){slot(object, 'tot_pop_implicit') <- value; return(object)}) 
#'rel_ini_pop
#'@export rel_ini_pop
setGeneric('rel_ini_pop', function(object, ...) standardGeneric('rel_ini_pop')) 
setMethod('rel_ini_pop', signature('MFCLRec'),function(object) return(slot(object, 'rel_ini_pop'))) 
#'rel_ini_pop
#'@export 
setGeneric('rel_ini_pop<-', function(object, ..., value) standardGeneric('rel_ini_pop<-')) 
setReplaceMethod('rel_ini_pop', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['rel_ini_pop'])),
                                function(object, value){slot(object, 'rel_ini_pop') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLRec'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 
# class  MFCLRegion 
#'control_flags
#'@export control_flags
setGeneric('control_flags', function(object, ...) standardGeneric('control_flags')) 
setMethod('control_flags', signature('MFCLRegion'),function(object) return(slot(object, 'control_flags'))) 
#'control_flags
#'@export 
setGeneric('control_flags<-', function(object, ..., value) standardGeneric('control_flags<-')) 
setReplaceMethod('control_flags', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['control_flags'])),
                                function(object, value){slot(object, 'control_flags') <- value; return(object)}) 
#'move_map
#'@export move_map
setGeneric('move_map', function(object, ...) standardGeneric('move_map')) 
setMethod('move_map', signature('MFCLRegion'),function(object) return(slot(object, 'move_map'))) 
#'move_map
#'@export 
setGeneric('move_map<-', function(object, ..., value) standardGeneric('move_map<-')) 
setReplaceMethod('move_map', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['move_map'])),
                                function(object, value){slot(object, 'move_map') <- value; return(object)}) 
#'diff_coffs
#'@export diff_coffs
setGeneric('diff_coffs', function(object, ...) standardGeneric('diff_coffs')) 
setMethod('diff_coffs', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs'))) 
#'diff_coffs
#'@export 
setGeneric('diff_coffs<-', function(object, ..., value) standardGeneric('diff_coffs<-')) 
setReplaceMethod('diff_coffs', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs'])),
                                function(object, value){slot(object, 'diff_coffs') <- value; return(object)}) 
#'diff_coffs_mat
#'@export diff_coffs_mat
setGeneric('diff_coffs_mat', function(object, ...) standardGeneric('diff_coffs_mat')) 
setMethod('diff_coffs_mat', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_mat'))) 
#'diff_coffs_mat
#'@export 
setGeneric('diff_coffs_mat<-', function(object, ..., value) standardGeneric('diff_coffs_mat<-')) 
setReplaceMethod('diff_coffs_mat', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_mat'])),
                                function(object, value){slot(object, 'diff_coffs_mat') <- value; return(object)}) 
#'diff_coffs_age_ssn
#'@export diff_coffs_age_ssn
setGeneric('diff_coffs_age_ssn', function(object, ...) standardGeneric('diff_coffs_age_ssn')) 
setMethod('diff_coffs_age_ssn', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_age_ssn'))) 
#'diff_coffs_age_ssn
#'@export 
setGeneric('diff_coffs_age_ssn<-', function(object, ..., value) standardGeneric('diff_coffs_age_ssn<-')) 
setReplaceMethod('diff_coffs_age_ssn', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_age_ssn'])),
                                function(object, value){slot(object, 'diff_coffs_age_ssn') <- value; return(object)}) 
#'diff_coffs_age_period
#'@export diff_coffs_age_period
setGeneric('diff_coffs_age_period', function(object, ...) standardGeneric('diff_coffs_age_period')) 
setMethod('diff_coffs_age_period', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_age_period'))) 
#'diff_coffs_age_period
#'@export 
setGeneric('diff_coffs_age_period<-', function(object, ..., value) standardGeneric('diff_coffs_age_period<-')) 
setReplaceMethod('diff_coffs_age_period', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_age_period'])),
                                function(object, value){slot(object, 'diff_coffs_age_period') <- value; return(object)}) 
#'diff_coffs_age
#'@export diff_coffs_age
setGeneric('diff_coffs_age', function(object, ...) standardGeneric('diff_coffs_age')) 
setMethod('diff_coffs_age', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_age'))) 
#'diff_coffs_age
#'@export 
setGeneric('diff_coffs_age<-', function(object, ..., value) standardGeneric('diff_coffs_age<-')) 
setReplaceMethod('diff_coffs_age', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_age'])),
                                function(object, value){slot(object, 'diff_coffs_age') <- value; return(object)}) 
#'diff_coffs_nl
#'@export diff_coffs_nl
setGeneric('diff_coffs_nl', function(object, ...) standardGeneric('diff_coffs_nl')) 
setMethod('diff_coffs_nl', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_nl'))) 
#'diff_coffs_nl
#'@export 
setGeneric('diff_coffs_nl<-', function(object, ..., value) standardGeneric('diff_coffs_nl<-')) 
setReplaceMethod('diff_coffs_nl', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_nl'])),
                                function(object, value){slot(object, 'diff_coffs_nl') <- value; return(object)}) 
#'diff_coffs_priors
#'@export diff_coffs_priors
setGeneric('diff_coffs_priors', function(object, ...) standardGeneric('diff_coffs_priors')) 
setMethod('diff_coffs_priors', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_priors'))) 
#'diff_coffs_priors
#'@export 
setGeneric('diff_coffs_priors<-', function(object, ..., value) standardGeneric('diff_coffs_priors<-')) 
setReplaceMethod('diff_coffs_priors', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_priors'])),
                                function(object, value){slot(object, 'diff_coffs_priors') <- value; return(object)}) 
#'diff_coffs_age_priors
#'@export diff_coffs_age_priors
setGeneric('diff_coffs_age_priors', function(object, ...) standardGeneric('diff_coffs_age_priors')) 
setMethod('diff_coffs_age_priors', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_age_priors'))) 
#'diff_coffs_age_priors
#'@export 
setGeneric('diff_coffs_age_priors<-', function(object, ..., value) standardGeneric('diff_coffs_age_priors<-')) 
setReplaceMethod('diff_coffs_age_priors', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_age_priors'])),
                                function(object, value){slot(object, 'diff_coffs_age_priors') <- value; return(object)}) 
#'diff_coffs_nl_priors
#'@export diff_coffs_nl_priors
setGeneric('diff_coffs_nl_priors', function(object, ...) standardGeneric('diff_coffs_nl_priors')) 
setMethod('diff_coffs_nl_priors', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_nl_priors'))) 
#'diff_coffs_nl_priors
#'@export 
setGeneric('diff_coffs_nl_priors<-', function(object, ..., value) standardGeneric('diff_coffs_nl_priors<-')) 
setReplaceMethod('diff_coffs_nl_priors', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_nl_priors'])),
                                function(object, value){slot(object, 'diff_coffs_nl_priors') <- value; return(object)}) 
#'region_rec_var
#'@export region_rec_var
setGeneric('region_rec_var', function(object, ...) standardGeneric('region_rec_var')) 
setMethod('region_rec_var', signature('MFCLRegion'),function(object) return(slot(object, 'region_rec_var'))) 
#'region_rec_var
#'@export 
setGeneric('region_rec_var<-', function(object, ..., value) standardGeneric('region_rec_var<-')) 
setReplaceMethod('region_rec_var', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['region_rec_var'])),
                                function(object, value){slot(object, 'region_rec_var') <- value; return(object)}) 
#'region_pars
#'@export region_pars
setGeneric('region_pars', function(object, ...) standardGeneric('region_pars')) 
setMethod('region_pars', signature('MFCLRegion'),function(object) return(slot(object, 'region_pars'))) 
#'region_pars
#'@export 
setGeneric('region_pars<-', function(object, ..., value) standardGeneric('region_pars<-')) 
setReplaceMethod('region_pars', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['region_pars'])),
                                function(object, value){slot(object, 'region_pars') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLRegion'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 
# class  MFCLSel 
#'availability_coffs
#'@export availability_coffs
setGeneric('availability_coffs', function(object, ...) standardGeneric('availability_coffs')) 
setMethod('availability_coffs', signature('MFCLSel'),function(object) return(slot(object, 'availability_coffs'))) 
#'availability_coffs
#'@export 
setGeneric('availability_coffs<-', function(object, ..., value) standardGeneric('availability_coffs<-')) 
setReplaceMethod('availability_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['availability_coffs'])),
                                function(object, value){slot(object, 'availability_coffs') <- value; return(object)}) 
#'fishery_sel
#'@export fishery_sel
setGeneric('fishery_sel', function(object, ...) standardGeneric('fishery_sel')) 
setMethod('fishery_sel', signature('MFCLSel'),function(object) return(slot(object, 'fishery_sel'))) 
#'fishery_sel
#'@export 
setGeneric('fishery_sel<-', function(object, ..., value) standardGeneric('fishery_sel<-')) 
setReplaceMethod('fishery_sel', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['fishery_sel'])),
                                function(object, value){slot(object, 'fishery_sel') <- value; return(object)}) 
#'fishery_sel_age_comp
#'@export fishery_sel_age_comp
setGeneric('fishery_sel_age_comp', function(object, ...) standardGeneric('fishery_sel_age_comp')) 
setMethod('fishery_sel_age_comp', signature('MFCLSel'),function(object) return(slot(object, 'fishery_sel_age_comp'))) 
#'fishery_sel_age_comp
#'@export 
setGeneric('fishery_sel_age_comp<-', function(object, ..., value) standardGeneric('fishery_sel_age_comp<-')) 
setReplaceMethod('fishery_sel_age_comp', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['fishery_sel_age_comp'])),
                                function(object, value){slot(object, 'fishery_sel_age_comp') <- value; return(object)}) 
#'av_q_coffs
#'@export av_q_coffs
setGeneric('av_q_coffs', function(object, ...) standardGeneric('av_q_coffs')) 
setMethod('av_q_coffs', signature('MFCLSel'),function(object) return(slot(object, 'av_q_coffs'))) 
#'av_q_coffs
#'@export 
setGeneric('av_q_coffs<-', function(object, ..., value) standardGeneric('av_q_coffs<-')) 
setReplaceMethod('av_q_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['av_q_coffs'])),
                                function(object, value){slot(object, 'av_q_coffs') <- value; return(object)}) 
#'ini_q_coffs
#'@export ini_q_coffs
setGeneric('ini_q_coffs', function(object, ...) standardGeneric('ini_q_coffs')) 
setMethod('ini_q_coffs', signature('MFCLSel'),function(object) return(slot(object, 'ini_q_coffs'))) 
#'ini_q_coffs
#'@export 
setGeneric('ini_q_coffs<-', function(object, ..., value) standardGeneric('ini_q_coffs<-')) 
setReplaceMethod('ini_q_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['ini_q_coffs'])),
                                function(object, value){slot(object, 'ini_q_coffs') <- value; return(object)}) 
#'q0_miss
#'@export q0_miss
setGeneric('q0_miss', function(object, ...) standardGeneric('q0_miss')) 
setMethod('q0_miss', signature('MFCLSel'),function(object) return(slot(object, 'q0_miss'))) 
#'q0_miss
#'@export 
setGeneric('q0_miss<-', function(object, ..., value) standardGeneric('q0_miss<-')) 
setReplaceMethod('q0_miss', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['q0_miss'])),
                                function(object, value){slot(object, 'q0_miss') <- value; return(object)}) 
#'q_dev_coffs
#'@export q_dev_coffs
setGeneric('q_dev_coffs', function(object, ...) standardGeneric('q_dev_coffs')) 
setMethod('q_dev_coffs', signature('MFCLSel'),function(object) return(slot(object, 'q_dev_coffs'))) 
#'q_dev_coffs
#'@export 
setGeneric('q_dev_coffs<-', function(object, ..., value) standardGeneric('q_dev_coffs<-')) 
setReplaceMethod('q_dev_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['q_dev_coffs'])),
                                function(object, value){slot(object, 'q_dev_coffs') <- value; return(object)}) 
#'effort_dev_coffs
#'@export effort_dev_coffs
setGeneric('effort_dev_coffs', function(object, ...) standardGeneric('effort_dev_coffs')) 
setMethod('effort_dev_coffs', signature('MFCLSel'),function(object) return(slot(object, 'effort_dev_coffs'))) 
#'effort_dev_coffs
#'@export 
setGeneric('effort_dev_coffs<-', function(object, ..., value) standardGeneric('effort_dev_coffs<-')) 
setReplaceMethod('effort_dev_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['effort_dev_coffs'])),
                                function(object, value){slot(object, 'effort_dev_coffs') <- value; return(object)}) 
#'catch_dev_coffs
#'@export catch_dev_coffs
setGeneric('catch_dev_coffs', function(object, ...) standardGeneric('catch_dev_coffs')) 
setMethod('catch_dev_coffs', signature('MFCLSel'),function(object) return(slot(object, 'catch_dev_coffs'))) 
#'catch_dev_coffs
#'@export 
setGeneric('catch_dev_coffs<-', function(object, ..., value) standardGeneric('catch_dev_coffs<-')) 
setReplaceMethod('catch_dev_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['catch_dev_coffs'])),
                                function(object, value){slot(object, 'catch_dev_coffs') <- value; return(object)}) 
#'catch_dev_coffs_flag
#'@export catch_dev_coffs_flag
setGeneric('catch_dev_coffs_flag', function(object, ...) standardGeneric('catch_dev_coffs_flag')) 
setMethod('catch_dev_coffs_flag', signature('MFCLSel'),function(object) return(slot(object, 'catch_dev_coffs_flag'))) 
#'catch_dev_coffs_flag
#'@export 
setGeneric('catch_dev_coffs_flag<-', function(object, ..., value) standardGeneric('catch_dev_coffs_flag<-')) 
setReplaceMethod('catch_dev_coffs_flag', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['catch_dev_coffs_flag'])),
                                function(object, value){slot(object, 'catch_dev_coffs_flag') <- value; return(object)}) 
#'sel_dev_corr
#'@export sel_dev_corr
setGeneric('sel_dev_corr', function(object, ...) standardGeneric('sel_dev_corr')) 
setMethod('sel_dev_corr', signature('MFCLSel'),function(object) return(slot(object, 'sel_dev_corr'))) 
#'sel_dev_corr
#'@export 
setGeneric('sel_dev_corr<-', function(object, ..., value) standardGeneric('sel_dev_corr<-')) 
setReplaceMethod('sel_dev_corr', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['sel_dev_corr'])),
                                function(object, value){slot(object, 'sel_dev_corr') <- value; return(object)}) 
#'sel_dev_coffs
#'@export sel_dev_coffs
setGeneric('sel_dev_coffs', function(object, ...) standardGeneric('sel_dev_coffs')) 
setMethod('sel_dev_coffs', signature('MFCLSel'),function(object) return(slot(object, 'sel_dev_coffs'))) 
#'sel_dev_coffs
#'@export 
setGeneric('sel_dev_coffs<-', function(object, ..., value) standardGeneric('sel_dev_coffs<-')) 
setReplaceMethod('sel_dev_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['sel_dev_coffs'])),
                                function(object, value){slot(object, 'sel_dev_coffs') <- value; return(object)}) 
#'sel_dev_coffs2
#'@export sel_dev_coffs2
setGeneric('sel_dev_coffs2', function(object, ...) standardGeneric('sel_dev_coffs2')) 
setMethod('sel_dev_coffs2', signature('MFCLSel'),function(object) return(slot(object, 'sel_dev_coffs2'))) 
#'sel_dev_coffs2
#'@export 
setGeneric('sel_dev_coffs2<-', function(object, ..., value) standardGeneric('sel_dev_coffs2<-')) 
setReplaceMethod('sel_dev_coffs2', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['sel_dev_coffs2'])),
                                function(object, value){slot(object, 'sel_dev_coffs2') <- value; return(object)}) 
#'season_q_pars
#'@export season_q_pars
setGeneric('season_q_pars', function(object, ...) standardGeneric('season_q_pars')) 
setMethod('season_q_pars', signature('MFCLSel'),function(object) return(slot(object, 'season_q_pars'))) 
#'season_q_pars
#'@export 
setGeneric('season_q_pars<-', function(object, ..., value) standardGeneric('season_q_pars<-')) 
setReplaceMethod('season_q_pars', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['season_q_pars'])),
                                function(object, value){slot(object, 'season_q_pars') <- value; return(object)}) 
#'fish_params
#'@export fish_params
setGeneric('fish_params', function(object, ...) standardGeneric('fish_params')) 
setMethod('fish_params', signature('MFCLSel'),function(object) return(slot(object, 'fish_params'))) 
#'fish_params
#'@export 
setGeneric('fish_params<-', function(object, ..., value) standardGeneric('fish_params<-')) 
setReplaceMethod('fish_params', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['fish_params'])),
                                function(object, value){slot(object, 'fish_params') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLSel'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 
# class  MFCLParBits 
#'fm_level_devs
#'@export fm_level_devs
setGeneric('fm_level_devs', function(object, ...) standardGeneric('fm_level_devs')) 
setMethod('fm_level_devs', signature('MFCLParBits'),function(object) return(slot(object, 'fm_level_devs'))) 
#'fm_level_devs
#'@export 
setGeneric('fm_level_devs<-', function(object, ..., value) standardGeneric('fm_level_devs<-')) 
setReplaceMethod('fm_level_devs', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['fm_level_devs'])),
                                function(object, value){slot(object, 'fm_level_devs') <- value; return(object)}) 
#'obj_fun
#'@export obj_fun
setGeneric('obj_fun', function(object, ...) standardGeneric('obj_fun')) 
setMethod('obj_fun', signature('MFCLParBits'),function(object) return(slot(object, 'obj_fun'))) 
#'obj_fun
#'@export 
setGeneric('obj_fun<-', function(object, ..., value) standardGeneric('obj_fun<-')) 
setReplaceMethod('obj_fun', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['obj_fun'])),
                                function(object, value){slot(object, 'obj_fun') <- value; return(object)}) 
#'n_pars
#'@export n_pars
setGeneric('n_pars', function(object, ...) standardGeneric('n_pars')) 
setMethod('n_pars', signature('MFCLParBits'),function(object) return(slot(object, 'n_pars'))) 
#'n_pars
#'@export 
setGeneric('n_pars<-', function(object, ..., value) standardGeneric('n_pars<-')) 
setReplaceMethod('n_pars', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['n_pars'])),
                                function(object, value){slot(object, 'n_pars') <- value; return(object)}) 
#'tag_lik
#'@export tag_lik
setGeneric('tag_lik', function(object, ...) standardGeneric('tag_lik')) 
setMethod('tag_lik', signature('MFCLParBits'),function(object) return(slot(object, 'tag_lik'))) 
#'tag_lik
#'@export 
setGeneric('tag_lik<-', function(object, ..., value) standardGeneric('tag_lik<-')) 
setReplaceMethod('tag_lik', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['tag_lik'])),
                                function(object, value){slot(object, 'tag_lik') <- value; return(object)}) 
#'mn_len_pen
#'@export mn_len_pen
setGeneric('mn_len_pen', function(object, ...) standardGeneric('mn_len_pen')) 
setMethod('mn_len_pen', signature('MFCLParBits'),function(object) return(slot(object, 'mn_len_pen'))) 
#'mn_len_pen
#'@export 
setGeneric('mn_len_pen<-', function(object, ..., value) standardGeneric('mn_len_pen<-')) 
setReplaceMethod('mn_len_pen', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['mn_len_pen'])),
                                function(object, value){slot(object, 'mn_len_pen') <- value; return(object)}) 
#'max_grad
#'@export max_grad
setGeneric('max_grad', function(object, ...) standardGeneric('max_grad')) 
setMethod('max_grad', signature('MFCLParBits'),function(object) return(slot(object, 'max_grad'))) 
#'max_grad
#'@export 
setGeneric('max_grad<-', function(object, ..., value) standardGeneric('max_grad<-')) 
setReplaceMethod('max_grad', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['max_grad'])),
                                function(object, value){slot(object, 'max_grad') <- value; return(object)}) 
#'av_fish_mort_inst
#'@export av_fish_mort_inst
setGeneric('av_fish_mort_inst', function(object, ...) standardGeneric('av_fish_mort_inst')) 
setMethod('av_fish_mort_inst', signature('MFCLParBits'),function(object) return(slot(object, 'av_fish_mort_inst'))) 
#'av_fish_mort_inst
#'@export 
setGeneric('av_fish_mort_inst<-', function(object, ..., value) standardGeneric('av_fish_mort_inst<-')) 
setReplaceMethod('av_fish_mort_inst', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['av_fish_mort_inst'])),
                                function(object, value){slot(object, 'av_fish_mort_inst') <- value; return(object)}) 
#'av_fish_mort_year
#'@export av_fish_mort_year
setGeneric('av_fish_mort_year', function(object, ...) standardGeneric('av_fish_mort_year')) 
setMethod('av_fish_mort_year', signature('MFCLParBits'),function(object) return(slot(object, 'av_fish_mort_year'))) 
#'av_fish_mort_year
#'@export 
setGeneric('av_fish_mort_year<-', function(object, ..., value) standardGeneric('av_fish_mort_year<-')) 
setReplaceMethod('av_fish_mort_year', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['av_fish_mort_year'])),
                                function(object, value){slot(object, 'av_fish_mort_year') <- value; return(object)}) 
#'av_fish_mort_age
#'@export av_fish_mort_age
setGeneric('av_fish_mort_age', function(object, ...) standardGeneric('av_fish_mort_age')) 
setMethod('av_fish_mort_age', signature('MFCLParBits'),function(object) return(slot(object, 'av_fish_mort_age'))) 
#'av_fish_mort_age
#'@export 
setGeneric('av_fish_mort_age<-', function(object, ..., value) standardGeneric('av_fish_mort_age<-')) 
setReplaceMethod('av_fish_mort_age', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['av_fish_mort_age'])),
                                function(object, value){slot(object, 'av_fish_mort_age') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLParBits'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 
