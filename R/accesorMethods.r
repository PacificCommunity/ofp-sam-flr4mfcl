# Automatically generated Generic Methods 
# Generated from 'createGenericMethods' 
# Generated on  Tue Feb  2 11:46:03 2016 

############################################################################################################# 
# class  MFCLBase 
#'dimensions
#'@export dimensions
setGeneric('dimensions', function(object, ...) standardGeneric('dimensions')) 
setMethod('dimensions', signature('MFCLBase'),function(object) return(slot(object, 'dimensions'))) 
#'dimensions
#'@export 
setGeneric('dimensions<-', function(object, ..., value) standardGeneric('dimensions<-')) 
setReplaceMethod('dimensions', signature(object='MFCLBase', value=unname(getSlots('MFCLBase')['dimensions'])),
                                function(object, value){slot(object, 'dimensions') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLBase'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLBase', value=unname(getSlots('MFCLBase')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
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

#############################################################################################################
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

#############################################################################################################
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
#setGeneric('dimensions', function(object, ...) standardGeneric('dimensions')) 
setMethod('dimensions', signature('MFCLBiol'),function(object) return(slot(object, 'dimensions'))) 
#'dimensions
#'@export 
#setGeneric('dimensions<-', function(object, ..., value) standardGeneric('dimensions<-')) 
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

#############################################################################################################
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

#############################################################################################################
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

#############################################################################################################
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

#############################################################################################################
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

#############################################################################################################
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

#############################################################################################################
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
#'logistic_normal_params
#'@export logistic_normal_params
setGeneric('logistic_normal_params', function(object, ...) standardGeneric('logistic_normal_params')) 
setMethod('logistic_normal_params', signature('MFCLParBits'),function(object) return(slot(object, 'logistic_normal_params'))) 
#'logistic_normal_params
#'@export 
setGeneric('logistic_normal_params<-', function(object, ..., value) standardGeneric('logistic_normal_params<-')) 
setReplaceMethod('logistic_normal_params', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['logistic_normal_params'])),
                                function(object, value){slot(object, 'logistic_normal_params') <- value; return(object)}) 
#'lagrangian
#'@export lagrangian
setGeneric('lagrangian', function(object, ...) standardGeneric('lagrangian')) 
setMethod('lagrangian', signature('MFCLParBits'),function(object) return(slot(object, 'lagrangian'))) 
#'lagrangian
#'@export 
setGeneric('lagrangian<-', function(object, ..., value) standardGeneric('lagrangian<-')) 
setReplaceMethod('lagrangian', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['lagrangian'])),
                                function(object, value){slot(object, 'lagrangian') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLParBits'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLIniBits 
#'age_pars
#'@export age_pars
setGeneric('age_pars', function(object, ...) standardGeneric('age_pars')) 
setMethod('age_pars', signature('MFCLIniBits'),function(object) return(slot(object, 'age_pars'))) 
#'age_pars
#'@export 
setGeneric('age_pars<-', function(object, ..., value) standardGeneric('age_pars<-')) 
setReplaceMethod('age_pars', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['age_pars'])),
                                function(object, value){slot(object, 'age_pars') <- value; return(object)}) 
#'rec_dist
#'@export rec_dist
setGeneric('rec_dist', function(object, ...) standardGeneric('rec_dist')) 
setMethod('rec_dist', signature('MFCLIniBits'),function(object) return(slot(object, 'rec_dist'))) 
#'rec_dist
#'@export 
setGeneric('rec_dist<-', function(object, ..., value) standardGeneric('rec_dist<-')) 
setReplaceMethod('rec_dist', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['rec_dist'])),
                                function(object, value){slot(object, 'rec_dist') <- value; return(object)}) 
#'lw_params
#'@export lw_params
setGeneric('lw_params', function(object, ...) standardGeneric('lw_params')) 
setMethod('lw_params', signature('MFCLIniBits'),function(object) return(slot(object, 'lw_params'))) 
#'lw_params
#'@export 
setGeneric('lw_params<-', function(object, ..., value) standardGeneric('lw_params<-')) 
setReplaceMethod('lw_params', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['lw_params'])),
                                function(object, value){slot(object, 'lw_params') <- value; return(object)}) 
#'sv
#'@export sv
setGeneric('sv', function(object, ...) standardGeneric('sv')) 
setMethod('sv', signature('MFCLIniBits'),function(object) return(slot(object, 'sv'))) 
#'sv
#'@export 
setGeneric('sv<-', function(object, ..., value) standardGeneric('sv<-')) 
setReplaceMethod('sv', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['sv'])),
                                function(object, value){slot(object, 'sv') <- value; return(object)}) 
#'sd_length_at_age
#'@export sd_length_at_age
setGeneric('sd_length_at_age', function(object, ...) standardGeneric('sd_length_at_age')) 
setMethod('sd_length_at_age', signature('MFCLIniBits'),function(object) return(slot(object, 'sd_length_at_age'))) 
#'sd_length_at_age
#'@export 
setGeneric('sd_length_at_age<-', function(object, ..., value) standardGeneric('sd_length_at_age<-')) 
setReplaceMethod('sd_length_at_age', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['sd_length_at_age'])),
                                function(object, value){slot(object, 'sd_length_at_age') <- value; return(object)}) 
#'sd_length_dep
#'@export sd_length_dep
setGeneric('sd_length_dep', function(object, ...) standardGeneric('sd_length_dep')) 
setMethod('sd_length_dep', signature('MFCLIniBits'),function(object) return(slot(object, 'sd_length_dep'))) 
#'sd_length_dep
#'@export 
setGeneric('sd_length_dep<-', function(object, ..., value) standardGeneric('sd_length_dep<-')) 
setReplaceMethod('sd_length_dep', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['sd_length_dep'])),
                                function(object, value){slot(object, 'sd_length_dep') <- value; return(object)}) 

#############################################################################################################
# class  MFCLRep 
#'fishery_realizations
#'@export fishery_realizations
setGeneric('fishery_realizations', function(object, ...) standardGeneric('fishery_realizations')) 
setMethod('fishery_realizations', signature('MFCLRep'),function(object) return(slot(object, 'fishery_realizations'))) 
#'fishery_realizations
#'@export 
setGeneric('fishery_realizations<-', function(object, ..., value) standardGeneric('fishery_realizations<-')) 
setReplaceMethod('fishery_realizations', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['fishery_realizations'])),
                                function(object, value){slot(object, 'fishery_realizations') <- value; return(object)}) 
#'mean_laa
#'@export mean_laa
setGeneric('mean_laa', function(object, ...) standardGeneric('mean_laa')) 
setMethod('mean_laa', signature('MFCLRep'),function(object) return(slot(object, 'mean_laa'))) 
#'mean_laa
#'@export 
setGeneric('mean_laa<-', function(object, ..., value) standardGeneric('mean_laa<-')) 
setReplaceMethod('mean_laa', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['mean_laa'])),
                                function(object, value){slot(object, 'mean_laa') <- value; return(object)}) 
#'sd_laa
#'@export sd_laa
setGeneric('sd_laa', function(object, ...) standardGeneric('sd_laa')) 
setMethod('sd_laa', signature('MFCLRep'),function(object) return(slot(object, 'sd_laa'))) 
#'sd_laa
#'@export 
setGeneric('sd_laa<-', function(object, ..., value) standardGeneric('sd_laa<-')) 
setReplaceMethod('sd_laa', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['sd_laa'])),
                                function(object, value){slot(object, 'sd_laa') <- value; return(object)}) 
#'m_at_age
#'@export m_at_age
setGeneric('m_at_age', function(object, ...) standardGeneric('m_at_age')) 
setMethod('m_at_age', signature('MFCLRep'),function(object) return(slot(object, 'm_at_age'))) 
#'m_at_age
#'@export 
setGeneric('m_at_age<-', function(object, ..., value) standardGeneric('m_at_age<-')) 
setReplaceMethod('m_at_age', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['m_at_age'])),
                                function(object, value){slot(object, 'm_at_age') <- value; return(object)}) 
#'sel
#'@export sel
setGeneric('sel', function(object, ...) standardGeneric('sel')) 
setMethod('sel', signature('MFCLRep'),function(object) return(slot(object, 'sel'))) 
#'sel
#'@export 
setGeneric('sel<-', function(object, ..., value) standardGeneric('sel<-')) 
setReplaceMethod('sel', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['sel'])),
                                function(object, value){slot(object, 'sel') <- value; return(object)}) 
#'q_fishery
#'@export q_fishery
setGeneric('q_fishery', function(object, ...) standardGeneric('q_fishery')) 
setMethod('q_fishery', signature('MFCLRep'),function(object) return(slot(object, 'q_fishery'))) 
#'q_fishery
#'@export 
setGeneric('q_fishery<-', function(object, ..., value) standardGeneric('q_fishery<-')) 
setReplaceMethod('q_fishery', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['q_fishery'])),
                                function(object, value){slot(object, 'q_fishery') <- value; return(object)}) 
#'q_effdev
#'@export q_effdev
setGeneric('q_effdev', function(object, ...) standardGeneric('q_effdev')) 
setMethod('q_effdev', signature('MFCLRep'),function(object) return(slot(object, 'q_effdev'))) 
#'q_effdev
#'@export 
setGeneric('q_effdev<-', function(object, ..., value) standardGeneric('q_effdev<-')) 
setReplaceMethod('q_effdev', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['q_effdev'])),
                                function(object, value){slot(object, 'q_effdev') <- value; return(object)}) 
#'fm
#'@export fm
setGeneric('fm', function(object, ...) standardGeneric('fm')) 
setMethod('fm', signature('MFCLRep'),function(object) return(slot(object, 'fm'))) 
#'fm
#'@export 
setGeneric('fm<-', function(object, ..., value) standardGeneric('fm<-')) 
setReplaceMethod('fm', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['fm'])),
                                function(object, value){slot(object, 'fm') <- value; return(object)}) 
#'popN
#'@export popN
setGeneric('popN', function(object, ...) standardGeneric('popN')) 
setMethod('popN', signature('MFCLRep'),function(object) return(slot(object, 'popN'))) 
#'popN
#'@export 
setGeneric('popN<-', function(object, ..., value) standardGeneric('popN<-')) 
setReplaceMethod('popN', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['popN'])),
                                function(object, value){slot(object, 'popN') <- value; return(object)}) 
#'rec_region
#'@export rec_region
setGeneric('rec_region', function(object, ...) standardGeneric('rec_region')) 
setMethod('rec_region', signature('MFCLRep'),function(object) return(slot(object, 'rec_region'))) 
#'rec_region
#'@export 
setGeneric('rec_region<-', function(object, ..., value) standardGeneric('rec_region<-')) 
setReplaceMethod('rec_region', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['rec_region'])),
                                function(object, value){slot(object, 'rec_region') <- value; return(object)}) 
#'totalBiomass
#'@export totalBiomass
setGeneric('totalBiomass', function(object, ...) standardGeneric('totalBiomass')) 
setMethod('totalBiomass', signature('MFCLRep'),function(object) return(slot(object, 'totalBiomass'))) 
#'totalBiomass
#'@export 
setGeneric('totalBiomass<-', function(object, ..., value) standardGeneric('totalBiomass<-')) 
setReplaceMethod('totalBiomass', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['totalBiomass'])),
                                function(object, value){slot(object, 'totalBiomass') <- value; return(object)}) 
#'adultBiomass
#'@export adultBiomass
setGeneric('adultBiomass', function(object, ...) standardGeneric('adultBiomass')) 
setMethod('adultBiomass', signature('MFCLRep'),function(object) return(slot(object, 'adultBiomass'))) 
#'adultBiomass
#'@export 
setGeneric('adultBiomass<-', function(object, ..., value) standardGeneric('adultBiomass<-')) 
setReplaceMethod('adultBiomass', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['adultBiomass'])),
                                function(object, value){slot(object, 'adultBiomass') <- value; return(object)}) 
#'adultBiomass_nofish
#'@export adultBiomass_nofish
setGeneric('adultBiomass_nofish', function(object, ...) standardGeneric('adultBiomass_nofish')) 
setMethod('adultBiomass_nofish', signature('MFCLRep'),function(object) return(slot(object, 'adultBiomass_nofish'))) 
#'adultBiomass_nofish
#'@export 
setGeneric('adultBiomass_nofish<-', function(object, ..., value) standardGeneric('adultBiomass_nofish<-')) 
setReplaceMethod('adultBiomass_nofish', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['adultBiomass_nofish'])),
                                function(object, value){slot(object, 'adultBiomass_nofish') <- value; return(object)}) 
#'vulnBiomass
#'@export vulnBiomass
setGeneric('vulnBiomass', function(object, ...) standardGeneric('vulnBiomass')) 
setMethod('vulnBiomass', signature('MFCLRep'),function(object) return(slot(object, 'vulnBiomass'))) 
#'vulnBiomass
#'@export 
setGeneric('vulnBiomass<-', function(object, ..., value) standardGeneric('vulnBiomass<-')) 
setReplaceMethod('vulnBiomass', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['vulnBiomass'])),
                                function(object, value){slot(object, 'vulnBiomass') <- value; return(object)}) 
#'srr
#'@export srr
setGeneric('srr', function(object, ...) standardGeneric('srr')) 
setMethod('srr', signature('MFCLRep'),function(object) return(slot(object, 'srr'))) 
#'srr
#'@export 
setGeneric('srr<-', function(object, ..., value) standardGeneric('srr<-')) 
setReplaceMethod('srr', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['srr'])),
                                function(object, value){slot(object, 'srr') <- value; return(object)}) 
#'ssb
#'@export ssb
setGeneric('ssb', function(object, ...) standardGeneric('ssb')) 
setMethod('ssb', signature('MFCLRep'),function(object) return(slot(object, 'ssb'))) 
#'ssb
#'@export 
setGeneric('ssb<-', function(object, ..., value) standardGeneric('ssb<-')) 
setReplaceMethod('ssb', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['ssb'])),
                                function(object, value){slot(object, 'ssb') <- value; return(object)}) 
#'ssb_obs
#'@export ssb_obs
setGeneric('ssb_obs', function(object, ...) standardGeneric('ssb_obs')) 
setMethod('ssb_obs', signature('MFCLRep'),function(object) return(slot(object, 'ssb_obs'))) 
#'ssb_obs
#'@export 
setGeneric('ssb_obs<-', function(object, ..., value) standardGeneric('ssb_obs<-')) 
setReplaceMethod('ssb_obs', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['ssb_obs'])),
                                function(object, value){slot(object, 'ssb_obs') <- value; return(object)}) 
#'rec
#'@export rec
setGeneric('rec', function(object, ...) standardGeneric('rec')) 
setMethod('rec', signature('MFCLRep'),function(object) return(slot(object, 'rec'))) 
#'rec
#'@export 
setGeneric('rec<-', function(object, ..., value) standardGeneric('rec<-')) 
setReplaceMethod('rec', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['rec'])),
                                function(object, value){slot(object, 'rec') <- value; return(object)}) 
#'rec_obs
#'@export rec_obs
setGeneric('rec_obs', function(object, ...) standardGeneric('rec_obs')) 
setMethod('rec_obs', signature('MFCLRep'),function(object) return(slot(object, 'rec_obs'))) 
#'rec_obs
#'@export 
setGeneric('rec_obs<-', function(object, ..., value) standardGeneric('rec_obs<-')) 
setReplaceMethod('rec_obs', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['rec_obs'])),
                                function(object, value){slot(object, 'rec_obs') <- value; return(object)}) 
#'dimensions
#'@export dimensions
#setGeneric('dimensions', function(object, ...) standardGeneric('dimensions')) 
setMethod('dimensions', signature('MFCLRep'),function(object) return(slot(object, 'dimensions'))) 
#'dimensions
#'@export 
#setGeneric('dimensions<-', function(object, ..., value) standardGeneric('dimensions<-')) 
setReplaceMethod('dimensions', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['dimensions'])),
                                function(object, value){slot(object, 'dimensions') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLRep'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLprojControl 
#'nyears
#'@export nyears
setGeneric('nyears', function(object, ...) standardGeneric('nyears')) 
setMethod('nyears', signature('MFCLprojControl'),function(object) return(slot(object, 'nyears'))) 
#'nyears
#'@export 
setGeneric('nyears<-', function(object, ..., value) standardGeneric('nyears<-')) 
setReplaceMethod('nyears', signature(object='MFCLprojControl', value=unname(getSlots('MFCLprojControl')['nyears'])),
                                function(object, value){slot(object, 'nyears') <- value; return(object)}) 
#'nsims
#'@export nsims
setGeneric('nsims', function(object, ...) standardGeneric('nsims')) 
setMethod('nsims', signature('MFCLprojControl'),function(object) return(slot(object, 'nsims'))) 
#'nsims
#'@export 
setGeneric('nsims<-', function(object, ..., value) standardGeneric('nsims<-')) 
setReplaceMethod('nsims', signature(object='MFCLprojControl', value=unname(getSlots('MFCLprojControl')['nsims'])),
                                function(object, value){slot(object, 'nsims') <- value; return(object)}) 
#'avyrs
#'@export avyrs
setGeneric('avyrs', function(object, ...) standardGeneric('avyrs')) 
setMethod('avyrs', signature('MFCLprojControl'),function(object) return(slot(object, 'avyrs'))) 
#'avyrs
#'@export 
setGeneric('avyrs<-', function(object, ..., value) standardGeneric('avyrs<-')) 
setReplaceMethod('avyrs', signature(object='MFCLprojControl', value=unname(getSlots('MFCLprojControl')['avyrs'])),
                                function(object, value){slot(object, 'avyrs') <- value; return(object)}) 
#'caeff
#'@export caeff
setGeneric('caeff', function(object, ...) standardGeneric('caeff')) 
setMethod('caeff', signature('MFCLprojControl'),function(object) return(slot(object, 'caeff'))) 
#'caeff
#'@export 
setGeneric('caeff<-', function(object, ..., value) standardGeneric('caeff<-')) 
setReplaceMethod('caeff', signature(object='MFCLprojControl', value=unname(getSlots('MFCLprojControl')['caeff'])),
                                function(object, value){slot(object, 'caeff') <- value; return(object)}) 
#'scaler
#'@export scaler
setGeneric('scaler', function(object, ...) standardGeneric('scaler')) 
setMethod('scaler', signature('MFCLprojControl'),function(object) return(slot(object, 'scaler'))) 
#'scaler
#'@export 
setGeneric('scaler<-', function(object, ..., value) standardGeneric('scaler<-')) 
setReplaceMethod('scaler', signature(object='MFCLprojControl', value=unname(getSlots('MFCLprojControl')['scaler'])),
                                function(object, value){slot(object, 'scaler') <- value; return(object)}) 

#############################################################################################################
# class  MFCLFrq
#'range
#'@export range
setMethod('range', signature(x='MFCLFrq'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLFrq', value=unname(getSlots('MFCLFrq')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLPar
#'range
#'@export range
setMethod('range', signature(x='MFCLPar'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLPar', value=unname(getSlots('MFCLPar')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 

#'dimensions
#'@export dimensions
#setGeneric('dimensions', function(object, ...) standardGeneric('dimensions')) 
setMethod('dimensions', signature('MFCLPar'),function(object) return(slot(object, 'dimensions'))) 
#'dimensions
#'@export 
#setGeneric('dimensions<-', function(object, ..., value) standardGeneric('dimensions<-')) 
setReplaceMethod('dimensions', signature(object='MFCLPar', value=unname(getSlots('MFCLPar')['dimensions'])),
                 function(object, value){slot(object, 'dimensions') <- value; return(object)}) 

#############################################################################################################
# class  MFCLIni
#'range
#'@export range
setMethod('range', signature(x='MFCLIni'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLIni', value=unname(getSlots('MFCLIni')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLRep
#'range
#'@export range
setMethod('range', signature(x='MFCLRep'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 

#'dimensions
#'@export dimensions
#setGeneric('dimensions', function(object, ...) standardGeneric('dimensions')) 
setMethod('dimensions', signature('MFCLRep'),function(object) return(slot(object, 'dimensions'))) 
#'dimensions
#'@export 
#setGeneric('dimensions<-', function(object, ..., value) standardGeneric('dimensions<-')) 
setReplaceMethod('dimensions', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['dimensions'])),
                 function(object, value){slot(object, 'dimensions') <- value; return(object)}) 

#############################################################################################################
# class  MFCLTag
#'release_groups
#'@export release_groups
setGeneric('release_groups', function(object, ...) standardGeneric('release_groups')) 
setMethod('release_groups', signature('MFCLTag'),function(object) return(slot(object, 'release_groups'))) 
#'release_groups
#'@export 
setGeneric('release_groups<-', function(object, ..., value) standardGeneric('release_groups<-')) 
setReplaceMethod('release_groups', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['release_groups'])),
                 function(object, value){slot(object, 'release_groups') <- value; return(object)}) 

#'release_lengths
#'@export release_lengths
setGeneric('release_lengths', function(object, ...) standardGeneric('release_lengths')) 
setMethod('release_lengths', signature('MFCLTag'),function(object) return(slot(object, 'release_lengths'))) 
#'lengths
#'@export 
setGeneric('release_lengths<-', function(object, ..., value) standardGeneric('release_lengths<-')) 
setReplaceMethod('release_lengths', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['release_lengths'])),
                 function(object, value){slot(object, 'release_lengths') <- value; return(object)}) 


#'recoveries
#'@export recoveries
setGeneric('recoveries', function(object, ...) standardGeneric('recoveries')) 
setMethod('recoveries', signature('MFCLTag'),function(object) return(slot(object, 'recoveries'))) 
#'recoveries
#'@export 
setGeneric('recoveries<-', function(object, ..., value) standardGeneric('recoveries<-')) 
setReplaceMethod('recoveries', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['recoveries'])),
                 function(object, value){slot(object, 'recoveries') <- value; return(object)}) 

#'releases
#'@export releases
setGeneric('releases', function(object, ...) standardGeneric('releases')) 
setMethod('releases', signature('MFCLTag'),function(object) return(slot(object, 'releases'))) 
#'releases
#'@export 
setGeneric('releases<-', function(object, ..., value) standardGeneric('releases<-')) 
setReplaceMethod('releases', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['releases'])),
                 function(object, value){slot(object, 'releases') <- value; return(object)}) 

#'recaptures
#'@export recaptures
setGeneric('recaptures', function(object, ...) standardGeneric('recaptures')) 
setMethod('recaptures', signature('MFCLTag'),function(object) return(slot(object, 'recaptures'))) 
#'recaptures
#'@export 
setGeneric('recaptures<-', function(object, ..., value) standardGeneric('recaptures<-')) 
setReplaceMethod('recaptures', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['recaptures'])),
                 function(object, value){slot(object, 'recaptures') <- value; return(object)}) 

#'range
#'@export range
setMethod('range', signature(x='MFCLTag'),function(x) return(slot(x,'range'))) 
#'range
#'@export range<- 
#setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLTag', value=unname(getSlots('MFCLIni')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 



