#FLR4MFCL - R4MFCL built with FLR classes
#Copyright (C) 2018  Rob Scott

# Automatically generated Generic Methods - and then updated by hand
# Generated from 'createGenericMethods' 
# Generated on  Tue Feb  2 11:46:03 2016 

############################################################################################################# 
# class  MFCLBase 
#' Accessor methods
#'
#' A metric butt-tonne of accessor methods.
#'
#' Accessors for extracting and writing data from / to an MFCLxxx object.
#' They are usually named after the slot names of the object class.
#' @param object An object of type 'MFCLxxx', e.g. 'MFCLFrq'
#' @param ... Other arguments
#' @rdname accessor-methods
#' @aliases accessors
setGeneric('dimensions', function(object, ...) standardGeneric('dimensions')) 
#' @rdname accessor-methods
setMethod('dimensions', signature('MFCLBase'),function(object) return(slot(object, 'dimensions'))) 
#' @rdname accessor-methods
setGeneric('dimensions<-', function(object, ..., value) standardGeneric('dimensions<-')) 
#' @rdname accessor-methods
setReplaceMethod('dimensions', signature(object='MFCLBase', value=unname(getSlots('MFCLBase')['dimensions'])),
                                function(object, value){slot(object, 'dimensions') <- value; return(object)}) 

#' @rdname accessor-methods
setMethod('range', signature(x='MFCLBase'),function(x) return(slot(x,'range'))) 

# In FLCore
# setGeneric("range<-", function(x, i, value) standardGeneric("range<-"))
#' @rdname accessor-methods
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 

#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLBase', value=unname(getSlots('MFCLBase')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLFrqStats 
#' @rdname accessor-methods
setGeneric('n_regions', function(object, ...) standardGeneric('n_regions')) 
#' @rdname accessor-methods
setMethod('n_regions', signature('MFCLFrqStats'),function(object) return(slot(object, 'n_regions'))) 
#' @rdname accessor-methods
setGeneric('n_regions<-', function(object, ..., value) standardGeneric('n_regions<-')) 
#' @rdname accessor-methods
setReplaceMethod('n_regions', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['n_regions'])),
                                function(object, value){slot(object, 'n_regions') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('n_fisheries', function(object, ...) standardGeneric('n_fisheries')) 
#' @rdname accessor-methods
setMethod('n_fisheries', signature('MFCLFrqStats'),function(object) return(slot(object, 'n_fisheries'))) 
#' @rdname accessor-methods
setGeneric('n_fisheries<-', function(object, ..., value) standardGeneric('n_fisheries<-')) 
#' @rdname accessor-methods
setReplaceMethod('n_fisheries', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['n_fisheries'])),
                                function(object, value){slot(object, 'n_fisheries') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('n_tag_groups', function(object, ...) standardGeneric('n_tag_groups')) 
#' @rdname accessor-methods
setMethod('n_tag_groups', signature('MFCLFrqStats'),function(object) return(slot(object, 'n_tag_groups'))) 
#' @rdname accessor-methods
setGeneric('n_tag_groups<-', function(object, ..., value) standardGeneric('n_tag_groups<-')) 
#' @rdname accessor-methods
setReplaceMethod('n_tag_groups', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['n_tag_groups'])),
                                function(object, value){slot(object, 'n_tag_groups') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('n_recs_yr', function(object, ...) standardGeneric('n_recs_yr')) 
#' @rdname accessor-methods
setMethod('n_recs_yr', signature('MFCLFrqStats'),function(object) return(slot(object, 'n_recs_yr'))) 
#' @rdname accessor-methods
setGeneric('n_recs_yr<-', function(object, ..., value) standardGeneric('n_recs_yr<-')) 
#' @rdname accessor-methods
setReplaceMethod('n_recs_yr', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['n_recs_yr'])),
                                function(object, value){slot(object, 'n_recs_yr') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('rec_month', function(object, ...) standardGeneric('rec_month')) 
#' @rdname accessor-methods
setMethod('rec_month', signature('MFCLFrqStats'),function(object) return(slot(object, 'rec_month'))) 
#' @rdname accessor-methods
setGeneric('rec_month<-', function(object, ..., value) standardGeneric('rec_month<-')) 
#' @rdname accessor-methods
setReplaceMethod('rec_month', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['rec_month'])),
                                function(object, value){slot(object, 'rec_month') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('generic_diffusion', function(object, ...) standardGeneric('generic_diffusion')) 
#' @rdname accessor-methods
setMethod('generic_diffusion', signature('MFCLFrqStats'),function(object) return(slot(object, 'generic_diffusion'))) 
#' @rdname accessor-methods
setGeneric('generic_diffusion<-', function(object, ..., value) standardGeneric('generic_diffusion<-')) 
#' @rdname accessor-methods
setReplaceMethod('generic_diffusion', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['generic_diffusion'])),
                                function(object, value){slot(object, 'generic_diffusion') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('frq_age_len', function(object, ...) standardGeneric('frq_age_len')) 
#' @rdname accessor-methods
setMethod('frq_age_len', signature('MFCLFrqStats'),function(object) return(slot(object, 'frq_age_len'))) 
#' @rdname accessor-methods
setGeneric('frq_age_len<-', function(object, ..., value) standardGeneric('frq_age_len<-')) 
#' @rdname accessor-methods
setReplaceMethod('frq_age_len', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['frq_age_len'])),
                                function(object, value){slot(object, 'frq_age_len') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('frq_version', function(object, ...) standardGeneric('frq_version')) 
#' @rdname accessor-methods
setMethod('frq_version', signature('MFCLFrqStats'),function(object) return(slot(object, 'frq_version'))) 
#' @rdname accessor-methods
setGeneric('frq_version<-', function(object, ..., value) standardGeneric('frq_version<-')) 
#' @rdname accessor-methods
setReplaceMethod('frq_version', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['frq_version'])),
                                function(object, value){slot(object, 'frq_version') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('region_size', function(object, ...) standardGeneric('region_size')) 
#' @rdname accessor-methods
setMethod('region_size', signature('MFCLFrqStats'),function(object) return(slot(object, 'region_size'))) 
#' @rdname accessor-methods
setGeneric('region_size<-', function(object, ..., value) standardGeneric('region_size<-')) 
#' @rdname accessor-methods
setReplaceMethod('region_size', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['region_size'])),
                                function(object, value){slot(object, 'region_size') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('region_fish', function(object, ...) standardGeneric('region_fish')) 
#' @rdname accessor-methods
setMethod('region_fish', signature('MFCLFrqStats'),function(object) return(slot(object, 'region_fish'))) 
#' @rdname accessor-methods
setGeneric('region_fish<-', function(object, ..., value) standardGeneric('region_fish<-')) 
#' @rdname accessor-methods
setReplaceMethod('region_fish', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['region_fish'])),
                                function(object, value){slot(object, 'region_fish') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('move_matrix', function(object, ...) standardGeneric('move_matrix')) 
#' @rdname accessor-methods
setMethod('move_matrix', signature('MFCLFrqStats'),function(object) return(slot(object, 'move_matrix'))) 
#' @rdname accessor-methods
setGeneric('move_matrix<-', function(object, ..., value) standardGeneric('move_matrix<-')) 
#' @rdname accessor-methods
setReplaceMethod('move_matrix', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['move_matrix'])),
                                function(object, value){slot(object, 'move_matrix') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('data_flags', function(object, ...) standardGeneric('data_flags')) 
#' @rdname accessor-methods
setMethod('data_flags', signature('MFCLFrqStats'),function(object) return(slot(object, 'data_flags'))) 
#' @rdname accessor-methods
setGeneric('data_flags<-', function(object, ..., value) standardGeneric('data_flags<-')) 
#' @rdname accessor-methods
setReplaceMethod('data_flags', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['data_flags'])),
                                function(object, value){slot(object, 'data_flags') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('season_flags', function(object, ...) standardGeneric('season_flags')) 
#' @rdname accessor-methods
setMethod('season_flags', signature('MFCLFrqStats'),function(object) return(slot(object, 'season_flags'))) 
#' @rdname accessor-methods
setGeneric('season_flags<-', function(object, ..., value) standardGeneric('season_flags<-')) 
#' @rdname accessor-methods
setReplaceMethod('season_flags', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['season_flags'])),
                                function(object, value){slot(object, 'season_flags') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('n_move_yr', function(object, ...) standardGeneric('n_move_yr')) 
#' @rdname accessor-methods
setMethod('n_move_yr', signature('MFCLFrqStats'),function(object) return(slot(object, 'n_move_yr'))) 
#' @rdname accessor-methods
setGeneric('n_move_yr<-', function(object, ..., value) standardGeneric('n_move_yr<-')) 
#' @rdname accessor-methods
setReplaceMethod('n_move_yr', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['n_move_yr'])),
                                function(object, value){slot(object, 'n_move_yr') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('move_weeks', function(object, ...) standardGeneric('move_weeks')) 
#' @rdname accessor-methods
setMethod('move_weeks', signature('MFCLFrqStats'),function(object) return(slot(object, 'move_weeks'))) 
#' @rdname accessor-methods
setGeneric('move_weeks<-', function(object, ..., value) standardGeneric('move_weeks<-')) 
#' @rdname accessor-methods
setReplaceMethod('move_weeks', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['move_weeks'])),
                                function(object, value){slot(object, 'move_weeks') <- value; return(object)}) 

#' @rdname accessor-methods
setMethod('range', signature(x='MFCLFrqStats'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLLenFreq 
#' @rdname accessor-methods
setGeneric('lf_range', function(object, ...) standardGeneric('lf_range')) 
#' @rdname accessor-methods
setMethod('lf_range', signature('MFCLLenFreq'),function(object) return(slot(object, 'lf_range'))) 
#' @rdname accessor-methods
setGeneric('lf_range<-', function(object, ..., value) standardGeneric('lf_range<-')) 
#' @rdname accessor-methods
setReplaceMethod('lf_range', signature(object='MFCLLenFreq', value=unname(getSlots('MFCLLenFreq')['lf_range'])),
                                function(object, value){slot(object, 'lf_range') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('age_nage', function(object, ...) standardGeneric('age_nage')) 
#' @rdname accessor-methods
setMethod('age_nage', signature('MFCLLenFreq'),function(object) return(slot(object, 'age_nage'))) 
#' @rdname accessor-methods
setGeneric('age_nage<-', function(object, ..., value) standardGeneric('age_nage<-')) 
#' @rdname accessor-methods
setReplaceMethod('age_nage', signature(object='MFCLLenFreq', value=unname(getSlots('MFCLLenFreq')['age_nage'])),
                                function(object, value){slot(object, 'age_nage') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('freq', function(object, ...) standardGeneric('freq')) 
#' @rdname accessor-methods
setMethod('freq', signature('MFCLLenFreq'),function(object) return(slot(object, 'freq'))) 
#' @rdname accessor-methods
setGeneric('freq<-', function(object, ..., value) standardGeneric('freq<-')) 
#' @rdname accessor-methods
setReplaceMethod('freq', signature(object='MFCLLenFreq', value=unname(getSlots('MFCLLenFreq')['freq'])),
                                function(object, value){slot(object, 'freq') <- value; return(object)}) 

#############################################################################################################
# class  MFCLBiol 
#' @rdname accessor-methods
setMethod('m', signature('MFCLBiol'),function(object) return(slot(object, 'm'))) 
#' @rdname accessor-methods
setReplaceMethod('m', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['m'])),
                                function(object, value){slot(object, 'm') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('m_devs_age', function(object, ...) standardGeneric('m_devs_age')) 
#' @rdname accessor-methods
setMethod('m_devs_age', signature('MFCLBiol'),function(object) return(slot(object, 'm_devs_age'))) 
#' @rdname accessor-methods
setGeneric('m_devs_age<-', function(object, ..., value) standardGeneric('m_devs_age<-')) 
#' @rdname accessor-methods
setReplaceMethod('m_devs_age', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['m_devs_age'])),
                                function(object, value){slot(object, 'm_devs_age') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('log_m', function(object, ...) standardGeneric('log_m')) 
#' @rdname accessor-methods
setMethod('log_m', signature('MFCLBiol'),function(object) return(slot(object, 'log_m'))) 
#' @rdname accessor-methods
setGeneric('log_m<-', function(object, ..., value) standardGeneric('log_m<-')) 
#' @rdname accessor-methods
setReplaceMethod('log_m', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['log_m'])),
                                function(object, value){slot(object, 'log_m') <- value; return(object)}) 
#' @rdname accessor-methods
setMethod('mat', signature('MFCLBiol'),function(object) return(slot(object, 'mat'))) 
#' @rdname accessor-methods
setReplaceMethod('mat', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['mat'])),
                                function(object, value){slot(object, 'mat') <- value; return(object)})
#' @rdname accessor-methods
setGeneric('mat_at_length', function(object, ...) standardGeneric('mat_at_length')) 
#' @rdname accessor-methods
setMethod('mat_at_length', signature('MFCLBiol'),function(object) return(slot(object, 'mat_at_length'))) 
#' @rdname accessor-methods
setGeneric('mat_at_length<-', function(object, ..., value) standardGeneric('mat_at_length<-')) 
#' @rdname accessor-methods
setReplaceMethod('mat_at_length', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['mat_at_length'])),
                 function(object, value){slot(object, 'mat_at_length') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('growth', function(object, ...) standardGeneric('growth')) 
#' @rdname accessor-methods
setMethod('growth', signature('MFCLBiol'),function(object) return(slot(object, 'growth'))) 
#' @rdname accessor-methods
setGeneric('growth<-', function(object, ..., value) standardGeneric('growth<-')) 
#' @rdname accessor-methods
setReplaceMethod('growth', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['growth'])),
                                function(object, value){slot(object, 'growth') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('richards', function(object, ...) standardGeneric('richards')) 
#' @rdname accessor-methods
setMethod('richards', signature('MFCLBiol'),function(object) return(slot(object, 'richards'))) 
#' @rdname accessor-methods
setGeneric('richards<-', function(object, ..., value) standardGeneric('richards<-')) 
#' @rdname accessor-methods
setReplaceMethod('richards', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['richards'])),
                                function(object, value){slot(object, 'richards') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('growth_var_pars', function(object, ...) standardGeneric('growth_var_pars')) 
#' @rdname accessor-methods
setMethod('growth_var_pars', signature('MFCLBiol'),function(object) return(slot(object, 'growth_var_pars'))) 
#' @rdname accessor-methods
setGeneric('growth_var_pars<-', function(object, ..., value) standardGeneric('growth_var_pars<-')) 
#' @rdname accessor-methods
setReplaceMethod('growth_var_pars', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['growth_var_pars'])),
                                function(object, value){slot(object, 'growth_var_pars') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('n_mean_constraints', function(object, ...) standardGeneric('n_mean_constraints')) 
#' @rdname accessor-methods
setMethod('n_mean_constraints', signature('MFCLBiol'),function(object) return(slot(object, 'n_mean_constraints'))) 
#' @rdname accessor-methods
setGeneric('n_mean_constraints<-', function(object, ..., value) standardGeneric('n_mean_constraints<-')) 
#' @rdname accessor-methods
setReplaceMethod('n_mean_constraints', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['n_mean_constraints'])),
                                function(object, value){slot(object, 'n_mean_constraints') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('growth_devs_age', function(object, ...) standardGeneric('growth_devs_age')) 
#' @rdname accessor-methods
setMethod('growth_devs_age', signature('MFCLBiol'),function(object) return(slot(object, 'growth_devs_age'))) 
#' @rdname accessor-methods
setGeneric('growth_devs_age<-', function(object, ..., value) standardGeneric('growth_devs_age<-')) 
#' @rdname accessor-methods
setReplaceMethod('growth_devs_age', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['growth_devs_age'])),
                                function(object, value){slot(object, 'growth_devs_age') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('growth_curve_devs', function(object, ...) standardGeneric('growth_curve_devs')) 
#' @rdname accessor-methods
setMethod('growth_curve_devs', signature('MFCLBiol'),function(object) return(slot(object, 'growth_curve_devs'))) 
#' @rdname accessor-methods
setGeneric('growth_curve_devs<-', function(object, ..., value) standardGeneric('growth_curve_devs<-')) 
#' @rdname accessor-methods
setReplaceMethod('growth_curve_devs', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['growth_curve_devs'])),
                                function(object, value){slot(object, 'growth_curve_devs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('growth_devs_cohort', function(object, ...) standardGeneric('growth_devs_cohort')) 
#' @rdname accessor-methods
setMethod('growth_devs_cohort', signature('MFCLBiol'),function(object) return(slot(object, 'growth_devs_cohort'))) 
#' @rdname accessor-methods
setGeneric('growth_devs_cohort<-', function(object, ..., value) standardGeneric('growth_devs_cohort<-')) 
#' @rdname accessor-methods
setReplaceMethod('growth_devs_cohort', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['growth_devs_cohort'])),
                                function(object, value){slot(object, 'growth_devs_cohort') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('season_growth_pars', function(object, ...) standardGeneric('season_growth_pars')) 
#' @rdname accessor-methods
setMethod('season_growth_pars', signature('MFCLBiol'),function(object) return(slot(object, 'season_growth_pars'))) 
#' @rdname accessor-methods
setGeneric('season_growth_pars<-', function(object, ..., value) standardGeneric('season_growth_pars<-')) 
#' @rdname accessor-methods
setReplaceMethod('season_growth_pars', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['season_growth_pars'])),
                                function(object, value){slot(object, 'season_growth_pars') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('len_bias_pars', function(object, ...) standardGeneric('len_bias_pars')) 
#' @rdname accessor-methods
setMethod('len_bias_pars', signature('MFCLBiol'),function(object) return(slot(object, 'len_bias_pars'))) 
#' @rdname accessor-methods
setGeneric('len_bias_pars<-', function(object, ..., value) standardGeneric('len_bias_pars<-')) 
#' @rdname accessor-methods
setReplaceMethod('len_bias_pars', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['len_bias_pars'])),
                                function(object, value){slot(object, 'len_bias_pars') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('common_len_bias_pars', function(object, ...) standardGeneric('common_len_bias_pars')) 
#' @rdname accessor-methods
setMethod('common_len_bias_pars', signature('MFCLBiol'),function(object) return(slot(object, 'common_len_bias_pars'))) 
#' @rdname accessor-methods
setGeneric('common_len_bias_pars<-', function(object, ..., value) standardGeneric('common_len_bias_pars<-')) 
#' @rdname accessor-methods
setReplaceMethod('common_len_bias_pars', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['common_len_bias_pars'])),
                                function(object, value){slot(object, 'common_len_bias_pars') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('common_len_bias_coffs', function(object, ...) standardGeneric('common_len_bias_coffs')) 
#' @rdname accessor-methods
setMethod('common_len_bias_coffs', signature('MFCLBiol'),function(object) return(slot(object, 'common_len_bias_coffs'))) 
#' @rdname accessor-methods
setGeneric('common_len_bias_coffs<-', function(object, ..., value) standardGeneric('common_len_bias_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('common_len_bias_coffs', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['common_len_bias_coffs'])),
                                function(object, value){slot(object, 'common_len_bias_coffs') <- value; return(object)}) 

#' @rdname accessor-methods
setMethod('dimensions', signature('MFCLBiol'),function(object) return(slot(object, 'dimensions'))) 
#' @rdname accessor-methods
setReplaceMethod('dimensions', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['dimensions'])),
                                function(object, value){slot(object, 'dimensions') <- value; return(object)}) 
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLBiol'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLFlags 
#' @rdname accessor-methods
setGeneric('flags', function(object, ...) standardGeneric('flags')) 
#' @rdname accessor-methods
setMethod('flags', signature('MFCLFlags'),function(object) return(slot(object, 'flags'))) 
#' @rdname accessor-methods
setGeneric('flags<-', function(object, ..., value) standardGeneric('flags<-')) 
#' @rdname accessor-methods
setReplaceMethod('flags', signature(object='MFCLFlags', value=unname(getSlots('MFCLFlags')['flags'])),
                                function(object, value){slot(object, 'flags') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('unused', function(object, ...) standardGeneric('unused')) 
#' @rdname accessor-methods
setMethod('unused', signature('MFCLFlags'),function(object) return(slot(object, 'unused'))) 
#' @rdname accessor-methods
setGeneric('unused<-', function(object, ..., value) standardGeneric('unused<-')) 
#' @rdname accessor-methods
setReplaceMethod('unused', signature(object='MFCLFlags', value=unname(getSlots('MFCLFlags')['unused'])),
                                function(object, value){slot(object, 'unused') <- value; return(object)}) 

#############################################################################################################
# class  MFCLTagRep 
#' @rdname accessor-methods
setGeneric('tag_fish_rep_rate', function(object, ...) standardGeneric('tag_fish_rep_rate')) 
#' @rdname accessor-methods
setMethod('tag_fish_rep_rate', signature('MFCLTagRep'),function(object) return(slot(object, 'tag_fish_rep_rate'))) 
#' @rdname accessor-methods
setGeneric('tag_fish_rep_rate<-', function(object, ..., value) standardGeneric('tag_fish_rep_rate<-')) 
#' @rdname accessor-methods
setReplaceMethod('tag_fish_rep_rate', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['tag_fish_rep_rate'])),
                                function(object, value){slot(object, 'tag_fish_rep_rate') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('tag_fish_rep_grp', function(object, ...) standardGeneric('tag_fish_rep_grp')) 
#' @rdname accessor-methods
setMethod('tag_fish_rep_grp', signature('MFCLTagRep'),function(object) return(slot(object, 'tag_fish_rep_grp'))) 
#' @rdname accessor-methods
setGeneric('tag_fish_rep_grp<-', function(object, ..., value) standardGeneric('tag_fish_rep_grp<-')) 
#' @rdname accessor-methods
setReplaceMethod('tag_fish_rep_grp', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['tag_fish_rep_grp'])),
                                function(object, value){slot(object, 'tag_fish_rep_grp') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('tag_fish_rep_flags', function(object, ...) standardGeneric('tag_fish_rep_flags')) 
#' @rdname accessor-methods
setMethod('tag_fish_rep_flags', signature('MFCLTagRep'),function(object) return(slot(object, 'tag_fish_rep_flags'))) 
#' @rdname accessor-methods
setGeneric('tag_fish_rep_flags<-', function(object, ..., value) standardGeneric('tag_fish_rep_flags<-')) 
#' @rdname accessor-methods
setReplaceMethod('tag_fish_rep_flags', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['tag_fish_rep_flags'])),
                                function(object, value){slot(object, 'tag_fish_rep_flags') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('tag_fish_rep_target', function(object, ...) standardGeneric('tag_fish_rep_target')) 
#' @rdname accessor-methods
setMethod('tag_fish_rep_target', signature('MFCLTagRep'),function(object) return(slot(object, 'tag_fish_rep_target'))) 
#' @rdname accessor-methods
setGeneric('tag_fish_rep_target<-', function(object, ..., value) standardGeneric('tag_fish_rep_target<-')) 
#' @rdname accessor-methods
setReplaceMethod('tag_fish_rep_target', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['tag_fish_rep_target'])),
                                function(object, value){slot(object, 'tag_fish_rep_target') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('tag_fish_rep_pen', function(object, ...) standardGeneric('tag_fish_rep_pen')) 
#' @rdname accessor-methods
setMethod('tag_fish_rep_pen', signature('MFCLTagRep'),function(object) return(slot(object, 'tag_fish_rep_pen'))) 
#' @rdname accessor-methods
setGeneric('tag_fish_rep_pen<-', function(object, ..., value) standardGeneric('tag_fish_rep_pen<-')) 
#' @rdname accessor-methods
setReplaceMethod('tag_fish_rep_pen', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['tag_fish_rep_pen'])),
                                function(object, value){slot(object, 'tag_fish_rep_pen') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('rep_rate_dev_coffs', function(object, ...) standardGeneric('rep_rate_dev_coffs')) 
#' @rdname accessor-methods
setMethod('rep_rate_dev_coffs', signature('MFCLTagRep'),function(object) return(slot(object, 'rep_rate_dev_coffs'))) 
#' @rdname accessor-methods
setGeneric('rep_rate_dev_coffs<-', function(object, ..., value) standardGeneric('rep_rate_dev_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('rep_rate_dev_coffs', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['rep_rate_dev_coffs'])),
                                function(object, value){slot(object, 'rep_rate_dev_coffs') <- value; return(object)}) 
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLTagRep'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['range'])),
                                                 function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLRec 
#' @rdname accessor-methods
setGeneric('rec_init_pop_diff', function(object, ...) standardGeneric('rec_init_pop_diff')) 
#' @rdname accessor-methods
setMethod('rec_init_pop_diff', signature('MFCLRec'),function(object) return(slot(object, 'rec_init_pop_diff'))) 
#' @rdname accessor-methods
setGeneric('rec_init_pop_diff<-', function(object, ..., value) standardGeneric('rec_init_pop_diff<-')) 
#' @rdname accessor-methods
setReplaceMethod('rec_init_pop_diff', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['rec_init_pop_diff'])),
                                function(object, value){slot(object, 'rec_init_pop_diff') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('rec_times', function(object, ...) standardGeneric('rec_times')) 
#' @rdname accessor-methods
setMethod('rec_times', signature('MFCLRec'),function(object) return(slot(object, 'rec_times'))) 
#' @rdname accessor-methods
setGeneric('rec_times<-', function(object, ..., value) standardGeneric('rec_times<-')) 
#' @rdname accessor-methods
setReplaceMethod('rec_times', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['rec_times'])),
                                function(object, value){slot(object, 'rec_times') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('rel_rec', function(object, ...) standardGeneric('rel_rec')) 
#' @rdname accessor-methods
setMethod('rel_rec', signature('MFCLRec'),function(object) return(slot(object, 'rel_rec'))) 
#' @rdname accessor-methods
setGeneric('rel_rec<-', function(object, ..., value) standardGeneric('rel_rec<-')) 
#' @rdname accessor-methods
setReplaceMethod('rel_rec', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['rel_rec'])),
                                function(object, value){slot(object, 'rel_rec') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('tot_pop', function(object, ...) standardGeneric('tot_pop')) 
#' @rdname accessor-methods
setMethod('tot_pop', signature('MFCLRec'),function(object) return(slot(object, 'tot_pop'))) 
#' @rdname accessor-methods
setGeneric('tot_pop<-', function(object, ..., value) standardGeneric('tot_pop<-')) 
#' @rdname accessor-methods
setReplaceMethod('tot_pop', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['tot_pop'])),
                                function(object, value){slot(object, 'tot_pop') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('tot_pop_implicit', function(object, ...) standardGeneric('tot_pop_implicit')) 
#' @rdname accessor-methods
setMethod('tot_pop_implicit', signature('MFCLRec'),function(object) return(slot(object, 'tot_pop_implicit'))) 
#' @rdname accessor-methods
setGeneric('tot_pop_implicit<-', function(object, ..., value) standardGeneric('tot_pop_implicit<-')) 
#' @rdname accessor-methods
setReplaceMethod('tot_pop_implicit', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['tot_pop_implicit'])),
                                function(object, value){slot(object, 'tot_pop_implicit') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('rel_ini_pop', function(object, ...) standardGeneric('rel_ini_pop')) 
#' @rdname accessor-methods
setMethod('rel_ini_pop', signature('MFCLRec'),function(object) return(slot(object, 'rel_ini_pop'))) 
#' @rdname accessor-methods
setGeneric('rel_ini_pop<-', function(object, ..., value) standardGeneric('rel_ini_pop<-')) 
#' @rdname accessor-methods
setReplaceMethod('rel_ini_pop', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['rel_ini_pop'])),
                                function(object, value){slot(object, 'rel_ini_pop') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('orth_coffs', function(object, ...) standardGeneric('orth_coffs')) 
#' @rdname accessor-methods
setMethod('orth_coffs', signature('MFCLRec'),function(object) return(slot(object, 'orth_coffs'))) 
#' @rdname accessor-methods
setGeneric('orth_coffs<-', function(object, ..., value) standardGeneric('orth_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('orth_coffs', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['orth_coffs'])),
                 function(object, value){slot(object, 'orth_coffs') <- value; return(object)}) 

#' @rdname accessor-methods
setMethod('range', signature(x='MFCLRec'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLRegion 
#' @rdname accessor-methods
setGeneric('control_flags', function(object, ...) standardGeneric('control_flags')) 
#' @rdname accessor-methods
setMethod('control_flags', signature('MFCLRegion'),function(object) return(slot(object, 'control_flags'))) 
#' @rdname accessor-methods
setGeneric('control_flags<-', function(object, ..., value) standardGeneric('control_flags<-')) 
#' @rdname accessor-methods
setReplaceMethod('control_flags', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['control_flags'])),
                                function(object, value){slot(object, 'control_flags') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('move_map', function(object, ...) standardGeneric('move_map')) 
#' @rdname accessor-methods
setMethod('move_map', signature('MFCLRegion'),function(object) return(slot(object, 'move_map'))) 
#' @rdname accessor-methods
setGeneric('move_map<-', function(object, ..., value) standardGeneric('move_map<-')) 
#' @rdname accessor-methods
setReplaceMethod('move_map', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['move_map'])),
                                function(object, value){slot(object, 'move_map') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('diff_coffs', function(object, ...) standardGeneric('diff_coffs')) 
#' @rdname accessor-methods
setMethod('diff_coffs', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs'))) 
#' @rdname accessor-methods
setGeneric('diff_coffs<-', function(object, ..., value) standardGeneric('diff_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('diff_coffs', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs'])),
                                function(object, value){slot(object, 'diff_coffs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('diff_coffs_mat', function(object, ...) standardGeneric('diff_coffs_mat')) 
#' @rdname accessor-methods
setMethod('diff_coffs_mat', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_mat'))) 
#' @rdname accessor-methods
setGeneric('diff_coffs_mat<-', function(object, ..., value) standardGeneric('diff_coffs_mat<-')) 
#' @rdname accessor-methods
setReplaceMethod('diff_coffs_mat', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_mat'])),
                                function(object, value){slot(object, 'diff_coffs_mat') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('diff_coffs_age_ssn', function(object, ...) standardGeneric('diff_coffs_age_ssn')) 
#' @rdname accessor-methods
setMethod('diff_coffs_age_ssn', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_age_ssn'))) 
#' @rdname accessor-methods
setGeneric('diff_coffs_age_ssn<-', function(object, ..., value) standardGeneric('diff_coffs_age_ssn<-')) 
#' @rdname accessor-methods
setReplaceMethod('diff_coffs_age_ssn', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_age_ssn'])),
                                function(object, value){slot(object, 'diff_coffs_age_ssn') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('diff_coffs_age_period', function(object, ...) standardGeneric('diff_coffs_age_period')) 
#' @rdname accessor-methods
setMethod('diff_coffs_age_period', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_age_period'))) 
#' @rdname accessor-methods
setGeneric('diff_coffs_age_period<-', function(object, ..., value) standardGeneric('diff_coffs_age_period<-')) 
#' @rdname accessor-methods
setReplaceMethod('diff_coffs_age_period', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_age_period'])),
                                function(object, value){slot(object, 'diff_coffs_age_period') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('diff_coffs_age', function(object, ...) standardGeneric('diff_coffs_age')) 
#' @rdname accessor-methods
setMethod('diff_coffs_age', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_age'))) 
#' @rdname accessor-methods
setGeneric('diff_coffs_age<-', function(object, ..., value) standardGeneric('diff_coffs_age<-')) 
#' @rdname accessor-methods
setReplaceMethod('diff_coffs_age', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_age'])),
                                function(object, value){slot(object, 'diff_coffs_age') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('diff_coffs_nl', function(object, ...) standardGeneric('diff_coffs_nl')) 
#' @rdname accessor-methods
setMethod('diff_coffs_nl', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_nl'))) 
#' @rdname accessor-methods
setGeneric('diff_coffs_nl<-', function(object, ..., value) standardGeneric('diff_coffs_nl<-')) 
#' @rdname accessor-methods
setReplaceMethod('diff_coffs_nl', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_nl'])),
                                function(object, value){slot(object, 'diff_coffs_nl') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('diff_coffs_priors', function(object, ...) standardGeneric('diff_coffs_priors')) 
#' @rdname accessor-methods
setMethod('diff_coffs_priors', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_priors'))) 
#' @rdname accessor-methods
setGeneric('diff_coffs_priors<-', function(object, ..., value) standardGeneric('diff_coffs_priors<-')) 
#' @rdname accessor-methods
setReplaceMethod('diff_coffs_priors', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_priors'])),
                                function(object, value){slot(object, 'diff_coffs_priors') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('diff_coffs_age_priors', function(object, ...) standardGeneric('diff_coffs_age_priors')) 
#' @rdname accessor-methods
setMethod('diff_coffs_age_priors', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_age_priors'))) 
#' @rdname accessor-methods
setGeneric('diff_coffs_age_priors<-', function(object, ..., value) standardGeneric('diff_coffs_age_priors<-')) 
#' @rdname accessor-methods
setReplaceMethod('diff_coffs_age_priors', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_age_priors'])),
                                function(object, value){slot(object, 'diff_coffs_age_priors') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('diff_coffs_nl_priors', function(object, ...) standardGeneric('diff_coffs_nl_priors')) 
#' @rdname accessor-methods
setMethod('diff_coffs_nl_priors', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs_nl_priors'))) 
#' @rdname accessor-methods
setGeneric('diff_coffs_nl_priors<-', function(object, ..., value) standardGeneric('diff_coffs_nl_priors<-')) 
#' @rdname accessor-methods
setReplaceMethod('diff_coffs_nl_priors', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs_nl_priors'])),
                                function(object, value){slot(object, 'diff_coffs_nl_priors') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('region_rec_var', function(object, ...) standardGeneric('region_rec_var')) 
#' @rdname accessor-methods
setMethod('region_rec_var', signature('MFCLRegion'),function(object) return(slot(object, 'region_rec_var'))) 
#' @rdname accessor-methods
setGeneric('region_rec_var<-', function(object, ..., value) standardGeneric('region_rec_var<-')) 
#' @rdname accessor-methods
setReplaceMethod('region_rec_var', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['region_rec_var'])),
                                function(object, value){slot(object, 'region_rec_var') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('region_pars', function(object, ...) standardGeneric('region_pars')) 
#' @rdname accessor-methods
setMethod('region_pars', signature('MFCLRegion'),function(object) return(slot(object, 'region_pars'))) 
#' @rdname accessor-methods
setGeneric('region_pars<-', function(object, ..., value) standardGeneric('region_pars<-')) 
#' @rdname accessor-methods
setReplaceMethod('region_pars', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['region_pars'])),
                                function(object, value){slot(object, 'region_pars') <- value; return(object)}) 
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLRegion'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLSel 
#' @rdname accessor-methods
setGeneric('availability_coffs', function(object, ...) standardGeneric('availability_coffs')) 
#' @rdname accessor-methods
setMethod('availability_coffs', signature('MFCLSel'),function(object) return(slot(object, 'availability_coffs'))) 
#' @rdname accessor-methods
setGeneric('availability_coffs<-', function(object, ..., value) standardGeneric('availability_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('availability_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['availability_coffs'])),
                                function(object, value){slot(object, 'availability_coffs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('fishery_sel', function(object, ...) standardGeneric('fishery_sel')) 
#' @rdname accessor-methods
setMethod('fishery_sel', signature('MFCLSel'),function(object) return(slot(object, 'fishery_sel'))) 
#' @rdname accessor-methods
setGeneric('fishery_sel<-', function(object, ..., value) standardGeneric('fishery_sel<-')) 
#' @rdname accessor-methods
setReplaceMethod('fishery_sel', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['fishery_sel'])),
                                function(object, value){slot(object, 'fishery_sel') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('fishery_sel_age_comp', function(object, ...) standardGeneric('fishery_sel_age_comp')) 
#' @rdname accessor-methods
setMethod('fishery_sel_age_comp', signature('MFCLSel'),function(object) return(slot(object, 'fishery_sel_age_comp'))) 
#' @rdname accessor-methods
setGeneric('fishery_sel_age_comp<-', function(object, ..., value) standardGeneric('fishery_sel_age_comp<-')) 
#' @rdname accessor-methods
setReplaceMethod('fishery_sel_age_comp', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['fishery_sel_age_comp'])),
                                function(object, value){slot(object, 'fishery_sel_age_comp') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('av_q_coffs', function(object, ...) standardGeneric('av_q_coffs')) 
#' @rdname accessor-methods
setMethod('av_q_coffs', signature('MFCLSel'),function(object) return(slot(object, 'av_q_coffs'))) 
#' @rdname accessor-methods
setGeneric('av_q_coffs<-', function(object, ..., value) standardGeneric('av_q_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('av_q_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['av_q_coffs'])),
                                function(object, value){slot(object, 'av_q_coffs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('ini_q_coffs', function(object, ...) standardGeneric('ini_q_coffs')) 
#' @rdname accessor-methods
setMethod('ini_q_coffs', signature('MFCLSel'),function(object) return(slot(object, 'ini_q_coffs'))) 
#' @rdname accessor-methods
setGeneric('ini_q_coffs<-', function(object, ..., value) standardGeneric('ini_q_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('ini_q_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['ini_q_coffs'])),
                                function(object, value){slot(object, 'ini_q_coffs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('q0_miss', function(object, ...) standardGeneric('q0_miss')) 
#' @rdname accessor-methods
setMethod('q0_miss', signature('MFCLSel'),function(object) return(slot(object, 'q0_miss'))) 
#' @rdname accessor-methods
setGeneric('q0_miss<-', function(object, ..., value) standardGeneric('q0_miss<-')) 
#' @rdname accessor-methods
setReplaceMethod('q0_miss', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['q0_miss'])),
                                function(object, value){slot(object, 'q0_miss') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('q_dev_coffs', function(object, ...) standardGeneric('q_dev_coffs')) 
#' @rdname accessor-methods
setMethod('q_dev_coffs', signature('MFCLSel'),function(object) return(slot(object, 'q_dev_coffs'))) 
#' @rdname accessor-methods
setGeneric('q_dev_coffs<-', function(object, ..., value) standardGeneric('q_dev_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('q_dev_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['q_dev_coffs'])),
                                function(object, value){slot(object, 'q_dev_coffs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('effort_dev_coffs', function(object, ...) standardGeneric('effort_dev_coffs')) 
#' @rdname accessor-methods
setMethod('effort_dev_coffs', signature('MFCLSel'),function(object) return(slot(object, 'effort_dev_coffs'))) 
#' @rdname accessor-methods
setGeneric('effort_dev_coffs<-', function(object, ..., value) standardGeneric('effort_dev_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('effort_dev_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['effort_dev_coffs'])),
                                function(object, value){slot(object, 'effort_dev_coffs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('catch_dev_coffs', function(object, ...) standardGeneric('catch_dev_coffs')) 
#' @rdname accessor-methods
setMethod('catch_dev_coffs', signature('MFCLSel'),function(object) return(slot(object, 'catch_dev_coffs'))) 
#' @rdname accessor-methods
setGeneric('catch_dev_coffs<-', function(object, ..., value) standardGeneric('catch_dev_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('catch_dev_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['catch_dev_coffs'])),
                                function(object, value){slot(object, 'catch_dev_coffs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('catch_dev_coffs_flag', function(object, ...) standardGeneric('catch_dev_coffs_flag')) 
#' @rdname accessor-methods
setMethod('catch_dev_coffs_flag', signature('MFCLSel'),function(object) return(slot(object, 'catch_dev_coffs_flag'))) 
#' @rdname accessor-methods
setGeneric('catch_dev_coffs_flag<-', function(object, ..., value) standardGeneric('catch_dev_coffs_flag<-')) 
#' @rdname accessor-methods
setReplaceMethod('catch_dev_coffs_flag', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['catch_dev_coffs_flag'])),
                                function(object, value){slot(object, 'catch_dev_coffs_flag') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('sel_dev_corr', function(object, ...) standardGeneric('sel_dev_corr')) 
#' @rdname accessor-methods
setMethod('sel_dev_corr', signature('MFCLSel'),function(object) return(slot(object, 'sel_dev_corr'))) 
#' @rdname accessor-methods
setGeneric('sel_dev_corr<-', function(object, ..., value) standardGeneric('sel_dev_corr<-')) 
#' @rdname accessor-methods
setReplaceMethod('sel_dev_corr', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['sel_dev_corr'])),
                                function(object, value){slot(object, 'sel_dev_corr') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('sel_dev_coffs', function(object, ...) standardGeneric('sel_dev_coffs')) 
#' @rdname accessor-methods
setMethod('sel_dev_coffs', signature('MFCLSel'),function(object) return(slot(object, 'sel_dev_coffs'))) 
#' @rdname accessor-methods
setGeneric('sel_dev_coffs<-', function(object, ..., value) standardGeneric('sel_dev_coffs<-')) 
#' @rdname accessor-methods
setReplaceMethod('sel_dev_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['sel_dev_coffs'])),
                                function(object, value){slot(object, 'sel_dev_coffs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('sel_dev_coffs2', function(object, ...) standardGeneric('sel_dev_coffs2')) 
#' @rdname accessor-methods
setMethod('sel_dev_coffs2', signature('MFCLSel'),function(object) return(slot(object, 'sel_dev_coffs2'))) 
#' @rdname accessor-methods
setGeneric('sel_dev_coffs2<-', function(object, ..., value) standardGeneric('sel_dev_coffs2<-')) 
#' @rdname accessor-methods
setReplaceMethod('sel_dev_coffs2', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['sel_dev_coffs2'])),
                                function(object, value){slot(object, 'sel_dev_coffs2') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('season_q_pars', function(object, ...) standardGeneric('season_q_pars')) 
#' @rdname accessor-methods
setMethod('season_q_pars', signature('MFCLSel'),function(object) return(slot(object, 'season_q_pars'))) 
#' @rdname accessor-methods
setGeneric('season_q_pars<-', function(object, ..., value) standardGeneric('season_q_pars<-')) 
#' @rdname accessor-methods
setReplaceMethod('season_q_pars', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['season_q_pars'])),
                                function(object, value){slot(object, 'season_q_pars') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('fish_params', function(object, ...) standardGeneric('fish_params')) 
#' @rdname accessor-methods
setMethod('fish_params', signature('MFCLSel'),function(object) return(slot(object, 'fish_params'))) 
#' @rdname accessor-methods
setGeneric('fish_params<-', function(object, ..., value) standardGeneric('fish_params<-')) 
#' @rdname accessor-methods
setReplaceMethod('fish_params', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['fish_params'])),
                                function(object, value){slot(object, 'fish_params') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('spp_params', function(object, ...) standardGeneric('spp_params')) 
#' @rdname accessor-methods
setMethod('spp_params', signature('MFCLSel'),function(object) return(slot(object, 'spp_params'))) 
#' @rdname accessor-methods
setGeneric('spp_params<-', function(object, ..., value) standardGeneric('spp_params<-')) 
#' @rdname accessor-methods
setReplaceMethod('spp_params', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['spp_params'])),
                 function(object, value){slot(object, 'spp_params') <- value; return(object)}) 
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLSel'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLParBits 
#' @rdname accessor-methods
setGeneric('fm_level_devs', function(object, ...) standardGeneric('fm_level_devs')) 
#' @rdname accessor-methods
setMethod('fm_level_devs', signature('MFCLParBits'),function(object) return(slot(object, 'fm_level_devs'))) 
#' @rdname accessor-methods
setGeneric('fm_level_devs<-', function(object, ..., value) standardGeneric('fm_level_devs<-')) 
#' @rdname accessor-methods
setReplaceMethod('fm_level_devs', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['fm_level_devs'])),
                                function(object, value){slot(object, 'fm_level_devs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('obj_fun', function(object, ...) standardGeneric('obj_fun')) 
#' @rdname accessor-methods
setMethod('obj_fun', signature('MFCLParBits'),function(object) return(slot(object, 'obj_fun'))) 
#' @rdname accessor-methods
setGeneric('obj_fun<-', function(object, ..., value) standardGeneric('obj_fun<-')) 
#' @rdname accessor-methods
setReplaceMethod('obj_fun', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['obj_fun'])),
                                function(object, value){slot(object, 'obj_fun') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('n_pars', function(object, ...) standardGeneric('n_pars')) 
#' @rdname accessor-methods
setMethod('n_pars', signature('MFCLParBits'),function(object) return(slot(object, 'n_pars'))) 
#' @rdname accessor-methods
setGeneric('n_pars<-', function(object, ..., value) standardGeneric('n_pars<-')) 
#' @rdname accessor-methods
setReplaceMethod('n_pars', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['n_pars'])),
                                function(object, value){slot(object, 'n_pars') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('tag_lik', function(object, ...) standardGeneric('tag_lik')) 
#' @rdname accessor-methods
setMethod('tag_lik', signature('MFCLParBits'),function(object) return(slot(object, 'tag_lik'))) 
#' @rdname accessor-methods
setGeneric('tag_lik<-', function(object, ..., value) standardGeneric('tag_lik<-')) 
#' @rdname accessor-methods
setReplaceMethod('tag_lik', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['tag_lik'])),
                                function(object, value){slot(object, 'tag_lik') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('mn_len_pen', function(object, ...) standardGeneric('mn_len_pen')) 
#' @rdname accessor-methods
setMethod('mn_len_pen', signature('MFCLParBits'),function(object) return(slot(object, 'mn_len_pen'))) 
#' @rdname accessor-methods
setGeneric('mn_len_pen<-', function(object, ..., value) standardGeneric('mn_len_pen<-')) 
#' @rdname accessor-methods
setReplaceMethod('mn_len_pen', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['mn_len_pen'])),
                                function(object, value){slot(object, 'mn_len_pen') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('max_grad', function(object, ...) standardGeneric('max_grad')) 
#' @rdname accessor-methods
setMethod('max_grad', signature('MFCLParBits'),function(object) return(slot(object, 'max_grad'))) 
#' @rdname accessor-methods
setGeneric('max_grad<-', function(object, ..., value) standardGeneric('max_grad<-')) 
#' @rdname accessor-methods
setReplaceMethod('max_grad', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['max_grad'])),
                                function(object, value){slot(object, 'max_grad') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('av_fish_mort_inst', function(object, ...) standardGeneric('av_fish_mort_inst')) 
#' @rdname accessor-methods
setMethod('av_fish_mort_inst', signature('MFCLParBits'),function(object) return(slot(object, 'av_fish_mort_inst'))) 
#' @rdname accessor-methods
setGeneric('av_fish_mort_inst<-', function(object, ..., value) standardGeneric('av_fish_mort_inst<-')) 
#' @rdname accessor-methods
setReplaceMethod('av_fish_mort_inst', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['av_fish_mort_inst'])),
                                function(object, value){slot(object, 'av_fish_mort_inst') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('av_fish_mort_year', function(object, ...) standardGeneric('av_fish_mort_year')) 
#' @rdname accessor-methods
setMethod('av_fish_mort_year', signature('MFCLParBits'),function(object) return(slot(object, 'av_fish_mort_year'))) 
#' @rdname accessor-methods
setGeneric('av_fish_mort_year<-', function(object, ..., value) standardGeneric('av_fish_mort_year<-')) 
#' @rdname accessor-methods
setReplaceMethod('av_fish_mort_year', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['av_fish_mort_year'])),
                                function(object, value){slot(object, 'av_fish_mort_year') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('av_fish_mort_age', function(object, ...) standardGeneric('av_fish_mort_age')) 
#' @rdname accessor-methods
setMethod('av_fish_mort_age', signature('MFCLParBits'),function(object) return(slot(object, 'av_fish_mort_age'))) 
#' @rdname accessor-methods
setGeneric('av_fish_mort_age<-', function(object, ..., value) standardGeneric('av_fish_mort_age<-')) 
#' @rdname accessor-methods
setReplaceMethod('av_fish_mort_age', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['av_fish_mort_age'])),
                                function(object, value){slot(object, 'av_fish_mort_age') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('logistic_normal_params', function(object, ...) standardGeneric('logistic_normal_params')) 
#' @rdname accessor-methods
setMethod('logistic_normal_params', signature('MFCLParBits'),function(object) return(slot(object, 'logistic_normal_params'))) 
#' @rdname accessor-methods
setGeneric('logistic_normal_params<-', function(object, ..., value) standardGeneric('logistic_normal_params<-')) 
#' @rdname accessor-methods
setReplaceMethod('logistic_normal_params', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['logistic_normal_params'])),
                                function(object, value){slot(object, 'logistic_normal_params') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('lagrangian', function(object, ...) standardGeneric('lagrangian')) 
#' @rdname accessor-methods
setMethod('lagrangian', signature('MFCLParBits'),function(object) return(slot(object, 'lagrangian'))) 
#' @rdname accessor-methods
setGeneric('lagrangian<-', function(object, ..., value) standardGeneric('lagrangian<-')) 
#' @rdname accessor-methods
setReplaceMethod('lagrangian', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['lagrangian'])),
                                function(object, value){slot(object, 'lagrangian') <- value; return(object)}) 
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLParBits'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLParBits', value=unname(getSlots('MFCLParBits')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLIniBits 
#' @rdname accessor-methods
setGeneric('age_pars', function(object, ...) standardGeneric('age_pars')) 
#' @rdname accessor-methods
setMethod('age_pars', signature('MFCLIniBits'),function(object) return(slot(object, 'age_pars'))) 
#' @rdname accessor-methods
setGeneric('age_pars<-', function(object, ..., value) standardGeneric('age_pars<-')) 
#' @rdname accessor-methods
setReplaceMethod('age_pars', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['age_pars'])),
                                function(object, value){slot(object, 'age_pars') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('rec_dist', function(object, ...) standardGeneric('rec_dist')) 
#' @rdname accessor-methods
setMethod('rec_dist', signature('MFCLIniBits'),function(object) return(slot(object, 'rec_dist'))) 
#' @rdname accessor-methods
setGeneric('rec_dist<-', function(object, ..., value) standardGeneric('rec_dist<-')) 
#' @rdname accessor-methods
setReplaceMethod('rec_dist', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['rec_dist'])),
                                function(object, value){slot(object, 'rec_dist') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('lw_params', function(object, ...) standardGeneric('lw_params')) 
#' @rdname accessor-methods
setMethod('lw_params', signature('MFCLIniBits'),function(object) return(slot(object, 'lw_params'))) 
#' @rdname accessor-methods
setGeneric('lw_params<-', function(object, ..., value) standardGeneric('lw_params<-')) 
#' @rdname accessor-methods
setReplaceMethod('lw_params', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['lw_params'])),
                                function(object, value){slot(object, 'lw_params') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('sv', function(object, ...) standardGeneric('sv')) 
#' @rdname accessor-methods
setMethod('sv', signature('MFCLIniBits'),function(object) return(slot(object, 'sv'))) 
#' @rdname accessor-methods
setGeneric('sv<-', function(object, ..., value) standardGeneric('sv<-')) 
#' @rdname accessor-methods
setReplaceMethod('sv', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['sv'])),
                                function(object, value){slot(object, 'sv') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('sd_length_at_age', function(object, ...) standardGeneric('sd_length_at_age')) 
#' @rdname accessor-methods
setMethod('sd_length_at_age', signature('MFCLIniBits'),function(object) return(slot(object, 'sd_length_at_age'))) 
#' @rdname accessor-methods
setGeneric('sd_length_at_age<-', function(object, ..., value) standardGeneric('sd_length_at_age<-')) 
#' @rdname accessor-methods
setReplaceMethod('sd_length_at_age', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['sd_length_at_age'])),
                                function(object, value){slot(object, 'sd_length_at_age') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('sd_length_dep', function(object, ...) standardGeneric('sd_length_dep')) 
#' @rdname accessor-methods
setMethod('sd_length_dep', signature('MFCLIniBits'),function(object) return(slot(object, 'sd_length_dep'))) 
#' @rdname accessor-methods
setGeneric('sd_length_dep<-', function(object, ..., value) standardGeneric('sd_length_dep<-')) 
#' @rdname accessor-methods
setReplaceMethod('sd_length_dep', signature(object='MFCLIniBits', value=unname(getSlots('MFCLIniBits')['sd_length_dep'])),
                                function(object, value){slot(object, 'sd_length_dep') <- value; return(object)}) 

#############################################################################################################
# class  MFCLRep 
#' @rdname accessor-methods
setGeneric('fishery_realizations', function(object, ...) standardGeneric('fishery_realizations')) 
#' @rdname accessor-methods
setMethod('fishery_realizations', signature('MFCLRep'),function(object) return(slot(object, 'fishery_realizations'))) 
#' @rdname accessor-methods
setGeneric('fishery_realizations<-', function(object, ..., value) standardGeneric('fishery_realizations<-')) 
#' @rdname accessor-methods
setReplaceMethod('fishery_realizations', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['fishery_realizations'])),
                                function(object, value){slot(object, 'fishery_realizations') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('mean_laa', function(object, ...) standardGeneric('mean_laa')) 
#' @rdname accessor-methods
setMethod('mean_laa', signature('MFCLRep'),function(object) return(slot(object, 'mean_laa'))) 
#' @rdname accessor-methods
setGeneric('mean_laa<-', function(object, ..., value) standardGeneric('mean_laa<-')) 
#' @rdname accessor-methods
setReplaceMethod('mean_laa', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['mean_laa'])),
                                function(object, value){slot(object, 'mean_laa') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('sd_laa', function(object, ...) standardGeneric('sd_laa')) 
#' @rdname accessor-methods
setMethod('sd_laa', signature('MFCLRep'),function(object) return(slot(object, 'sd_laa'))) 
#' @rdname accessor-methods
setGeneric('sd_laa<-', function(object, ..., value) standardGeneric('sd_laa<-')) 
#' @rdname accessor-methods
setReplaceMethod('sd_laa', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['sd_laa'])),
                                function(object, value){slot(object, 'sd_laa') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('m_at_age', function(object, ...) standardGeneric('m_at_age')) 
#' @rdname accessor-methods
setMethod('m_at_age', signature('MFCLRep'),function(object) return(slot(object, 'm_at_age'))) 
#' @rdname accessor-methods
setGeneric('m_at_age<-', function(object, ..., value) standardGeneric('m_at_age<-')) 
#' @rdname accessor-methods
setReplaceMethod('m_at_age', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['m_at_age'])),
                                function(object, value){slot(object, 'm_at_age') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('sel', function(object, ...) standardGeneric('sel')) 
#' @rdname accessor-methods
setMethod('sel', signature('MFCLRep'),function(object) return(slot(object, 'sel'))) 
#' @rdname accessor-methods
setGeneric('sel<-', function(object, ..., value) standardGeneric('sel<-')) 
#' @rdname accessor-methods
setReplaceMethod('sel', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['sel'])),
                                function(object, value){slot(object, 'sel') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('q_fishery', function(object, ...) standardGeneric('q_fishery')) 
#' @rdname accessor-methods
setMethod('q_fishery', signature('MFCLRep'),function(object) return(slot(object, 'q_fishery'))) 
#' @rdname accessor-methods
setGeneric('q_fishery<-', function(object, ..., value) standardGeneric('q_fishery<-')) 
#' @rdname accessor-methods
setReplaceMethod('q_fishery', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['q_fishery'])),
                                function(object, value){slot(object, 'q_fishery') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('q_effdev', function(object, ...) standardGeneric('q_effdev')) 
#' @rdname accessor-methods
setMethod('q_effdev', signature('MFCLRep'),function(object) return(slot(object, 'q_effdev'))) 
#' @rdname accessor-methods
setGeneric('q_effdev<-', function(object, ..., value) standardGeneric('q_effdev<-')) 
#' @rdname accessor-methods
setReplaceMethod('q_effdev', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['q_effdev'])),
                                function(object, value){slot(object, 'q_effdev') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('fm', function(object, ...) standardGeneric('fm')) 
#' @rdname accessor-methods
setMethod('fm', signature('MFCLRep'),function(object) return(slot(object, 'fm'))) 
#' @rdname accessor-methods
setGeneric('fm<-', function(object, ..., value) standardGeneric('fm<-')) 
#' @rdname accessor-methods
setReplaceMethod('fm', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['fm'])),
                                function(object, value){slot(object, 'fm') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('popN', function(object, ...) standardGeneric('popN')) 
#' @rdname accessor-methods
setMethod('popN', signature('MFCLRep'),function(object) return(slot(object, 'popN'))) 
#' @rdname accessor-methods
setGeneric('popN<-', function(object, ..., value) standardGeneric('popN<-')) 
#' @rdname accessor-methods
setReplaceMethod('popN', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['popN'])),
                                function(object, value){slot(object, 'popN') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('rec_region', function(object, ...) standardGeneric('rec_region')) 
#' @rdname accessor-methods
setMethod('rec_region', signature('MFCLRep'),function(object) return(slot(object, 'rec_region'))) 
#' @rdname accessor-methods
setGeneric('rec_region<-', function(object, ..., value) standardGeneric('rec_region<-')) 
#' @rdname accessor-methods
setReplaceMethod('rec_region', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['rec_region'])),
                                function(object, value){slot(object, 'rec_region') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('totalBiomass', function(object, ...) standardGeneric('totalBiomass')) 
#' @rdname accessor-methods
setMethod('totalBiomass', signature('MFCLRep'),function(object) return(slot(object, 'totalBiomass'))) 
#' @rdname accessor-methods
setGeneric('totalBiomass<-', function(object, ..., value) standardGeneric('totalBiomass<-')) 
#' @rdname accessor-methods
setReplaceMethod('totalBiomass', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['totalBiomass'])),
                                                function(object, value){slot(object, 'totalBiomass') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('totalBiomass_nofish', function(object, ...) standardGeneric('totalBiomass_nofish')) 
#' @rdname accessor-methods
setMethod('totalBiomass_nofish', signature('MFCLRep'),function(object) return(slot(object, 'totalBiomass_nofish'))) 
#' @rdname accessor-methods
setGeneric('totalBiomass_nofish<-', function(object, ..., value) standardGeneric('totalBiomass_nofish<-')) 
#' @rdname accessor-methods
setReplaceMethod('totalBiomass_nofish', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['totalBiomass_nofish'])),
                 function(object, value){slot(object, 'totalBiomass_nofish') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('adultBiomass', function(object, ...) standardGeneric('adultBiomass')) 
#' @rdname accessor-methods
setMethod('adultBiomass', signature('MFCLRep'),function(object) return(slot(object, 'adultBiomass'))) 
#' @rdname accessor-methods
setGeneric('adultBiomass<-', function(object, ..., value) standardGeneric('adultBiomass<-')) 
#' @rdname accessor-methods
setReplaceMethod('adultBiomass', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['adultBiomass'])),
                                function(object, value){slot(object, 'adultBiomass') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('adultBiomass_nofish', function(object, ...) standardGeneric('adultBiomass_nofish')) 
#' @rdname accessor-methods
setMethod('adultBiomass_nofish', signature('MFCLRep'),function(object) return(slot(object, 'adultBiomass_nofish'))) 
#' @rdname accessor-methods
setGeneric('adultBiomass_nofish<-', function(object, ..., value) standardGeneric('adultBiomass_nofish<-')) 
#' @rdname accessor-methods
setReplaceMethod('adultBiomass_nofish', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['adultBiomass_nofish'])),
                                function(object, value){slot(object, 'adultBiomass_nofish') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('vulnBiomass', function(object, ...) standardGeneric('vulnBiomass')) 
#' @rdname accessor-methods
setMethod('vulnBiomass', signature('MFCLRep'),function(object) return(slot(object, 'vulnBiomass'))) 
#' @rdname accessor-methods
setGeneric('vulnBiomass<-', function(object, ..., value) standardGeneric('vulnBiomass<-')) 
#' @rdname accessor-methods
setReplaceMethod('vulnBiomass', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['vulnBiomass'])),
                                function(object, value){slot(object, 'vulnBiomass') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('srr', function(object, ...) standardGeneric('srr')) 
#' @rdname accessor-methods
setMethod('srr', signature('MFCLRep'),function(object) return(slot(object, 'srr'))) 
#' @rdname accessor-methods
setGeneric('srr<-', function(object, ..., value) standardGeneric('srr<-')) 
#' @rdname accessor-methods
setReplaceMethod('srr', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['srr'])),
                                function(object, value){slot(object, 'srr') <- value; return(object)}) 
#' @rdname accessor-methods
setMethod('ssb', signature('MFCLRep'),function(object) return(slot(object, 'ssb'))) 
#' @rdname accessor-methods
setReplaceMethod('ssb', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['ssb'])),
                                function(object, value){slot(object, 'ssb') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('ssb_obs', function(object, ...) standardGeneric('ssb_obs')) 
#' @rdname accessor-methods
setMethod('ssb_obs', signature('MFCLRep'),function(object) return(slot(object, 'ssb_obs'))) 
#' @rdname accessor-methods
setGeneric('ssb_obs<-', function(object, ..., value) standardGeneric('ssb_obs<-')) 
#' @rdname accessor-methods
setReplaceMethod('ssb_obs', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['ssb_obs'])),
                                function(object, value){slot(object, 'ssb_obs') <- value; return(object)}) 
#' @rdname accessor-methods
setMethod('rec', signature('MFCLRep'),function(object) return(slot(object, 'rec'))) 
#' @rdname accessor-methods
setReplaceMethod('rec', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['rec'])),
                                function(object, value){slot(object, 'rec') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('rec_obs', function(object, ...) standardGeneric('rec_obs')) 
#' @rdname accessor-methods
setMethod('rec_obs', signature('MFCLRep'),function(object) return(slot(object, 'rec_obs'))) 
#' @rdname accessor-methods
setGeneric('rec_obs<-', function(object, ..., value) standardGeneric('rec_obs<-')) 
#' @rdname accessor-methods
setReplaceMethod('rec_obs', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['rec_obs'])),
                                function(object, value){slot(object, 'rec_obs') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('catch_obs', function(object, ...) standardGeneric('catch_obs')) 
#' @rdname accessor-methods
setMethod('catch_obs', signature('MFCLRep'),function(object) return(slot(object, 'catch_obs'))) 
#' @rdname accessor-methods
setGeneric('catch_obs<-', function(object, ..., value) standardGeneric('catch_obs<-')) 
#' @rdname accessor-methods
setReplaceMethod('catch_obs', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['catch_obs'])),
                 function(object, value){slot(object, 'catch_obs') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('catch_pred', function(object, ...) standardGeneric('catch_pred')) 
#' @rdname accessor-methods
setMethod('catch_pred', signature('MFCLRep'),function(object) return(slot(object, 'catch_pred'))) 
#' @rdname accessor-methods
setGeneric('catch_pred<-', function(object, ..., value) standardGeneric('catch_pred<-')) 
#' @rdname accessor-methods
setReplaceMethod('catch_pred', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['catch_pred'])),
                 function(object, value){slot(object, 'catch_pred') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('cpue_obs', function(object, ...) standardGeneric('cpue_obs')) 
#' @rdname accessor-methods
setMethod('cpue_obs', signature('MFCLRep'),function(object) return(slot(object, 'cpue_obs'))) 
#' @rdname accessor-methods
setGeneric('cpue_obs<-', function(object, ..., value) standardGeneric('cpue_obs<-')) 
#' @rdname accessor-methods
setReplaceMethod('cpue_obs', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['cpue_obs'])),
                 function(object, value){slot(object, 'cpue_obs') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('cpue_pred', function(object, ...) standardGeneric('cpue_pred')) 
#' @rdname accessor-methods
setMethod('cpue_pred', signature('MFCLRep'),function(object) return(slot(object, 'cpue_pred'))) 
#' @rdname accessor-methods
setGeneric('cpue_pred<-', function(object, ..., value) standardGeneric('cpue_pred<-')) 
#' @rdname accessor-methods
setReplaceMethod('cpue_pred', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['cpue_pred'])),
                 function(object, value){slot(object, 'cpue_pred') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('eq_biomass', function(object, ...) standardGeneric('eq_biomass')) 
#' @rdname accessor-methods
setMethod('eq_biomass', signature('MFCLRep'),function(object) return(slot(object, 'eq_biomass'))) 
#' @rdname accessor-methods
setGeneric('eq_biomass<-', function(object, ..., value) standardGeneric('eq_biomass<-')) 
#' @rdname accessor-methods
setReplaceMethod('eq_biomass', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['eq_biomass'])),
                 function(object, value){slot(object, 'eq_biomass') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('eq_yield', function(object, ...) standardGeneric('eq_yield')) 
#' @rdname accessor-methods
setMethod('eq_yield', signature('MFCLRep'),function(object) return(slot(object, 'eq_yield'))) 
#' @rdname accessor-methods
setGeneric('eq_yield<-', function(object, ..., value) standardGeneric('eq_yield<-')) 
#' @rdname accessor-methods
setReplaceMethod('eq_yield', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['eq_yield'])),
                 function(object, value){slot(object, 'eq_yield') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('MSY', function(object, ...) standardGeneric('MSY')) 
#' @rdname accessor-methods
setMethod('MSY', signature('MFCLRep'),function(object) return(slot(object, 'MSY'))) 
#' @rdname accessor-methods
setGeneric('MSY<-', function(object, ..., value) standardGeneric('MSY<-')) 
#' @rdname accessor-methods
setReplaceMethod('MSY', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['MSY'])),
                 function(object, value){slot(object, 'MSY') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('FMSY', function(object, ...) standardGeneric('FMSY')) 
#' @rdname accessor-methods
setMethod('FMSY', signature('MFCLRep'),function(object) return(slot(object, 'FMSY'))) 
#' @rdname accessor-methods
setGeneric('FMSY<-', function(object, ..., value) standardGeneric('FMSY<-')) 
#' @rdname accessor-methods
setReplaceMethod('FMSY', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['FMSY'])),
                 function(object, value){slot(object, 'FMSY') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('BMSY', function(object, ...) standardGeneric('BMSY')) 
#' @rdname accessor-methods
setMethod('BMSY', signature('MFCLRep'),function(object) return(slot(object, 'BMSY'))) 
#' @rdname accessor-methods
setGeneric('BMSY<-', function(object, ..., value) standardGeneric('BMSY<-')) 
#' @rdname accessor-methods
setReplaceMethod('BMSY', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['BMSY'])),
                 function(object, value){slot(object, 'BMSY') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('FFMSY', function(object, ...) standardGeneric('FFMSY')) 
#' @rdname accessor-methods
setMethod('FFMSY', signature('MFCLRep'),function(object) return(slot(object, 'FFMSY'))) 
#' @rdname accessor-methods
setGeneric('FFMSY<-', function(object, ..., value) standardGeneric('FFMSY<-')) 
#' @rdname accessor-methods
setReplaceMethod('FFMSY', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['FFMSY'])),
                 function(object, value){slot(object, 'FFMSY') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('BBMSY', function(object, ...) standardGeneric('BBMSY')) 
#' @rdname accessor-methods
setMethod('BBMSY', signature('MFCLRep'),function(object) return(slot(object, 'BBMSY'))) 
#' @rdname accessor-methods
setGeneric('BBMSY<-', function(object, ..., value) standardGeneric('BBMSY<-')) 
#' @rdname accessor-methods
setReplaceMethod('BBMSY', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['BBMSY'])),
                 function(object, value){slot(object, 'BBMSY') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('Fmult', function(object, ...) standardGeneric('Fmult')) 
#' @rdname accessor-methods
setMethod('Fmult', signature('MFCLRep'),function(object) return(slot(object, 'Fmult'))) 
#' @rdname accessor-methods
setGeneric('Fmult<-', function(object, ..., value) standardGeneric('Fmult<-')) 
#' @rdname accessor-methods
setReplaceMethod('Fmult', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['Fmult'])),
                 function(object, value){slot(object, 'Fmult') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('AggregateF', function(object, ...) standardGeneric('AggregateF')) 
#' @rdname accessor-methods
setMethod('AggregateF', signature('MFCLRep'),function(object) return(slot(object, 'AggregateF'))) 
#' @rdname accessor-methods
setGeneric('AggregateF<-', function(object, ..., value) standardGeneric('AggregateF<-')) 
#' @rdname accessor-methods
setReplaceMethod('AggregateF', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['AggregateF'])),
                 function(object, value){slot(object, 'AggregateF') <- value; return(object)}) 

#' @rdname accessor-methods
setMethod('dimensions', signature('MFCLRep'),function(object) return(slot(object, 'dimensions'))) 
#' @rdname accessor-methods
setReplaceMethod('dimensions', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['dimensions'])),
                                function(object, value){slot(object, 'dimensions') <- value; return(object)}) 
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLRep'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLprojControl 
#' @rdname accessor-methods
setGeneric('nyears', function(object, ...) standardGeneric('nyears')) 
#' @rdname accessor-methods
setMethod('nyears', signature('MFCLprojControl'),function(object) return(slot(object, 'nyears'))) 
#' @rdname accessor-methods
setGeneric('nyears<-', function(object, ..., value) standardGeneric('nyears<-')) 
#' @rdname accessor-methods
setReplaceMethod('nyears', signature(object='MFCLprojControl', value=unname(getSlots('MFCLprojControl')['nyears'])),
                                function(object, value){slot(object, 'nyears') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('nsims', function(object, ...) standardGeneric('nsims')) 
#' @rdname accessor-methods
setMethod('nsims', signature('MFCLprojControl'),function(object) return(slot(object, 'nsims'))) 
#' @rdname accessor-methods
setGeneric('nsims<-', function(object, ..., value) standardGeneric('nsims<-')) 
#' @rdname accessor-methods
setReplaceMethod('nsims', signature(object='MFCLprojControl', value=unname(getSlots('MFCLprojControl')['nsims'])),
                                function(object, value){slot(object, 'nsims') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('avyrs', function(object, ...) standardGeneric('avyrs')) 
#' @rdname accessor-methods
setMethod('avyrs', signature('MFCLprojControl'),function(object) return(slot(object, 'avyrs'))) 
#' @rdname accessor-methods
setGeneric('avyrs<-', function(object, ..., value) standardGeneric('avyrs<-')) 
#' @rdname accessor-methods
setReplaceMethod('avyrs', signature(object='MFCLprojControl', value=unname(getSlots('MFCLprojControl')['avyrs'])),
                                function(object, value){slot(object, 'avyrs') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('fprojyr', function(object, ...) standardGeneric('fprojyr')) 
#' @rdname accessor-methods
setMethod('fprojyr', signature('MFCLprojControl'),function(object) return(slot(object, 'fprojyr'))) 
#' @rdname accessor-methods
setGeneric('fprojyr<-', function(object, ..., value) standardGeneric('fprojyr<-')) 
#' @rdname accessor-methods
setReplaceMethod('fprojyr', signature(object='MFCLprojControl', value=unname(getSlots('MFCLprojControl')['fprojyr'])),
                 function(object, value){slot(object, 'fprojyr') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('controls', function(object, ...) standardGeneric('controls')) 
#' @rdname accessor-methods
setMethod('controls', signature('MFCLprojControl'),function(object) return(slot(object, 'controls'))) 
#' @rdname accessor-methods
setGeneric('controls<-', function(object, ..., value) standardGeneric('controls<-')) 
#' @rdname accessor-methods
setReplaceMethod('controls', signature(object='MFCLprojControl', value=unname(getSlots('MFCLprojControl')['controls'])),
                 function(object, value){slot(object, 'controls') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('caeff', function(object, ...) standardGeneric('caeff')) 
#' @rdname accessor-methods
setMethod('caeff', signature('MFCLprojControl'),function(object) return(slot(object, 'controls')$caeff)) 
#' @rdname accessor-methods
setGeneric('caeff<-', function(object, ..., value) standardGeneric('caeff<-')) 
#' @rdname accessor-methods
setReplaceMethod('caeff', signature(object='MFCLprojControl', value="numeric"), #unname(getSlots('MFCLprojControl')['controls'])),
                                function(object, value){slot(object, 'controls')$caeff <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('scaler', function(object, ...) standardGeneric('scaler')) 
#' @rdname accessor-methods
setMethod('scaler', signature('MFCLprojControl'),function(object) return(slot(object, 'controls')$scaler)) 
#' @rdname accessor-methods
setGeneric('scaler<-', function(object, ..., value) standardGeneric('scaler<-')) 
#' @rdname accessor-methods
setReplaceMethod('scaler', signature(object='MFCLprojControl', value="numeric"), #unname(getSlots('MFCLprojControl')['scaler'])),
                                function(object, value){slot(object, 'controls')$scaler <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('ess', function(object, ...) standardGeneric('ess')) 
#' @rdname accessor-methods
setMethod('ess', signature('MFCLprojControl'),function(object) return(slot(object, 'controls')$ess)) 
#' @rdname accessor-methods
setGeneric('ess<-', function(object, ..., value) standardGeneric('ess<-')) 
#' @rdname accessor-methods
setReplaceMethod('ess', signature(object='MFCLprojControl', value="numeric"), #unname(getSlots('MFCLprojControl')['scaler'])),
                 function(object, value){slot(object, 'controls')$ess <- value; return(object)}) 


#############################################################################################################
# class  MFCLFrq
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLFrq'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLFrq', value=unname(getSlots('MFCLFrq')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLPar
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLPar'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLPar', value=unname(getSlots('MFCLPar')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 

#' @rdname accessor-methods
setMethod('dimensions', signature('MFCLPar'),function(object) return(slot(object, 'dimensions'))) 
#' @rdname accessor-methods
setReplaceMethod('dimensions', signature(object='MFCLPar', value=unname(getSlots('MFCLPar')['dimensions'])),
                 function(object, value){slot(object, 'dimensions') <- value; return(object)}) 

#############################################################################################################
# class  MFCLIni
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLIni'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLIni', value=unname(getSlots('MFCLIni')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLRep
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLRep'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 

#' @rdname accessor-methods
setMethod('dimensions', signature('MFCLRep'),function(object) return(slot(object, 'dimensions'))) 
#' @rdname accessor-methods
setReplaceMethod('dimensions', signature(object='MFCLRep', value=unname(getSlots('MFCLRep')['dimensions'])),
                 function(object, value){slot(object, 'dimensions') <- value; return(object)}) 

#############################################################################################################
# class  MFCLTagProj
#' @rdname accessor-methods
setGeneric('release_groups_proj', function(object, ...) standardGeneric('release_groups_proj')) 
#' @rdname accessor-methods
setMethod('release_groups_proj', signature('MFCLTagProj'),function(object) return(slot(object, 'release_groups_proj'))) 
#' @rdname accessor-methods
setGeneric('release_groups_proj<-', function(object, ..., value) standardGeneric('release_groups_proj<-')) 
#' @rdname accessor-methods
setReplaceMethod('release_groups_proj', signature(object='MFCLTagProj', value=unname(getSlots('MFCLTagProj')['release_groups_proj'])),
                 function(object, value){slot(object, 'release_groups_proj') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('releases_proj', function(object, ...) standardGeneric('releases_proj')) 
#' @rdname accessor-methods
setMethod('releases_proj', signature('MFCLTagProj'),function(object) return(slot(object, 'releases_proj'))) 
#' @rdname accessor-methods
setGeneric('releases_proj<-', function(object, ..., value) standardGeneric('releases_proj<-')) 
#' @rdname accessor-methods
setReplaceMethod('releases_proj', signature(object='MFCLTagProj', value=unname(getSlots('MFCLTagProj')['releases_proj'])),
                 function(object, value){slot(object, 'releases_proj') <- value; return(object)}) 
#' @rdname accessor-methods
setGeneric('rep_rate_proj', function(object, ...) standardGeneric('rep_rate_proj')) 
#' @rdname accessor-methods
setMethod('rep_rate_proj', signature('MFCLTagProj'),function(object) return(slot(object, 'rep_rate_proj'))) 
#' @rdname accessor-methods
setGeneric('rep_rate_proj<-', function(object, ..., value) standardGeneric('rep_rate_proj<-')) 
#' @rdname accessor-methods
setReplaceMethod('rep_rate_proj', signature(object='MFCLTagProj', value=unname(getSlots('MFCLTagProj')['rep_rate_proj'])),
                 function(object, value){slot(object, 'rep_rate_proj') <- value; return(object)}) 
#' @rdname accessor-methods
setMethod('range', signature(x='MFCLTagProj'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLTagProj', value=unname(getSlots('MFCLTagProj')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 

#############################################################################################################
# class  MFCLTag
#' @rdname accessor-methods
setGeneric('release_groups', function(object, ...) standardGeneric('release_groups')) 
#' @rdname accessor-methods
setMethod('release_groups', signature('MFCLTag'),function(object) return(slot(object, 'release_groups'))) 
#' @rdname accessor-methods
setGeneric('release_groups<-', function(object, ..., value) standardGeneric('release_groups<-')) 
#' @rdname accessor-methods
setReplaceMethod('release_groups', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['release_groups'])),
                 function(object, value){slot(object, 'release_groups') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('release_lengths', function(object, ...) standardGeneric('release_lengths')) 
#' @rdname accessor-methods
setMethod('release_lengths', signature('MFCLTag'),function(object) return(slot(object, 'release_lengths'))) 
#' @rdname accessor-methods
setGeneric('release_lengths<-', function(object, ..., value) standardGeneric('release_lengths<-')) 
#' @rdname accessor-methods
setReplaceMethod('release_lengths', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['release_lengths'])),
                 function(object, value){slot(object, 'release_lengths') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('recoveries', function(object, ...) standardGeneric('recoveries')) 
#' @rdname accessor-methods
setMethod('recoveries', signature('MFCLTag'),function(object) return(slot(object, 'recoveries'))) 
#' @rdname accessor-methods
setGeneric('recoveries<-', function(object, ..., value) standardGeneric('recoveries<-')) 
#' @rdname accessor-methods
setReplaceMethod('recoveries', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['recoveries'])),
                 function(object, value){slot(object, 'recoveries') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('releases', function(object, ...) standardGeneric('releases')) 
#' @rdname accessor-methods
setMethod('releases', signature('MFCLTag'),function(object) return(slot(object, 'releases'))) 
#' @rdname accessor-methods
setGeneric('releases<-', function(object, ..., value) standardGeneric('releases<-')) 
#' @rdname accessor-methods
setReplaceMethod('releases', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['releases'])),
                 function(object, value){slot(object, 'releases') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('recaptures', function(object, ...) standardGeneric('recaptures')) 
#' @rdname accessor-methods
setMethod('recaptures', signature('MFCLTag'),function(object) return(slot(object, 'recaptures'))) 
#' @rdname accessor-methods
setGeneric('recaptures<-', function(object, ..., value) standardGeneric('recaptures<-')) 
#' @rdname accessor-methods
setReplaceMethod('recaptures', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['recaptures'])),
                 function(object, value){slot(object, 'recaptures') <- value; return(object)}) 

#' @rdname accessor-methods
setMethod('range', signature(x='MFCLTag'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLTag', value=unname(getSlots('MFCLTag')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 


#############################################################################################################
# class  MFCLCatch
#' @rdname accessor-methods
setGeneric('total_catch', function(object, ...) standardGeneric('total_catch')) 
#' @rdname accessor-methods
setMethod('total_catch', signature('MFCLCatch'),function(object) return(slot(object, 'total_catch'))) 
#' @rdname accessor-methods
setGeneric('total_catch<-', function(object, ..., value) standardGeneric('total_catch<-')) 
#' @rdname accessor-methods
setReplaceMethod('total_catch', signature(object='MFCLCatch', value=unname(getSlots('MFCLCatch')['total_catch'])),
                 function(object, value){slot(object, 'total_catch') <- value; return(object)}) 

#' @rdname accessor-methods
setGeneric('fishery_catch', function(object, ...) standardGeneric('fishery_catch')) 
#' @rdname accessor-methods
setMethod('fishery_catch', signature('MFCLCatch'),function(object) return(slot(object, 'fishery_catch'))) 
#' @rdname accessor-methods
setGeneric('fishery_catch<-', function(object, ..., value) standardGeneric('fishery_catch<-')) 
#' @rdname accessor-methods
setReplaceMethod('fishery_catch', signature(object='MFCLCatch', value=unname(getSlots('MFCLCatch')['fishery_catch'])),
                 function(object, value){slot(object, 'fishery_catch') <- value; return(object)}) 

#' @rdname accessor-methods
setMethod('range', signature(x='MFCLCatch'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLCatch', value=unname(getSlots('MFCLCatch')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 


#############################################################################################################
# class  MFCLLenFit
#' @rdname accessor-methods
setGeneric('laa', function(object, ...) standardGeneric('laa'))
#' @rdname accessor-methods
setMethod('laa', signature('MFCLLenFit'), function(object) return(slot(object, 'laa')))
#' @rdname accessor-methods
setGeneric('laa<-', function(object, ..., value) standardGeneric('laa<-'))
#' @rdname accessor-methods
setReplaceMethod('laa', signature(object='MFCLLenFit', value=unname(getSlots('MFCLLenFit')['laa'])), 
                 function(object, value){slot(object, 'laa') <- value; return(object)})

#' @rdname accessor-methods
setGeneric('lenfits', function(object, ...) standardGeneric('lenfits'))
#' @rdname accessor-methods
setMethod('lenfits', signature('MFCLLenFit'), function(object) return(slot(object, 'lenfits')))
#' @rdname accessor-methods
setGeneric('lenfits<-', function(object, ..., value) standardGeneric('lenfits<-'))
#' @rdname accessor-methods
setReplaceMethod('lenfits', signature(object='MFCLLenFit', value=unname(getSlots('MFCLLenFit')['lenfits'])), 
                 function(object, value){slot(object, 'lenfits') <- value; return(object)})

#' @rdname accessor-methods
setGeneric('lenagefits', function(object, ...) standardGeneric('lenagefits'))
#' @rdname accessor-methods
setMethod('lenagefits', signature('MFCLLenFit'), function(object) return(slot(object, 'lenagefits')))
#' @rdname accessor-methods
setGeneric('lenagefits<-', function(object, ..., value) standardGeneric('lenagefits<-'))
#' @rdname accessor-methods
setReplaceMethod('lenagefits', signature(object='MFCLLenFit', value=unname(getSlots('MFCLLenFit')['lenagefits'])), 
                 function(object, value){slot(object, 'lenagefits') <- value; return(object)})

#' @rdname accessor-methods
setMethod('range', signature(x='MFCLLenFit'),function(x) return(slot(x,'range'))) 
#' @rdname accessor-methods
setReplaceMethod('range', signature(object='MFCLLenFit', value=unname(getSlots('MFCLLenFit')['range'])),
                 function(object, value){slot(object, 'range') <- value; return(object)}) 



#############################################################################################################
# class  MFCLPseudo
#' @rdname accessor-methods
setGeneric('catcheff', function(object, ...) standardGeneric('catcheff'))
#' @rdname accessor-methods
setMethod('catcheff', signature('MFCLPseudo'), function(object) return(slot(object, 'catcheff')))
#' @rdname accessor-methods
setGeneric('catcheff<-', function(object, ..., value) standardGeneric('catcheff<-'))
#' @rdname accessor-methods
setReplaceMethod('catcheff', signature(object='MFCLPseudo', value=unname(getSlots('MFCLPseudo')['catcheff'])), 
                 function(object, value){slot(object, 'catcheff') <- value; return(object)})
#' @rdname accessor-methods
setGeneric('l_frq', function(object, ...) standardGeneric('l_frq'))
#' @rdname accessor-methods
setMethod('l_frq', signature('MFCLPseudo'), function(object) return(slot(object, 'l_frq')))
#' @rdname accessor-methods
setGeneric('l_frq<-', function(object, ..., value) standardGeneric('l_frq<-'))
#' @rdname accessor-methods
setReplaceMethod('l_frq', signature(object='MFCLPseudo', value=unname(getSlots('MFCLPseudo')['l_frq'])), 
                 function(object, value){slot(object, 'l_frq') <- value; return(object)})
#' @rdname accessor-methods
setGeneric('w_frq', function(object, ...) standardGeneric('w_frq'))
#' @rdname accessor-methods
setMethod('w_frq', signature('MFCLPseudo'), function(object) return(slot(object, 'w_frq')))
#' @rdname accessor-methods
setGeneric('w_frq<-', function(object, ..., value) standardGeneric('w_frq<-'))
#' @rdname accessor-methods
setReplaceMethod('w_frq', signature(object='MFCLPseudo', value=unname(getSlots('MFCLPseudo')['w_frq'])), 
                 function(object, value){slot(object, 'w_frq') <- value; return(object)})


