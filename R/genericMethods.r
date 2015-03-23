# Automatically generated Generic Methods 
# Generated from 'createGenericMethods' 
# Generated on  Mon Mar 23 14:35:12 2015 
 
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
#'@export 
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
