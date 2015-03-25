# Automatically generated Generic Methods 
# Generated from 'createGenericMethods' 
# Generated on  Wed Mar 25 11:58:53 2015 
 
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
#'cohort_growth_devs
#'@export cohort_growth_devs
setGeneric('cohort_growth_devs', function(object, ...) standardGeneric('cohort_growth_devs')) 
setMethod('cohort_growth_devs', signature('MFCLBiol'),function(object) return(slot(object, 'cohort_growth_devs'))) 
#'cohort_growth_devs
#'@export 
setGeneric('cohort_growth_devs<-', function(object, ..., value) standardGeneric('cohort_growth_devs<-')) 
setReplaceMethod('cohort_growth_devs', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['cohort_growth_devs'])),
                                function(object, value){slot(object, 'cohort_growth_devs') <- value; return(object)}) 
#'season_growth_pars
#'@export season_growth_pars
setGeneric('season_growth_pars', function(object, ...) standardGeneric('season_growth_pars')) 
setMethod('season_growth_pars', signature('MFCLBiol'),function(object) return(slot(object, 'season_growth_pars'))) 
#'season_growth_pars
#'@export 
setGeneric('season_growth_pars<-', function(object, ..., value) standardGeneric('season_growth_pars<-')) 
setReplaceMethod('season_growth_pars', signature(object='MFCLBiol', value=unname(getSlots('MFCLBiol')['season_growth_pars'])),
                                function(object, value){slot(object, 'season_growth_pars') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLBiol'),function(x) return(slot(x,'range'))) 
#'range
#'@export 
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
#'@export 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLTagRep', value=unname(getSlots('MFCLTagRep')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 
# class  MFCLRec 
#'rec_init
#'@export rec_init
setGeneric('rec_init', function(object, ...) standardGeneric('rec_init')) 
setMethod('rec_init', signature('MFCLRec'),function(object) return(slot(object, 'rec_init'))) 
#'rec_init
#'@export 
setGeneric('rec_init<-', function(object, ..., value) standardGeneric('rec_init<-')) 
setReplaceMethod('rec_init', signature(object='MFCLRec', value=unname(getSlots('MFCLRec')['rec_init'])),
                                function(object, value){slot(object, 'rec_init') <- value; return(object)}) 
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
#'range
#'@export range
setMethod('range', signature(x='MFCLRec'),function(x) return(slot(x,'range'))) 
#'range
#'@export 
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
#'diff_coffs
#'@export diff_coffs
setGeneric('diff_coffs', function(object, ...) standardGeneric('diff_coffs')) 
setMethod('diff_coffs', signature('MFCLRegion'),function(object) return(slot(object, 'diff_coffs'))) 
#'diff_coffs
#'@export 
setGeneric('diff_coffs<-', function(object, ..., value) standardGeneric('diff_coffs<-')) 
setReplaceMethod('diff_coffs', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['diff_coffs'])),
                                function(object, value){slot(object, 'diff_coffs') <- value; return(object)}) 
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
#'reg_rec_var
#'@export reg_rec_var
setGeneric('reg_rec_var', function(object, ...) standardGeneric('reg_rec_var')) 
setMethod('reg_rec_var', signature('MFCLRegion'),function(object) return(slot(object, 'reg_rec_var'))) 
#'reg_rec_var
#'@export 
setGeneric('reg_rec_var<-', function(object, ..., value) standardGeneric('reg_rec_var<-')) 
setReplaceMethod('reg_rec_var', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['reg_rec_var'])),
                                function(object, value){slot(object, 'reg_rec_var') <- value; return(object)}) 
#'reg_pars
#'@export reg_pars
setGeneric('reg_pars', function(object, ...) standardGeneric('reg_pars')) 
setMethod('reg_pars', signature('MFCLRegion'),function(object) return(slot(object, 'reg_pars'))) 
#'reg_pars
#'@export 
setGeneric('reg_pars<-', function(object, ..., value) standardGeneric('reg_pars<-')) 
setReplaceMethod('reg_pars', signature(object='MFCLRegion', value=unname(getSlots('MFCLRegion')['reg_pars'])),
                                function(object, value){slot(object, 'reg_pars') <- value; return(object)}) 
#'range
#'@export range
setMethod('range', signature(x='MFCLRegion'),function(x) return(slot(x,'range'))) 
#'range
#'@export 
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
#'q_dev_coffs
#'@export q_dev_coffs
setGeneric('q_dev_coffs', function(object, ...) standardGeneric('q_dev_coffs')) 
setMethod('q_dev_coffs', signature('MFCLSel'),function(object) return(slot(object, 'q_dev_coffs'))) 
#'q_dev_coffs
#'@export 
setGeneric('q_dev_coffs<-', function(object, ..., value) standardGeneric('q_dev_coffs<-')) 
setReplaceMethod('q_dev_coffs', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['q_dev_coffs'])),
                                function(object, value){slot(object, 'q_dev_coffs') <- value; return(object)}) 
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
#'range
#'@export range
setMethod('range', signature(x='MFCLSel'),function(x) return(slot(x,'range'))) 
#'range
#'@export 
setGeneric('range<-', function(object, ..., value) standardGeneric('range<-')) 
setReplaceMethod('range', signature(object='MFCLSel', value=unname(getSlots('MFCLSel')['range'])),
                                function(object, value){slot(object, 'range') <- value; return(object)}) 
