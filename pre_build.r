

library(roxygen2)

createGenericMethods <- function(class, path, add=T, omit=c("range")){
  
  file <- paste(path, "genericMethods.r", sep="//")
  
  if(file.exists(file) && add==F) {
    file.remove(file)
    cat("# Automatically generated Generic Methods \n", file=file)
    cat(paste("# Generated from 'createGenericMethods' \n"), file=file, append=T)
    cat(paste("# Generated on ", date(), "\n \n"), file=file, append=T)
  }
  
  cat(paste("# class ", class, "\n"), file=file, append=T)
  
  slots <- names(getSlots(class))[!is.element(names(getSlots(class)), omit)]
  for(slot in slots){    
    # Accessor
    cat(paste("#'", slot, "\n", "#'@export ",slot, "\n", sep=""), file=file, append=T)
    cat(paste("setGeneric('",slot,"', function(object, ...) standardGeneric('",slot,"')) \n",sep=""), file=file, append=T)
    cat(paste("setMethod('",slot,"', signature('",class,"'),function(object) return(slot(object, '",slot,"'))) \n", sep=""), file=file, append=T)
    # Replacer
    cat(paste("#'", slot,  "\n", "#'@export \n", sep=""), file=file, append=T)
    cat(paste("setGeneric('",slot,"<-', function(object, ..., value) standardGeneric('",slot,"<-')) \n",sep=""), file=file, append=T)
    cat(paste("setReplaceMethod('",slot,"', signature(object='",class,"', value=unname(getSlots('",class,"')['",slot,"'])),
                                function(object, value){slot(object, '",slot,"') <- value; return(object)}) \n", sep=""), file=file, append=T  )  
  }
  # Primitives
  slots <- names(getSlots(class))[is.element(names(getSlots(class)), omit)]
  if(length(slots)>0){
    for(slot in slots){
      cat(paste("#'", slot, "\n", "#'@export ",slot, "\n", sep=""), file=file, append=T)
      cat(paste("setMethod('",slot,"', signature(x='",class,"'),function(x) return(slot(x,'",slot,"'))) \n",sep=""),file=file, append=T) 
      cat(paste("#'", slot,  "\n", "#'@export \n", sep=""), file=file, append=T)
      cat(paste("setGeneric('",slot,"<-', function(object, ..., value) standardGeneric('",slot,"<-')) \n",sep=""), file=file, append=T)
      cat(paste("setReplaceMethod('",slot,"', signature(object='",class,"', value=unname(getSlots('",class,"')['",slot,"'])),
                                function(object, value){slot(object, '",slot,"') <- value; return(object)}) \n", sep=""), file=file, append=T  )
    }
  }
}

createGenericMethods("MFCLFrqStats",path='C://R4MFCL//FLR4MFCL//R',add=F)  # working
createGenericMethods("MFCLLenFreq", path='C://R4MFCL//FLR4MFCL//R',add=T)  # working


roxygenize("C://R4MFCL//FLR4MFCL")





#createGenericMethods("MFCLFrq",     path='C://R4MFCL//FLR4MFCL//R',add=T)

#setMethod("range", signature(x="MFCLFrqStats"),function(x) return(slot(x,"range")))
#setGeneric('range<-', function(object, ..., value) standardGeneric('range<-'))
#setReplaceMethod('range', signature(object='MFCLFrqStats', value=unname(getSlots('MFCLFrqStats')['range'])),
#                 function(object, value){slot(object, 'range') <- value; return(object)})
#range(stat)
#range(stat)[1]<- 0





