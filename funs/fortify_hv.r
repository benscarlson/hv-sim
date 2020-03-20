
# #TODO: finish this
# fortify.HypervolumeList <- function(hvl,slots=NULL) {
#   # hvs %>% map(fortify) %>% bind_rows
# }

fortify.Hypervolume <- function(hv,slots=NULL) {
  
  #if slot is NULL, make a dataframe of all relevant slots
  #in some cases slots might not exists (e.g. 'Data' doesn't exist for slot operations)
  # so need to intersection with existing slots
  if(is.null(slots)) {
    slots <- intersect(c('Data','RandomPoints'),getSlot(hv))
  }

  df <- enframe(slots,name=NULL,value='slot') %>%
    mutate(
      name=hv@Name,
      data=map(slot,function(.) {
        pluck(hv,.) %>% as_tibble
    })) %>%
    unnest(cols=data)

  return(df)
}
