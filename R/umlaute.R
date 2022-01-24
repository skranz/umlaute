replace_umlaute_addin = function(...) {
  library(umlaute)
  replace_umlaute()
}


#' Replace German Umlaute in character vector or RStudio document
#'
#' @param x A character vector or rstudioapi document context. By default the open document in the RStudio source editor
replace_umlaute = function(x=rstudioapi::getSourceEditorContext(), save=FALSE) {

  if (is(x,"document_context")) {
    txt = x$contents
    res = replace_umlaute(txt)
    setDocumentContents(paste0(res,collapse="\n"), id = x$id)
    if (save) {
      documentSave(id=x$id)
    }
    return(invisible(res))
  }

  if (is(x,"character")) {
    res = stringi::stri_replace_all_fixed(
      x,
      c("ä", "ö", "ü", "Ä", "Ö", "Ü","ß"),
      c("ae", "oe", "ue", "Ae", "Oe", "Ue","ss"),
      vectorize_all = FALSE
    )
    return(res)
  }
  stop("Cannot call replace_umlaute with object of class ", paste0(class(x), collapse=", "))
}
