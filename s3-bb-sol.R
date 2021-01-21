# define low-level constructor for bb()-function
new_bb <- function(text, type = character()) {
  stopifnot(is.character(text) | is.list(text) | is.factor(text))
  stopifnot(is.character(type))

  structure(
    text,
    class = c("bb", type)
  )
}

# generic function to
# replace ((leading consonants) (1-2 vowels) (trailing consonants)) with
# ((leading consonants) (vowels) b (same vowels again) (trailing consonants)):
bb <- function(text) {
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  UseMethod("bb")
}

# method for character inputs of generic function bb()
bb.character <- function(text) {
  text_in_bb <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
  new_bb(text_in_bb)
}

# method for list inputs of generic function bb()
bb.list <- function(text) {
  text_in_bb <- lapply(text, bb)
  new_bb(text_in_bb, "list")
}

# method for factor inputs generic function bb()
bb.factor <- function(text) {
  text_in_bb <- text
  levels(text_in_bb) <- bb(levels(text))

  if (is.ordered(text)) {
    return(new_bb(text_in_bb, "ordered"))
  }
  new_bb(text_in_bb, "factor")
}

