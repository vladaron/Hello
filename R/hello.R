#'Hello World
#'
#'`hello` says *"hello"* in the user-specified language. The user is asked to
#'give her/his name so that the hello massage gets personalized.
#'
#'@param who a `character` vector of length 1 that specifies the name of the
#'  person to whom the message is adressed.
#'
#'@param lang a `character` vector of length 1 that specifies the preferred
#'  language. Default to "EN" for English. Other possible values include "FR"
#'  for French, "IT" for Italian, "ES" for Spanish, or "DE" for German. Case is
#'  ignored."
#'
#'@param LangData optional data.frame with two columns each of mode character. The
#'  first column gives the language codes and the second column gives the
#'  corresponding "hello" word. Default to language. see ?language
#'
#'@return a `character` vector with a personalized *"hello"* message.
#'
#'@examples
#'hello("james")
#'hello("Amelia", "Es")
#'
#'@export

hello <- function(who, lang = "EN", LangData = Hello::language) {

  stopifnot("Please enter a valid name; see ?hello" = is.character(who) & length(who) == 1)

  lang_low <- tolower(lang)
  lang_choices <- LangData[,1]
  if(any(lang_choices == lang_low)){

    hello <- LangData[,2][which(LangData[,1] == lang_low)]
    exp <- str_c(hello, who,  "!", sep = " ")
    print(exp)

  } else {
    exp <- str_c("Sorry", who,  "your language ('", lang,"') is not available.", sep = " ")
    print(exp)
  }


}


