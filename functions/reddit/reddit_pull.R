
#' @export
redpul_subreddit <- function(name = getOption("subreddit", "all")) {
  {
    box::use(redpul)
  }
  redpul$redpul_find(name)
}


#' @export
praw_subreddit <- function(name = getOption("subreddit", "all"),
                           limit = 10, type = c("new"),
                           client_id = Sys.getenv("REDDIT_CLIENT"),
                           client_secret = Sys.getenv("REDDIT_AUTH"),
                           user_agent = Sys.getenv("USER_AGENT")) {
  {
    box::use(dotenv[load_dot_env])
    box::use(reticulate[import, iterate])
    box::use(reticulate[iterate])
    box::use(purrr[map])
    box::use(dplyr[tibble])
  }
  praw <- import("praw")
  reddit <- praw$Reddit(client_id = client_id, client_secret = client_secret, user_agent = user_agent)
  subreddit <- reddit$subreddit(name)
  type <- match.arg(type)
  new_posts <- switch(type,
    new = subreddit$new(limit = limit)
  )
  new_posts <- iterate(new_posts)
}
