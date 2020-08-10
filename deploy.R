
git <- function (..., echo_cmd = TRUE, echo = TRUE, error_on_status = TRUE)
{
    processx::run( "git"
                 , c(...)
                 , echo_cmd = echo_cmd
                 , echo = echo
                 , error_on_status = error_on_status)
}

git_has_remote_branch <- function (remote, branch) {
    git("ls-remote", "--quiet",
        "--exit-code", remote, branch, echo = FALSE, echo_cmd = FALSE,
        error_on_status = FALSE)$status == 0
}
git_current_branch <- function () {
    branch <- git("rev-parse", "--abbrev-ref", "HEAD",
        echo = FALSE, echo_cmd = FALSE)$stdout
    sub("\n$", "", branch)
}
github_worktree_add <- function (dir, remote, branch) {
    rule("Adding worktree", line = 1)
    git("worktree", "add", "--track", "-B",
        branch, dir, paste0(remote, "/", branch))
}
github_worktree_remove <- function (dir) {
    cli::cat_rule("Removing worktree", line = 1)
    git("worktree", "remove", dir)
}

deploy <-
function( pkg = "."
        , commit_message = "Deployed"
        , branch = "gh-pages"
        , remote = "origin"
        , github_pages = (branch == "gh-pages")
        , ...)
{
    dest_dir <- fs::dir_create(fs::file_temp())
    on.exit(fs::dir_delete(dest_dir))
    if (!git_has_remote_branch(remote, branch)) {
        old_branch <- git_current_branch()
        git("checkout", "--orphan", branch)
        git("rm", "-rf", "--quiet", ".")
        git("commit", "--allow-empty", "-m",
            sprintf("Initializing %s branch", branch))
        git("push", remote, paste0("HEAD:", branch))
        git("checkout", old_branch)
    }
    git("remote", "set-branches", remote, branch)
    git("fetch", remote, branch)
    github_worktree_add(dest_dir, remote, branch)
    on.exit(github_worktree_remove(dest_dir), add = TRUE)

    pkg <- as_pkgdown(pkg, override = list(destination = dest_dir))
    build_site(pkg, devel = FALSE, preview = FALSE, install = FALSE, ...)
    if (github_pages) {
        build_github_pages(pkg)
    }
    github_push(dest_dir, commit_message, remote, branch)
    invisible()
}
