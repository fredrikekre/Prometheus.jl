const liveserver = "liveserver" in ARGS

if liveserver
    using Revise
    Revise.revise()
end

using Documenter
using Prometheus

# Build it!
Documenter.makedocs(
    sitename = "Prometheus.jl",
    format = Documenter.HTML(
        canonical = "https://fredrikekre.github.io/Prometheus.jl/v1",
    ),
    modules = [Prometheus],
    warnonly = true,
)

# Rewrite to "single page" mode
index = joinpath(@__DIR__, "build/index.html")
str = read(index, String)
# Remove the sidebar
str = replace(str, r"<nav class=\"docs-sidebar\">.*?</nav>" => "")
# Remove the breadcrumb
str = replace(str, r"<nav class=\"breadcrumb\">.*?</nav>" => "")
# Remove the hamburger in narrow mode
str = replace(str, r"<a class=\"docs-sidebar-button.*?</a>" => "")
# Move the buttons to the right
str = replace(str, r"<div class=\"docs-right\">" => "<div class=\"docs-right\" style=\"margin-left: auto;\">")
# Center the content
str = replace(str, r"<div class=\"docs-main\">" => "<div class=\"docs-main\" style=\"margin: auto; padding-right: 0;\">")
# Remove the global docstring folding
str = replace(str, r"<a class=\"docs-article-toggle-button.*?</a>" => "")
# Write it back
write(index, str)

# Nuke a function in documenter.js...
documenterjs = joinpath(@__DIR__, "build/assets/documenter.js")
str = read(documenterjs, String)
str = replace(
    str,
    """
    document.querySelector(".docs-search-query").addEventListener("click", () => {
      openModal();
    });
    """ => ""
)
write(documenterjs, str)

# Deploy it!
if !liveserver
    Documenter.deploydocs(
        repo = "github.com/fredrikekre/Prometheus.jl.git",
        push_preview = true,
        versions = ["v1" => "v^", "v#.#", "dev" => "dev"],
    )
end
