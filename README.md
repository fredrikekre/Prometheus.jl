# Prometheus.jl - Prometheus client for Julia

| **Documentation**         | **Build Status**                                        |
|:------------------------- |:------------------------------------------------------- |
| [![][docs-img]][docs-url] | [![][ci-img]][ci-url] [![][coverage-img]][coverage-url] |

Prometheus.jl is a Julia client for [Prometheus](https://prometheus.io/).

## Quickstart

1. Install Prometheus.jl and [HTTP.jl](https://github.com/JuliaWeb/HTTP.jl)
   using the package manager:
   ```
   pkg> add Prometheus HTTP
   ```

2. Paste the following code into a Julia REPL.
   ```julia
   # Load the packages
   using Prometheus, HTTP

   # Create a Counter metric
   const request_counter = Prometheus.Counter("request_count", "Number of handled requests")

   # Start a HTTP server on localhost port 8000 to server the metrics
   server = HTTP.listen!(8000) do http
       Prometheus.inc(request_counter) # Increment the request counter
       return Prometheus.expose(http)  # Expose the metrics
   end
   ```

3. Visit <http://localhost:8000> in your browser. You will see something like the following
   ```
   # HELP gc_alloc_bytes_total Total number of allocated bytes
   # TYPE gc_alloc_bytes_total counter
   gc_alloc_bytes_total 365578814

   [...]

   # HELP request_count Number of handled requests
   # TYPE request_count counter
   request_count 1
   ```
   The output contains some default metrics related to the running process, as well as the
   request counter that we added ourselves. Every time you refresh, the counter will
   increment its value. `close(server)` will shutdown the server.

Visit the [documentation](https://fredrikekre.github.io/Prometheus.jl/) for
much more details!


[docs-img]: https://img.shields.io/badge/docs-latest%20release-blue.svg
[docs-url]: https://fredrikekre.github.io/Prometheus.jl/
[ci-img]: https://github.com/fredrikekre/Prometheus.jl/actions/workflows/CI.yml/badge.svg?event=push
[ci-url]: https://github.com/fredrikekre/Prometheus.jl/actions/workflows/CI.yml
[coverage-img]: https://codecov.io/github/fredrikekre/Prometheus.jl/graph/badge.svg
[coverage-url]: https://codecov.io/github/fredrikekre/Prometheus.jl
