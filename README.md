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
   The default output contains some default metrics (see [GCCollector](#gccollector) and
   [ProcessCollector](#processcollector)), as well as the request counter that we added
   ourselves. Every time you refresh, the counter will increment its value. `close(server)`
   will shutdown the server.


## Collectors

### Counter

See <https://prometheus.io/docs/concepts/metric_types/#counter> for details.

Supported methods:
 - `Prometheus.inc(counter)`: increment the counter with 1.
 - `Prometheus.inc(counter, v)`: increment the counter with `v`.

### Gauge

See <https://prometheus.io/docs/concepts/metric_types/#gauge> for details.

Supported methods:
 - `Prometheus.inc(gauge)`: increment the gauges's value with 1.
 - `Prometheus.inc(gauge, v)`: increment the gauge's value with `v`.
 - `Prometheus.dec(gauge)`: decrement the gauges's value with 1.
 - `Prometheus.dec(gauge, v)`: decrement the gauge's value with `v`.
 - `Prometheus.set_to_current_time(gauge)`: set the gauge's value to the current unixtime in
   seconds.

### Summary

See <https://prometheus.io/docs/concepts/metric_types/#summary> for details.

Supported methods:
 - `Prometheus.observe(summary, v)`: record the observed value `v`.

### GCCollector

A collector that exports metrics about allocations and garbage collection (for example
number of allocations, number of bytes allocated, time spent in garbage collection, etc).
These metrics have the `gc_` prefix in their name.

### ProcessCollector

A collector that exports metrics about a running process, for example CPU seconds and
metrics about I/O operations. Metrics from this collector have the `process_` prefix in
their name. This collector is only available on Linux since it requires the `/proc` file
system.


## Labels

See <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels> for details.

All metrics can be labeled using the special `Prometheus.Family` collector. For example, a
labeled Counter collector
```julia
labelnames = ["endpoint", "status_code"]
counter_family = Prometheus.Family{Prometheus.Collector}(
    "http_requests",
    "Number of processed requests",
    labelnames,
)
```

Supported methods:
 - `Prometheus.labels(family, ["label 1", "label 2"])`: extract the child collector
   corresponding to the labels `["label 1", "label 2"]`.
 - `Prometheus.remove(family, ["label 1", "label 2"])`: remove the child collector
   corresponding to the labels `["label 1", "label 2"]`.
 - `Prometheus.clear(family)`: clear all child collectors.

## Registries

[docs-img]: https://img.shields.io/badge/docs-latest%20release-blue.svg
[docs-url]: https://fredrikekre.github.io/Prometheus.jl/
[ci-img]: https://github.com/fredrikekre/Prometheus.jl/actions/workflows/CI.yml/badge.svg?event=push
[ci-url]: https://github.com/fredrikekre/Prometheus.jl/actions/workflows/CI.yml
[coverage-img]: https://codecov.io/github/fredrikekre/Prometheus.jl/graph/badge.svg
[coverage-url]: https://codecov.io/github/fredrikekre/Prometheus.jl
