# Prometheus.jl

*A [Prometheus](https://prometheus.io/) client for Julia*

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
   # HELP request_count Number of handled requests
   # TYPE request_count counter
   request_count 1
   ```
   which is how the counter is presented when Prometheus scrapes the metrics.
   Every time you refresh, the counter will increment its value.
   `close(server)` will shutdown the server.


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
