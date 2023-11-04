# Prometheus.jl

## Introduction

This package is a Julia client for [Prometheus](https://prometheus.io/). If you are not
familiar with Prometheus it is recommended to browse the [upstream
documentation](https://prometheus.io/docs/introduction/overview/). The documentation here
focuses on the Julia client.

Two of the basic concepts of a Prometheus client are [Registries](@ref) and
[Collectors](@ref). Registries are collections of collectors, and the collectors are the
units responsible to record and capture metrics. Client libraries implement a default
registry which all collectors implicity register with, so for basic usage there is no need
to interact with a registry (see [Default registry](@ref)).

The third important concept is [Exposition](@ref) of the collected metrics. Typically
metrics are exposed over a HTTP server, as in the [Quickstart](@ref)-example just below. See
the section about [Exposition](@ref) for more details and examples on how metrics can be
exposed.

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


## Collectors

This section documents the collectors that are currently supported. This include the "basic"
collectors ([Counter](@ref), [Gauge](@ref), [Summary](@ref)) as well as some custom
collectors ([GCCollector](@ref), [ProcessCollector](@ref)). There is also a section on how
to implement your own collector, see [Custom collectors](@ref).

Upstream documentation:
 - <https://prometheus.io/docs/concepts/metric_types/>
 - <https://prometheus.io/docs/instrumenting/writing_clientlibs/#metrics>


### Counter

Quoting the [upstream
documentation](https://prometheus.io/docs/concepts/metric_types/#counter):
> A counter is a cumulative metric that represents a single monotonically increasing counter
> whose value can only increase or be reset to zero on restart. For example, you can use a
> counter to represent the number of requests served, tasks completed, or errors.
>
> Do not use a counter to expose a value that can decrease. For example, do not use a
> counter for the number of currently running processes; instead use a gauge.


#### Counter API reference

```@docs
Prometheus.Counter(::String, ::String; kwargs...)
Prometheus.inc(::Prometheus.Counter, ::Any)
```

### Gauge

Quoting the [upstream
documentation](https://prometheus.io/docs/concepts/metric_types/#gauge):
> A gauge is a metric that represents a single numerical value that can arbitrarily go up
> and down.
>
> Gauges are typically used for measured values like temperatures or current memory usage,
> but also "counts" that can go up and down, like the number of concurrent requests.

#### Gauge API reference

```@docs
Prometheus.Gauge(::String, ::String; kwargs...)
Prometheus.inc(::Prometheus.Gauge, ::Any)
Prometheus.dec(::Prometheus.Gauge, ::Any)
Prometheus.set(::Prometheus.Gauge, ::Any)
Prometheus.set_to_current_time(::Prometheus.Gauge)
```

### Summary

Quoting the [upstream
documentation](https://prometheus.io/docs/concepts/metric_types/#summary):
> Similar to a histogram, a summary samples observations (usually things like request
> durations and response sizes). While it also provides a total count of observations and a
> sum of all observed values, it calculates configurable quantiles over a sliding time
> window.

#### Summary API reference

```@docs
Prometheus.Summary(::String, ::String; kwargs...)
Prometheus.observe(::Prometheus.Summary, ::Any)
```

### GCCollector

A collector that exports metrics about allocations and garbage collection (for example
number of allocations, number of bytes allocated, time spent in garbage collection, etc).
These metrics have the `julia_gc_` prefix in their name.

A `GCCollector` is registered automatically with the default registry, see
[Default registry](@ref) for more details.

#### GCCollector API reference

```@docs
Prometheus.GCCollector(; kwargs...)
```

### ProcessCollector

A collector that exports metrics about a running process, for example CPU seconds and
metrics about I/O operations. Metrics from this collector have the `process_` prefix in
their name. This collector is only available on Linux since it requires the `/proc` file
system.

A `ProcessCollector` for the current process is registered automatically with the
default registry, see [Default registry](@ref) for more details.

#### ProcessCollector API reference

```@docs
Prometheus.ProcessCollector(::Function; kwargs...)
```

### Custom collectors

RandomCollector


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


### Default registry


## Exposition

Prometheus support

```@docs
Prometheus.expose
```
