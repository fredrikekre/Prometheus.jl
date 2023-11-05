############################
# GCCollector <: Collector #
############################

mutable struct GCCollector <: Collector
    function GCCollector(; registry::Union{CollectorRegistry, Nothing}=DEFAULT_REGISTRY)
        gcc = new()
        if registry !== nothing
            register(registry, gcc)
        end
        return gcc
    end
end

"""
    Prometheus.GCCollector(; registry=DEFAULT_REGISTRY)

Create a collector that exports metrics about allocations and garbage collection.

**Keyword arguments**
 - `registry :: Prometheus.CollectorRegistry`: the registry in which to register the
   collector. The default registry is used by default. Pass `registry = nothing` to skip
   registration.

!!! note
    A `GCCollector` is registered automatically with the default registry.
    If necessary it can be removed by calling
    ```julia
    Prometheus.unregister(Prometheus.DEFAULT_REGISTRY, Prometheus.GC_COLLECTOR)
    ```
"""
GCCollector(; kwargs...)

function metric_names(::GCCollector)
    return (
        "gc_alloc_total", "gc_free_total", "gc_alloc_bytes_total",
        "gc_live_bytes", "gc_seconds_total", "gc_collections_total",
    )
end

function collect!(metrics::Vector, ::GCCollector)
    # See base/timing.jl
    gc_num = Base.gc_num()
    gc_live_bytes = Base.gc_live_bytes()
    # Push all the metrics
    push!(metrics,
        Metric(
            "counter", "gc_alloc_total", "Total number of allocations (calls to malloc, realloc, etc)",
            LabelNames(["type"]),
            [
                Sample(nothing, LabelValues(["bigalloc"]), gc_num.bigalloc),
                Sample(nothing, LabelValues(["malloc"]), gc_num.malloc),
                Sample(nothing, LabelValues(["poolalloc"]), gc_num.poolalloc),
                Sample(nothing, LabelValues(["realloc"]), gc_num.realloc),
            ],
        ),
        Metric(
            "counter", "gc_free_total", "Total number of calls to free()",
            nothing, Sample(nothing, nothing, gc_num.freecall),
        ),
        Metric(
            "counter", "gc_alloc_bytes_total", "Total number of allocated bytes", nothing,
            Sample(nothing, nothing, Base.gc_total_bytes(gc_num)),
        ),
        Metric(
            "gauge", "gc_live_bytes", "Current number of live bytes", nothing,
            Sample(nothing, nothing, gc_live_bytes),
        ),
        Metric(
            "counter", "gc_seconds_total", "Total time spent in garbage collection", nothing,
            Sample(nothing, nothing, gc_num.total_time / 10^9), # [ns] to [s]
        ),
        Metric(
            "counter", "gc_collections_total", "Total number of calls to garbage collection",
            LabelNames(["type"]),
            [
                Sample(nothing, LabelValues(["full"]), gc_num.full_sweep),
                Sample(nothing, LabelValues(["minor"]), gc_num.pause - gc_num.full_sweep),
            ],
        ),
    )
    return metrics
end
