# SPDX-License-Identifier: MIT

############################
# GCCollector <: Collector #
############################

mutable struct GCCollector <: Collector
    function GCCollector(; registry::Union{CollectorRegistry, Nothing} = DEFAULT_REGISTRY)
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
        "julia_gc_alloc_total", "julia_gc_free_total", "julia_gc_alloc_bytes_total",
        "julia_gc_live_bytes", "julia_gc_seconds_total", "julia_gc_collections_total",
    )
end

function collect!(metrics::Vector, ::GCCollector)
    # See base/timing.jl
    gc_num = Base.gc_num()
    gc_live_bytes = Base.gc_live_bytes()
    # Push all the metrics
    push!(
        metrics,
        let label_names = LabelNames(("type",))
            Metric(
                "counter", "julia_gc_alloc_total", "Total number of allocations (calls to malloc, realloc, etc)",
                [
                    Sample(nothing, label_names, LabelValues(("bigalloc",)), gc_num.bigalloc),
                    Sample(nothing, label_names, LabelValues(("malloc",)), gc_num.malloc),
                    Sample(nothing, label_names, LabelValues(("poolalloc",)), gc_num.poolalloc),
                    Sample(nothing, label_names, LabelValues(("realloc",)), gc_num.realloc),
                ],
            )
        end,
        Metric(
            "counter", "julia_gc_free_total", "Total number of calls to free()",
            Sample(nothing, nothing, nothing, gc_num.freecall),
        ),
        Metric(
            "counter", "julia_gc_alloc_bytes_total", "Total number of allocated bytes",
            Sample(nothing, nothing, nothing, Base.gc_total_bytes(gc_num)),
        ),
        Metric(
            "gauge", "julia_gc_live_bytes", "Current number of live bytes",
            Sample(nothing, nothing, nothing, gc_live_bytes),
        ),
        Metric(
            "counter", "julia_gc_seconds_total", "Total time spent in garbage collection",
            Sample(nothing, nothing, nothing, gc_num.total_time / 10^9), # [ns] to [s]
        ),
        let label_names = LabelNames(("type",))
            Metric(
                "counter", "julia_gc_collections_total", "Total number of calls to garbage collection",
                [
                    Sample(nothing, label_names, LabelValues(("full",)), gc_num.full_sweep),
                    Sample(nothing, label_names, LabelValues(("minor",)), gc_num.pause - gc_num.full_sweep),
                ],
            )
        end,
    )
    return metrics
end
