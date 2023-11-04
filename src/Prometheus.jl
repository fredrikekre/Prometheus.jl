module Prometheus

using CodecZlib: GzipCompressorStream
using HTTP: HTTP
using SimpleBufferStream: BufferStream
using Sockets: Sockets

abstract type Collector end

#########
# Utils #
#########

abstract type PrometheusException <: Exception end

struct PrometheusUnreachable <: PrometheusException end
unreachable() = throw(PrometheusUnreachable())

struct PrometheusAssert <: PrometheusException end
macro assert(cond)
    return quote
        $(esc(cond)) || throw(PrometheusAssert())
    end
end

###########################################
# Compat for const fields, @lock, @atomic #
###########################################
@eval macro $(Symbol("const"))(field)
    if VERSION >= v"1.8.0-DEV.1148"
        Expr(:const, esc(field))
    else
        return esc(field)
    end
end
if VERSION < v"1.7.0"
    # Defined but not exported
    using Base: @lock
end
if !isdefined(Base, Symbol("@atomic")) # v1.7.0
    const ATOMIC_COMPAT_LOCK = ReentrantLock()
    macro atomic(expr)
        if Meta.isexpr(expr, :(::))
            return esc(expr)
        else
            return quote
                lock(ATOMIC_COMPAT_LOCK)
                tmp = $(esc(expr))
                unlock(ATOMIC_COMPAT_LOCK)
                tmp
            end
        end
    end
end
if !isdefined(Base, :eachsplit) # v1.8.0
    const eachsplit = split
end


#####################
# CollectorRegistry #
#####################

struct CollectorRegistry
    lock::ReentrantLock
    collectors::Base.IdSet{Collector}
    function CollectorRegistry()
        return new(ReentrantLock(), Base.IdSet{Collector}())
    end
end

function register(reg::CollectorRegistry, collector::Collector)
    existing_names = Set{String}() # TODO: Cache existing_names in the registry?
    @lock reg.lock begin
        for c in reg.collectors
            union!(existing_names, metric_names(c))
        end
        if any(in(existing_names), metric_names(collector))
            error("not allowed")
        end
        push!(reg.collectors, collector)
    end
    return
end

function unregister(reg::CollectorRegistry, collector::Collector)
    @lock reg.lock delete!(reg.collectors, collector)
    return
end

##############
# Collectors #
##############

# abstract type Collector end

function collect(collector::Collector)
    return collect!(Metric[], collector)
end

########################
# Counter <: Collector #
########################
# https://prometheus.io/docs/instrumenting/writing_clientlibs/#counter

# TODO: A counter is ENCOURAGED to have:
#  - A way to count exceptions throw/raised in a given piece of code, and optionally only
#    certain types of exceptions. This is count_exceptions in Python.

mutable struct Counter <: Collector
    @const metric_name::String
    @const help::String
    @atomic value::Float64

    function Counter(
            registry::Union{CollectorRegistry, Nothing},
            metric_name::String, help::String,
        )
        initial_value = 0.0
        counter = new(metric_name, help, initial_value)
        if registry !== nothing
            register(registry, counter)
        end
        return counter
    end
end

function Counter(metric_name::String, help::String)
    return Counter(DEFAULT_REGISTRY, metric_name, help)
end

"""
    Prometheus.Counter(; name, help, registry=DEFAULT_REGISTRY)

Construct a Counter collector.

**Required keyword arguments**
 - `name :: String`: the name of the counter metric.
 - `help :: String`: the documentation for the counter metric.

**Optional keyword arguments**
 - `registry :: Prometheus.CollectorRegistry`: the registry in which to register the
   collector. If not specified the default registry is used. Pass `registry = nothing` to
   skip registration.
"""
Counter

function metric_names(counter::Counter)
    return (counter.metric_name, )
end

"""
    Prometheus.inc(c::Counter, v = 1)

Increment the value of the counter `c` with `v`.
`v` must be non-negative, and defaults to `v = 1`.
"""
function inc(m::Counter, v = 1.0)
    if v < 0
        error("counting backwards")
    end
    @atomic m.value += v
    return nothing
end

function collect!(metrics::Vector, counter::Counter)
    push!(metrics,
        Metric(
            "counter", counter.metric_name, counter.help,
            nothing, Sample(nothing, nothing, @atomic(counter.value)),
        ),
    )
    return metrics
end


######################
# Gauge <: Collector #
######################
# https://prometheus.io/docs/instrumenting/writing_clientlibs/#gauge

# TODO: A gauge is ENCOURAGED to have:
#  - A way to track in-progress requests in some piece of code/function. This is
#    track_inprogress in Python.
#  - A way to time a piece of code and set the gauge to its duration in seconds. This is
#    useful for batch jobs. This is startTimer/setDuration in Java and the time()
#    decorator/context manager in Python. This SHOULD match the pattern in Summary/Histogram
#    (though set() rather than observe()).

mutable struct Gauge <: Collector
    @const metric_name::String
    @const help::String
    @atomic value::Float64

    function Gauge(
            registry::Union{CollectorRegistry, Nothing},
            metric_name::String, help::String,
        )
        initial_value = 0.0
        gauge = new(metric_name, help, initial_value)
        if registry !== nothing
            register(registry, gauge)
        end
        return gauge
    end
end

function Gauge(metric_name::String, help::String)
    return Gauge(DEFAULT_REGISTRY, metric_name, help)
end

"""
    Prometheus.Gauge(; name, help, registry=DEFAULT_REGISTRY)

Construct a Gauge collector.

**Required keyword arguments**
 - `name :: String`: the name of the gauge metric.
 - `help :: String`: the documentation for the gauge metric.

**Optional keyword arguments**
 - `registry :: Prometheus.CollectorRegistry`: the registry in which to register the
   collector. If not specified the default registry is used. Pass `registry = nothing` to
   skip registration.
"""
Gauge

function metric_names(gauge::Gauge)
    return (gauge.metric_name, )
end

"""
    Prometheus.inc(g::Gauge, v = 1)

Increment the value of the gauge `g` with `v`.
`v` defaults to `v = 1`.
"""
function inc(m::Gauge, v = 1.0)
    if v < 0
        error("incrementing with negative value, use dec(...)?")
    end
    @atomic m.value += v
    return nothing
end

"""
    Prometheus.dec(g::Gauge, v = 1)

Decrement the value of the gauge `g` with `v`.
`v` defaults to `v = 1`.
"""
function dec(m::Gauge, v = 1.0)
    if v < 0
        error("decrementing with negative value, use inc(...)?")
    end
    @atomic m.value -= v
    return nothing
end

"""
    Prometheus.set(g::Gauge, v)

Set the value of the gauge to `v`.
"""
function set(m::Gauge, v)
    @atomic m.value = v
    return nothing
end

"""
    Prometheus.set_to_current_time(g::Gauge)

Set the value of the gauge to the current unixtime in seconds.
"""
function set_to_current_time(m::Gauge)
    @atomic m.value = time()
    return nothing
end

function collect!(metrics::Vector, gauge::Gauge)
    push!(metrics,
        Metric(
            "gauge", gauge.metric_name, gauge.help,
            nothing, Sample(nothing, nothing, @atomic(gauge.value)),
        ),
    )
    return metrics
end


########################
# Summary <: Collector #
########################
# https://prometheus.io/docs/instrumenting/writing_clientlibs/#summary

# TODO: A summary SHOULD have the following methods:
#  - Some way to time code for users in seconds. In Python this is the time()
#    decorator/context manager. In Java this is startTimer/observeDuration. Units other than
#    seconds MUST NOT be offered (if a user wants something else, they can do it by hand).
#    This should follow the same pattern as Gauge/Histogram.

mutable struct Summary <: Collector
    @const metric_name::String
    @const help::String
    @atomic _count::Int
    @atomic _sum::Float64

    function Summary(
            registry::Union{CollectorRegistry, Nothing},
            metric_name::String, help::String,
        )
        initial_count = 0
        initial_sum = 0.0
        summary = new(metric_name, help, initial_count, initial_sum)
        if registry !== nothing
            register(registry, summary)
        end
        return summary
    end
end

function Summary(metric_name::String, help::String)
    return Summary(DEFAULT_REGISTRY, metric_name, help)
end

"""
    Prometheus.Summary(; name, help, registry=DEFAULT_REGISTRY)

Construct a Summary collector.

**Required keyword arguments**
 - `name :: String`: the name of the summary metric.
 - `help :: String`: the documentation for the summary metric.

**Optional keyword arguments**
 - `registry :: Prometheus.CollectorRegistry`: the registry in which to register the
   collector. If not specified the default registry is used. Pass `registry = nothing` to
   skip registration.
"""
Summary

function metric_names(summary::Summary)
    return (summary.metric_name * "_count", summary.metric_name * "_sum")
end

"""
    Prometheus.observe(summary::Summary, v)

Add the observed value `v` to the summary.
This increases the sum and count of the summary with `v` and `1`, respectively.
"""
function observe(summary::Summary, v)
    @atomic summary._count += 1
    @atomic summary._sum += v
    return nothing
end

function collect!(metrics::Vector, summary::Summary)
    push!(metrics,
        Metric(
            "summary", summary.metric_name, summary.help, nothing,
            [
                Sample("_count", nothing, @atomic(summary._count)),
                Sample("_sum", nothing, @atomic(summary._sum)),
            ]
        ),
    )
    return metrics
end


####################################
# Family{<:Collector} <: Collector #
####################################

struct LabelNames
    labelnames::Vector{String}
end

struct LabelValues
    labelvalues::Vector{String}
end
function Base.hash(l::LabelValues, h::UInt)
    h = hash(0x94a2d04ee9e5a55b, h) # hash("Prometheus.LabelValues") on Julia 1.9.3
    for v in l.labelvalues
        h = hash(v, h)
    end
    return h
end
function Base.:(==)(l1::LabelValues, l2::LabelValues)
    return l1.labelvalues == l2.labelvalues
end

struct Family{C} <: Collector
    metric_name::String
    help::String
    labelnames::LabelNames
    children::Dict{LabelValues, C}
    lock::ReentrantLock

    function Family{C}(
            registry::Union{CollectorRegistry, Nothing},
            metric_name::String, help::String, labelnames::LabelNames,
        ) where C
        children = Dict{LabelValues, C}()
        lock = ReentrantLock()
        family = new(metric_name, help, labelnames, children, lock)
        if registry !== nothing
            register(registry, family)
        end
        return family
    end
end

function Family{C}(metric_name::String, help::String, labelnames) where C
    return Family{C}(DEFAULT_REGISTRY, metric_name, help, LabelNames(labelnames))
end
function Family{C}(registry::Union{CollectorRegistry, Nothing}, metric_name::String, help::String, labelnames) where C
    return Family{C}(registry, metric_name, help, LabelNames(labelnames))
end

function metric_names(family::Family)
    return (family.metric_name, )
end

function labels(family::Family{C}, labelvalues::LabelValues) where C
    collector = @lock family.lock get!(family.children, labelvalues) do
        C(nothing, family.metric_name, family.help)
    end
    return collector
end
labels(family::Family, labelvalues) = labels(family, LabelValues(labelvalues))

function remove(family::Family, labelvalues::LabelValues)
    @lock family.lock delete!(family.children, labelvalues)
    return
end
remove(family::Family, labelvalues) = remove(family, LabelValues(labelvalues))

function clear(family::Family)
    @lock family.lock empty!(family.children)
    return
end

prometheus_type(::Type{Counter}) = "counter"
prometheus_type(::Type{Gauge}) = "gauge"
prometheus_type(::Type{Summary}) = "summary"
prometheus_type(::Type) = unreachable()

function collect!(metrics::Vector, family::Family{C}) where C
    type = prometheus_type(C)
    samples = Sample[]
    buf = Metric[]
    @lock family.lock begin
        for (labels, child) in family.children
            # collect!(...) the child, throw away the metric, but keep the samples
            child_metrics = collect!(resize!(buf, 0), child)
            length(child_metrics) !=1 && error("multiple metrics not supported (yet?)")
            child_metric = child_metrics[1]
            @assert(child_metric.type == type)
            # Unwrap and rewrap samples with the labels
            child_samples = child_metric.samples
            if child_samples isa Sample
                push!(samples, Sample(child_samples.suffix, labels, child_samples.value))
            else
                @assert(child_samples isa Vector{Sample})
                for child_sample in child_samples
                    @assert(child_sample.labels === nothing)
                    push!(samples, Sample(child_sample.suffix, labels, child_sample.value))
                end
            end
        end
    end
    # Sort samples lexicographically by the labels
    sort!(samples; by = function(x)
        labels = x.labels
        @assert(labels !== nothing)
        return labels.labelvalues
    end)
    push!(metrics, Metric(type, family.metric_name, family.help, family.labelnames, samples))
    return metrics
end

############################
# GCCollector <: Collector #
############################

mutable struct GCCollector <: Collector
    function GCCollector(registry::Union{CollectorRegistry, Nothing}=DEFAULT_REGISTRY)
        gcc = new()
        if registry !== nothing
            register(registry, gcc)
        end
        return gcc
    end
end

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


##############
# Exposition #
##############

struct Sample
    suffix::Union{String, Nothing} # e.g. _count or _sum
    labels::Union{LabelValues, Nothing}
    value::Float64
end

struct Metric
    type::String
    metric_name::String
    help::String
    labelnames::Union{LabelNames, Nothing}
    # TODO: Union{Tuple{Sample}, Vector{Sample}} would always make this iterable.
    samples::Union{Sample, Vector{Sample}}
end

function expose_metric(io::IO, metric::Metric)
    println(io, "# HELP ", metric.metric_name, " ", metric.help)
    println(io, "# TYPE ", metric.metric_name, " ", metric.type)
    labelnames = metric.labelnames
    samples = metric.samples
    if samples isa Sample
        # Single sample, no labels
        @assert(labelnames === nothing)
        @assert(samples.labels === nothing)
        @assert(samples.suffix === nothing)
        val = samples.value
        println(io, metric.metric_name, " ", isinteger(val) ? Int(val) : val)
    else
        # Multiple samples, might have labels
        @assert(samples isa Vector{Sample})
        for sample in samples
            # Print metric name
            print(io, metric.metric_name)
            # Print potential suffix
            if sample.suffix !== nothing
                print(io, sample.suffix)
            end
            # Print potential labels
            labels = sample.labels
            if labelnames !== nothing && labels !== nothing
                first = true
                print(io, "{")
                for (name, value) in zip(labelnames.labelnames, labels.labelvalues)
                    first || print(io, ",")
                    print(io, name, "=\"", value, "\"")
                    first = false
                end
                print(io, "}")
            end
            # Print the value
            println(io, " ", isinteger(sample.value) ? Int(sample.value) : sample.value)
        end
    end
end

"""
    Prometheus.expose(file::String, reg::CollectorRegistry = DEFAULT_REGISTRY)

Export all metrics in `reg` by writing them to the file `file`.
"""
function expose(path::String, reg::CollectorRegistry = DEFAULT_REGISTRY)
    dir = dirname(path)
    mkpath(dir)
    mktemp(dirname(path)) do tmp_path, tmp_io
        expose_io(tmp_io, reg)
        close(tmp_io)
        mv(tmp_path, path; force=true)
    end
    return
end

"""
    expose(io::IO, reg::CollectorRegistry = DEFAULT_REGISTRY)

Export all metrics in `reg` by writing them to the I/O stream `io`.
"""
function expose(io::IO, reg::CollectorRegistry = DEFAULT_REGISTRY)
    return expose_io(io, reg)
end

function expose_io(io::IO, reg::CollectorRegistry)
    # Collect all metrics
    metrics = Metric[]
    @lock reg.lock for collector in reg.collectors
        collect!(metrics, collector)
    end
    sort!(metrics; by = metric -> metric.metric_name)
    # Write to IO
    buf = IOBuffer(; maxsize=1024^2) # 1 MB
    for metric in metrics
        truncate(buf, 0)
        expose_metric(buf, metric)
        seekstart(buf)
        write(io, buf)
    end
    return
end

#######################
# HTTP.jl integration #
#######################

const CONTENT_TYPE_LATEST = "text/plain; version=0.0.4; charset=utf-8"

function gzip_accepted(http::HTTP.Stream)
    accept_encoding = HTTP.header(http.message, "Accept-Encoding")
    for enc in eachsplit(accept_encoding, ',')
        if lowercase(strip(first(eachsplit(enc, ';')))) == "gzip"
            return true
        end
    end
    return false
end

"""
    expose(http::HTTP.Stream, reg::CollectorRegistry = DEFAULT_REGISTRY; kwargs...)

Export all metrics in `reg` by writing them to the HTTP stream `http`.

The caller is responsible for checking e.g. the HTTP method and URI target. For
HEAD requests this method do not write a body, however.
"""
function expose(http::HTTP.Stream, reg::CollectorRegistry = DEFAULT_REGISTRY; compress::Bool=true)
    # TODO: Handle Accept request header for different formats?
    # Compress by default if client supports it and user haven't disabled it
    if compress
        compress = gzip_accepted(http)
    end
    # Create the response
    HTTP.setstatus(http, 200)
    HTTP.setheader(http, "Content-Type" => CONTENT_TYPE_LATEST)
    if compress
        HTTP.setheader(http, "Content-Encoding" => "gzip")
    end
    HTTP.startwrite(http)
    # The user is responsible for making sure that e.g. target and method is
    # correct, but at least we skip writing the body for HEAD requests.
    if http.message.method != "HEAD"
        if compress
            buf = BufferStream()
            gzstream = GzipCompressorStream(buf)
            tsk = @async try
                expose_io(gzstream, reg)
            finally
                # Close the compressor stream to free resources in zlib and
                # to let the write(http, buf) below finish.
                close(gzstream)
            end
            write(http, buf)
            wait(tsk)
        else
            expose_io(http, reg)
        end
    end
    return
end

include("process_collector.jl")

# Default registry and collectors
const DEFAULT_REGISTRY = CollectorRegistry()
const GC_COLLECTOR = GCCollector(DEFAULT_REGISTRY)
const PROCESS_COLLECTOR = ProcessCollector(DEFAULT_REGISTRY)

end # module Prometheus
