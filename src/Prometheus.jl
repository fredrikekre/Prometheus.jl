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
            metric_name::String, help::String;
            registry::Union{CollectorRegistry, Nothing}=DEFAULT_REGISTRY,
        )
        initial_value = 0.0
        counter = new(metric_name, help, initial_value)
        if registry !== nothing
            register(registry, counter)
        end
        return counter
    end
end

"""
    Prometheus.Counter(name, help; registry=DEFAULT_REGISTRY)

Construct a Counter collector.

**Arguments**
 - `name :: String`: the name of the counter metric.
 - `help :: String`: the documentation for the counter metric.

**Keyword arguments**
 - `registry :: Prometheus.CollectorRegistry`: the registry in which to register the
   collector. If not specified the default registry is used. Pass `registry = nothing` to
   skip registration.
"""
Counter(::String, ::String; kwargs...)

function metric_names(counter::Counter)
    return (counter.metric_name, )
end

"""
    Prometheus.inc(counter::Counter, v = 1)

Increment the value of the counter with `v`.
`v` must be non-negative, and defaults to `v = 1`.
"""
function inc(counter::Counter, v = 1.0)
    if v < 0
        error("counting backwards")
    end
    @atomic counter.value += v
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
            metric_name::String, help::String;
            registry::Union{CollectorRegistry, Nothing}=DEFAULT_REGISTRY,
        )
        initial_value = 0.0
        gauge = new(metric_name, help, initial_value)
        if registry !== nothing
            register(registry, gauge)
        end
        return gauge
    end
end

"""
    Prometheus.Gauge(name, help; registry=DEFAULT_REGISTRY)

Construct a Gauge collector.

**Arguments**
 - `name :: String`: the name of the gauge metric.
 - `help :: String`: the documentation for the gauge metric.

**Keyword arguments**
 - `registry :: Prometheus.CollectorRegistry`: the registry in which to register the
   collector. If not specified the default registry is used. Pass `registry = nothing` to
   skip registration.
"""
Gauge(::String, ::String; kwargs...)

function metric_names(gauge::Gauge)
    return (gauge.metric_name, )
end

"""
    Prometheus.inc(gauge::Gauge, v = 1)

Increment the value of the gauge with `v`.
`v` defaults to `v = 1`.
"""
function inc(gauge::Gauge, v = 1.0)
    @atomic gauge.value += v
    return nothing
end

"""
    Prometheus.dec(gauge::Gauge, v = 1)

Decrement the value of the gauge with `v`.
`v` defaults to `v = 1`.
"""
function dec(gauge::Gauge, v = 1.0)
    @atomic gauge.value -= v
    return nothing
end

"""
    Prometheus.set(gauge::Gauge, v)

Set the value of the gauge to `v`.
"""
function set(gauge::Gauge, v)
    @atomic gauge.value = v
    return nothing
end

"""
    Prometheus.set_to_current_time(gauge::Gauge)

Set the value of the gauge to the current unixtime in seconds.
"""
function set_to_current_time(gauge::Gauge)
    @atomic gauge.value = time()
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
            metric_name::String, help::String;
            registry::Union{CollectorRegistry, Nothing}=DEFAULT_REGISTRY,
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

"""
    Prometheus.Summary(name, help; registry=DEFAULT_REGISTRY)

Construct a Summary collector.

**Arguments**
 - `name :: String`: the name of the summary metric.
 - `help :: String`: the documentation for the summary metric.

**Keyword arguments**
 - `registry :: Prometheus.CollectorRegistry`: the registry in which to register the
   collector. If not specified the default registry is used. Pass `registry = nothing` to
   skip registration.
"""
Summary(::String, ::String; kwargs...)

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

struct LabelNames{N}
    labelnames::NTuple{N, String}
end

struct LabelValues{N}
    labelvalues::NTuple{N, String}
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

struct Family{C, N} <: Collector
    metric_name::String
    help::String
    labelnames::LabelNames{N}
    children::Dict{LabelValues{N}, C}
    lock::ReentrantLock

    function Family{C}(
            metric_name::String, help::String, labelnames::NTuple{N, String};
            registry::Union{CollectorRegistry, Nothing}=DEFAULT_REGISTRY,
        ) where {C, N}
        children = Dict{LabelValues{N}, C}()
        lock = ReentrantLock()
        family = new{C, N}(metric_name, help, LabelNames(labelnames), children, lock)
        if registry !== nothing
            register(registry, family)
        end
        return family
    end
end

"""
    Prometheus.Family{C}(name, help, labelnames; registry=DEFAULT_REGISTRY)

Create a labeled collector family with labels given by `labelnames`. For every new set of
label values encountered a new collector of type `C <: Collector` will be created.

**Arguments**
 - `name :: String`: the name of the family metric.
 - `help :: String`: the documentation for the family metric.
 - `labelnames :: Tuple{String, ...}`: the label names.

**Keyword arguments**
 - `registry :: Prometheus.CollectorRegistry`: the registry in which to register the
   collector. If not specified the default registry is used. Pass `registry = nothing` to
   skip registration.

**Methods**
 - [`Prometheus.labels`](@ref): return the collector for a specific set of labels.
 - [`Prometheus.remove`](@ref): remove the collector for a specific set of labels.
 - [`Prometheus.clear`](@ref): remove all collectors in the family.

# Examples
```julia
# Construct a family of Counters
counter_family = Prometheus.Family{Counter}(
    "http_requests", "Number of HTTP requests", ["status_code", "endpoint"],
)

# Increment the counter for the labels status_code = "200" and endpoint = "/api"
Prometheus.inc(Prometheus.labels(counter_family, ["200", "/api"]))
```
"""
Family{C}(::String, ::String, ::Any; kwargs...) where C

function metric_names(family::Family)
    return (family.metric_name, )
end

"""
    Prometheus.labels(family::Family{C}, labelvalues::Tuple{String, ...}) where C

Return the collector of type `C` from the family corresponding to the labels given by
`labelvalues`.

!!! note
    This method does an acquire/release of a lock, and a dictionary lookup, to find the
    collector matching the label names. For typical applications this overhead does not
    matter (below 100ns for some basic benchmarks) but it is safe to cache the returned
    collector if required.
"""
function labels(family::Family{C, N}, labelvalues::NTuple{N, String}) where {C, N}
    collector = @lock family.lock get!(family.children, LabelValues(labelvalues)) do
        C(family.metric_name, family.help; registry=nothing)
    end
    return collector
end

"""
    Prometheus.remove(family::Family, labelvalues::Tuple{String, ...})

Remove the collector corresponding to `labelvalues`. Effectively this resets the collector
since [`Prometheus.labels`](@ref) will recreate the collector when called with the same
label names.

!!! note
    This method invalidates cached collectors for the label names.
"""
function remove(family::Family{<:Any, N}, labelvalues::NTuple{N, String}) where N
    @lock family.lock delete!(family.children, LabelValues(labelvalues))
    return
end

"""
    Prometheus.clear(family::Family)

Remove all collectors in the family. Effectively this resets the collectors since
[`Prometheus.labels`](@ref) will recreate them when needed.

!!! note
    This method invalidates all cached collectors.
"""
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

include("gc_collector.jl")
include("process_collector.jl")

# Default registry and collectors
const DEFAULT_REGISTRY = CollectorRegistry()
const GC_COLLECTOR = GCCollector(; registry=DEFAULT_REGISTRY)
const PROCESS_COLLECTOR = ProcessCollector(; registry=DEFAULT_REGISTRY)

end # module Prometheus
