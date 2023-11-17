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

struct ArgumentError <: PrometheusException
    msg::String
end
function Base.showerror(io::IO, err::ArgumentError)
    print(io, "Prometheus.", nameof(typeof(err)), ": ", err.msg)
end

struct UnreachableError <: PrometheusException end
unreachable() = throw(UnreachableError())

struct AssertionError <: PrometheusException end
macro assert(cond)
    return quote
        $(esc(cond)) || throw(AssertionError())
    end
end

function Base.showerror(io::IO, err::Union{AssertionError, UnreachableError})
    print(
        io,
        "Prometheus.", nameof(typeof(err)),
        ": this is unexpected. Please file an issue at " *
        "https://github.com/fredrikekre/Prometheus.jl/issues/new",
    )
end

# https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels
# Metric names may contain ASCII letters, digits, underscores, and colons.
# It must match the regex [a-zA-Z_:][a-zA-Z0-9_:]*.
# Note: The colons are reserved for user defined recording rules. They should
# not be used by exporters or direct instrumentation.
function verify_metric_name(metric_name::String)
    metric_name_regex = r"^[a-zA-Z_:][a-zA-Z0-9_:]*$"
    if !occursin(metric_name_regex, metric_name)
        throw(ArgumentError("metric name \"$(metric_name)\" is invalid"))
    end
    return metric_name
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
        for metric_name in metric_names(collector)
            if metric_name in existing_names
                throw(ArgumentError(
                    "collector already contains a metric with the name \"$(metric_name)\""
                ))
            end
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
        counter = new(verify_metric_name(metric_name), help, initial_value)
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

**Methods**
 - [`Prometheus.inc`](@ref): increment the counter.
"""
Counter(::String, ::String; kwargs...)

function metric_names(counter::Counter)
    return (counter.metric_name, )
end

"""
    Prometheus.inc(counter::Counter, v = 1)

Increment the value of the counter with `v`. The value defaults to `v = 1`.

Throw a `Prometheus.ArgumentError` if `v < 0` (a counter must not decrease).
"""
function inc(counter::Counter, v = 1.0)
    if v < 0
        throw(ArgumentError(
            "invalid value $v: a counter must not decrease"
        ))
    end
    @atomic counter.value += v
    return nothing
end

function collect!(metrics::Vector, counter::Counter)
    push!(metrics,
        Metric(
            "counter", counter.metric_name, counter.help,
            Sample(nothing, nothing, nothing, @atomic(counter.value)),
        ),
    )
    return metrics
end


######################
# Gauge <: Collector #
######################
# https://prometheus.io/docs/instrumenting/writing_clientlibs/#gauge

mutable struct Gauge <: Collector
    @const metric_name::String
    @const help::String
    @atomic value::Float64

    function Gauge(
            metric_name::String, help::String;
            registry::Union{CollectorRegistry, Nothing}=DEFAULT_REGISTRY,
        )
        initial_value = 0.0
        gauge = new(verify_metric_name(metric_name), help, initial_value)
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

**Methods**
 - [`Prometheus.inc`](@ref inc(::Gauge, ::Any)): increment the value
   of the gauge.
 - [`Prometheus.dec`](@ref): decrement the value of the gauge.
 - [`Prometheus.set`](@ref): set the value of the gauge.
 - [`Prometheus.set_to_current_time`](@ref): set the value of the gauge to the
   current unixtime.
 - [`Prometheus.@time`](@ref): time a section and set the value of the the gauge to the
   elapsed time.
 - [`Prometheus.@inprogress`](@ref): Track number of inprogress operations; increment the
   gauge when entering the section, decrement it when leaving.
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
            Sample(nothing, nothing, nothing, @atomic(gauge.value)),
        ),
    )
    return metrics
end


########################
# Summary <: Collector #
########################
# https://prometheus.io/docs/instrumenting/writing_clientlibs/#summary

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
        summary = new(verify_metric_name(metric_name), help, initial_count, initial_sum)
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

**Methods**
 - [`Prometheus.observe`](@ref): add an observation to the summary.
 - [`Prometheus.@time`](@ref): time a section and add the elapsed time as an observation.
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
            "summary", summary.metric_name, summary.help,
            [
                Sample("_count", nothing, nothing, @atomic(summary._count)),
                Sample("_sum", nothing, nothing, @atomic(summary._sum)),
            ]
        ),
    )
    return metrics
end


################
# "Decorators" #
################

"""
    Prometheus.@time collector expr

Time the evaluation of `expr` and send the elapsed time in seconds to `collector`. The
specific action depends on the type of collector:

 - `collector :: Gauge`: set the value of the gauge to the elapsed time
   ([`Prometheus.set`](@ref))
 - `collector :: Summary`: add the elapsed time as an observation
   ([`Prometheus.observe`](@ref))

The expression to time, `expr`, can be a single expression (for example a function call), or
a code block (`begin`, `let`, etc), e.g.
```julia
Prometheus.@time collector <expr>

Prometheus.@time collector begin
    <expr>
end
```

It is also possible to apply the macro to a function *definition*, i.e.
```julia
Prometheus.@time collector function time_this(args...)
    # function body
end
```
to time every call to this function (covering all call sites).
"""
macro time(collector, expr)
    return expr_gen(:time, collector, expr)
end

at_time(gauge::Gauge, v) = set(gauge, v)
at_time(summary::Summary, v) = observe(summary, v)
at_time(::Collector, v) = unreachable()

"""
    Prometheus.@inprogress collector expr

Track the number of concurrent in-progress evaluations of `expr`. From the builtin
collectors this is only applicable to the [`Gauge`](@ref) -- the value of the gauge is
incremented with 1 when entering the section, and decremented with 1 when exiting the
section.

The expression, `expr`, can be a single expression (for example a function call), or a code
block (`begin`, `let`, etc), e.g.
```julia
Prometheus.@inprogress collector <expr>

Prometheus.@inprogress collector begin
    <expr>
end
```

It is also possible to apply the macro to a function *definition*, i.e.
```julia
Prometheus.@inprogress collector function track_this(args...)
    # function body
end
```
to track number of concurrent in-progress calls (covering all call sites).
"""
macro inprogress(collector, expr)
    return expr_gen(:inprogress, collector, expr)
end

at_inprogress_enter(gauge::Gauge) = inc(gauge)
at_inprogress_exit(gauge::Gauge) = dec(gauge)
at_inprogress_enter(::Collector) = unreachable()
at_inprogress_exit(::Collector) = unreachable()

function expr_gen(macroname, collector, code)
    if macroname === :time
        local cllctr, t0, val
        @gensym cllctr t0 val
        preamble = Expr[
            Expr(:(=), cllctr, esc(collector)),
            Expr(:(=), t0, Expr(:call, time)),
        ]
        postamble = Expr[
            Expr(:(=), val, Expr(:call, max, Expr(:call, -, Expr(:call, time), t0), 0.0)),
            Expr(:call, at_time, cllctr, val)
        ]
    elseif macroname === :inprogress
        local cllctr
        @gensym cllctr
        preamble = Expr[
            Expr(:(=), cllctr, esc(collector)),
            Expr(:call, at_inprogress_enter, cllctr),
        ]
        postamble = Expr[
            Expr(:call, at_inprogress_exit, cllctr)
        ]
    else
        unreachable()
    end
    local ret
    @gensym ret
    if Meta.isexpr(code, :function) || Base.is_short_function_def(code)
        @assert length(code.args) == 2
        fsig = esc(code.args[1])
        fbody = esc(code.args[2])
        return Expr(
            code.head, # might as well preserve :function or :(=)
            fsig,
            Expr(
                :block,
                preamble...,
                Expr(
                    :tryfinally,
                    Expr(:(=), ret, fbody),
                    Expr(:block, postamble...,),
                ),
                ret,
            ),
        )
    else
        return Expr(
            :block,
            preamble...,
            Expr(
                :tryfinally,
                Expr(:(=), ret, esc(code)),
                Expr(:block, postamble...,),
            ),
            ret,
        )
    end
end


####################################
# Family{<:Collector} <: Collector #
####################################

# https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels
# - Labels may contain ASCII letters, numbers, as well as underscores.
#   They must match the regex [a-zA-Z_][a-zA-Z0-9_]*.
# - Label names beginning with __ (two "_") are reserved for internal use.
function verify_label_name(label_name::String)
    label_name_regex = r"^[a-zA-Z_][a-zA-Z0-9_]*$"
    if !occursin(label_name_regex, label_name) || startswith(label_name, "__")
        throw(ArgumentError("label name \"$(label_name)\" is invalid"))
    end
    return label_name
end

struct LabelNames{N}
    label_names::NTuple{N, Symbol}
    function LabelNames(label_names::NTuple{N, Symbol}) where N
        for label_name in label_names
            verify_label_name(String(label_name))
        end
        return new{N}(label_names)
    end
end

# Tuple of strings
function LabelNames(label_names::NTuple{N, String}) where N
    return LabelNames(map(Symbol, label_names))
end

# NamedTuple-type or a (user defined) struct
function LabelNames(::Type{T}) where T
    return LabelNames(fieldnames(T))
end

struct LabelValues{N}
    label_values::NTuple{N, String}
end

function make_label_values(::LabelNames{N}, label_values::NTuple{N, String}) where N
    return LabelValues(label_values)
end

stringify(str::String) = str
stringify(str) = String(string(str))::String

# Heterogeneous tuple
function make_label_values(::LabelNames{N}, label_values::Tuple{Vararg{<:Any, N}}) where N
    return LabelValues(map(stringify, label_values)::NTuple{N, String})
end

# NamedTuple or a (user defined) struct
function make_label_values(label_names::LabelNames{N}, label_values) where N
    t::NTuple{N, String} = ntuple(N) do i
        stringify(getfield(label_values, label_names.label_names[i]))::String
    end
    return LabelValues{N}(t)
end

function Base.hash(l::LabelValues, h::UInt)
    h = hash(0x94a2d04ee9e5a55b, h) # hash("Prometheus.LabelValues") on Julia 1.9.3
    for v in l.label_values
        h = hash(v, h)
    end
    return h
end

function Base.:(==)(l1::LabelValues, l2::LabelValues)
    return l1.label_values == l2.label_values
end

struct Family{C, N} <: Collector
    metric_name::String
    help::String
    label_names::LabelNames{N}
    children::Dict{LabelValues{N}, C}
    lock::ReentrantLock

    function Family{C}(
            metric_name::String, help::String, label_names;
            registry::Union{CollectorRegistry, Nothing}=DEFAULT_REGISTRY,
        ) where {C}
        labels = LabelNames(label_names)
        N = length(labels.label_names)
        children = Dict{LabelValues{N}, C}()
        lock = ReentrantLock()
        family = new{C, N}(
            verify_metric_name(metric_name), help, labels, children, lock,
        )
        if registry !== nothing
            register(registry, family)
        end
        return family
    end
end

"""
    Prometheus.Family{C}(name, help, label_names; registry=DEFAULT_REGISTRY)

Create a labeled collector family with labels given by `label_names`. For every new set of
label values encountered a new collector of type `C <: Collector` will be created.

**Arguments**
 - `name :: String`: the name of the family metric.
 - `help :: String`: the documentation for the family metric.
 - `label_names`: the label names for the family. Label names can be given as either of the
   following (typically matching the methods label values will be given later, see
   [`Prometheus.labels`](@ref)):
    - a tuple of symbols or strings, e.g. `(:target, :status_code)` or
      `("target", "status_code")`
    - a named tuple type, e.g. `@NamedTuple{target::String, status_code::Int}` where the
      names are used as the label names
    - a custom struct type, e.g. `RequestLabels` defined as
      ```julia
      struct RequestLabels
          target::String
          status_code::Int
      end
      ```
      where the field names are used for the label names.

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
    "http_requests", "Number of HTTP requests", (:target, :status_code),
)

# Increment the counter for the labels `target="/api"` and `status_code=200`
Prometheus.inc(Prometheus.labels(counter_family, (target="/api", status_code=200)))
```
"""
Family{C}(::String, ::String, ::Any; kwargs...) where C

function metric_names(family::Family)
    return (family.metric_name, )
end

"""
    Prometheus.labels(family::Family{C}, label_values) where C

Return the collector of type `C` from the family corresponding to the labels given by
`label_values`.

Similarly to when creating the [`Family`](@ref), `label_values` can be given as either of
the following:
 - a tuple, e.g. `("/api", 200)`
 - a named tuple with names matching the label names, e.g.`(target="/api", status_code=200)`
 - a struct instance with field names matching the label names , e.g.
   `RequestLabels("/api", 200)`

All non-string values (e.g. `200` in the examples above) are stringified using `string`.

!!! note
    This method does an acquire/release of a lock, and a dictionary lookup, to find the
    collector matching the label names. For typical applications this overhead does not
    matter (below 100ns for some basic benchmarks) but it is safe to cache the returned
    collector if required.
"""
function labels(family::Family{C, N}, label_values) where {C, N}
    labels = make_label_values(family.label_names, label_values)::LabelValues{N}
    collector = @lock family.lock get!(family.children, labels) do
        # TODO: Avoid the re-verification of the metric name?
        C(family.metric_name, family.help; registry=nothing)
    end
    return collector
end

"""
    Prometheus.remove(family::Family, label_values)

Remove the collector corresponding to `label_values`. Effectively this resets the collector
since [`Prometheus.labels`](@ref) will recreate the collector when called with the same
label names.

Refer to [`Prometheus.labels`](@ref) for how to specify `label_values`.

!!! note
    This method invalidates cached collectors for the label names.
"""
function remove(family::Family{<:Any, N}, label_values) where N
    labels = make_label_values(family.label_names, label_values)::LabelValues{N}
    @lock family.lock delete!(family.children, labels)
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
    label_names = family.label_names
    @lock family.lock begin
        for (label_values, child) in family.children
            # collect!(...) the child, throw away the metric, but keep the samples
            child_metrics = collect!(resize!(buf, 0), child)
            length(child_metrics) != 1 && unreachable() # TODO: maybe this should be supported?
            child_metric = child_metrics[1]
            @assert(child_metric.type == type)
            # Unwrap and rewrap samples with the labels
            child_samples = child_metric.samples
            if child_samples isa Sample
                push!(samples, Sample(child_samples.suffix, label_names, label_values, child_samples.value))
            else
                @assert(child_samples isa Vector{Sample})
                for child_sample in child_samples
                    @assert(child_sample.label_values === nothing)
                    push!(samples, Sample(child_sample.suffix, label_names, label_values, child_sample.value))
                end
            end
        end
    end
    # Sort samples lexicographically by the labels
    sort!(samples; by = function(x)
        labels = x.label_values
        @assert(labels !== nothing)
        return labels.label_values
    end)
    push!(
        metrics,
        Metric(type, family.metric_name, family.help, samples),
    )
    return metrics
end


##############
# Exposition #
##############

struct Sample
    suffix::Union{String, Nothing} # e.g. _count or _sum
    label_names::Union{LabelNames, Nothing}
    label_values::Union{LabelValues, Nothing}
    value::Float64
end

struct Metric
    type::String
    metric_name::String
    help::String
    # TODO: Union{Tuple{Sample}, Vector{Sample}} would always make this iterable.
    samples::Union{Sample, Vector{Sample}}
end

function print_escaped(io::IO, help::String, esc)
    for c in help
        if c in esc
            c == '\n' ? print(io, "\\n") : print(io, '\\', c)
        else
            print(io, c)
        end
    end
    return
end

function expose_metric(io::IO, metric::Metric)
    print(io, "# HELP ", metric.metric_name, " ")
    print_escaped(io, metric.help, ('\\', '\n'))
    println(io)
    println(io, "# TYPE ", metric.metric_name, " ", metric.type)
    samples = metric.samples
    if samples isa Sample
        # Single sample, no labels
        @assert(samples.label_names === nothing)
        @assert(samples.label_values === nothing)
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
            label_names = sample.label_names
            label_values = sample.label_values
            @assert((label_names === nothing) === (label_values === nothing))
            if label_names !== nothing && label_values !== nothing
                first = true
                print(io, "{")
                for (name, value) in zip(label_names.label_names, label_values.label_values)
                    first || print(io, ",")
                    print(io, name, "=\"")
                    print_escaped(io, value, ('\\', '\"', '\n'))
                    print(io, "\"")
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
