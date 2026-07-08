# SPDX-License-Identifier: MIT

using HTTP: HTTP
using Prometheus: Prometheus
using Test: @test, @test_logs, @test_throws, @testset

@testset "Prometheus.CollectorRegistry" begin
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    # Default registry
    c = Prometheus.Counter("metric_name_counter", "A counter.")
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    @test_throws(
        Prometheus.ArgumentError("collector already contains a metric with the name \"metric_name_counter\""),
        Prometheus.Counter("metric_name_counter", "A counter."),
    )
    Prometheus.unregister(Prometheus.DEFAULT_REGISTRY, c)
    @test !(c in Prometheus.DEFAULT_REGISTRY.collectors)
    c2 = Prometheus.Counter("metric_name_counter", "A counter.")
    @test c2 in Prometheus.DEFAULT_REGISTRY.collectors
    # Provided registry
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Counter("metric_name_counter", "A counter."; registry = r)
    @test c in r.collectors
    @test !(c in Prometheus.DEFAULT_REGISTRY.collectors)
    # No registry on construction, register after
    c = Prometheus.Counter("metric_name_counter", "A counter."; registry = nothing)
    @test !(c in Prometheus.DEFAULT_REGISTRY.collectors)
    r = Prometheus.CollectorRegistry()
    Prometheus.register(r, c)
    @test c in r.collectors
    @test_throws(
        Prometheus.ArgumentError("collector already contains a metric with the name \"metric_name_counter\""),
        Prometheus.register(r, c),
    )
    # Summary reserves its base name too (in addition to _count/_sum): a
    # Counter with the same base name must collide.
    r = Prometheus.CollectorRegistry()
    Prometheus.Summary("shared_name", "help"; registry = r)
    @test_throws(
        Prometheus.ArgumentError("collector already contains a metric with the name \"shared_name\""),
        Prometheus.Counter("shared_name", "help"; registry = r),
    )
end

@testset "Prometheus.Counter" begin
    # Constructors and implicit registration
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    c = Prometheus.Counter("metric_name_counter", "A counter.")
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Counter("metric_name_counter", "A counter."; registry = r)
    @test c in r.collectors
    @test c.value == 0
    @test_throws(
        Prometheus.ArgumentError("metric name \"invalid-name\" is invalid"),
        Prometheus.Counter("invalid-name", "help"),
    )
    # Prometheus.inc(...)
    Prometheus.inc(c)
    @test c.value == 1
    Prometheus.inc(c, 0)
    @test c.value == 1
    Prometheus.inc(c, 2)
    @test c.value == 3
    @test_throws Prometheus.ArgumentError Prometheus.inc(c, -1)
    # Prometheus.collect(...)
    metrics = Prometheus.collect(c)
    @test length(metrics) == 1
    metric = metrics[1]
    @test metric.metric_name == c.metric_name
    @test metric.help == c.help
    @test length(metric.samples) == 2
    @test metric.samples[1].suffix === nothing
    @test metric.samples[1].value == c.value
    @test metric.samples[2].suffix == "_created"
    @test metric.samples[2].value == c.created
    # Prometheus.expose_metric(...): Prom text hides `_created`.
    @test sprint(Prometheus.expose_metric, metric) ==
        sprint(Prometheus.expose_io, r) ==
        """
        # HELP metric_name_counter A counter.
        # TYPE metric_name_counter counter
        metric_name_counter 3
        """
    # OpenMetrics includes `_created` and the `_total` sample suffix.
    om = sprint((io, m) -> Prometheus.expose_metric(io, m, Prometheus.OPENMETRICS_TEXT_100), metric)
    @test occursin("metric_name_counter_total 3\n", om)
    @test occursin("metric_name_counter_created $(c.created)\n", om)
end

@testset "Prometheus.Gauge" begin
    # Constructors and implicit registration
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    c = Prometheus.Gauge("metric_name_gauge", "A gauge.")
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Gauge("metric_name_gauge", "A gauge."; registry = r)
    @test c in r.collectors
    @test c.value == 0
    @test_throws(
        Prometheus.ArgumentError("metric name \"invalid-name\" is invalid"),
        Prometheus.Gauge("invalid-name", "help"),
    )
    # Prometheus.inc(...)
    Prometheus.inc(c)
    @test c.value == 1
    Prometheus.inc(c, 0)
    @test c.value == 1
    Prometheus.inc(c, 2)
    @test c.value == 3
    # Prometheus.dec(...)
    Prometheus.dec(c)
    @test c.value == 2
    Prometheus.dec(c, 1)
    @test c.value == 1
    # Prometheus.set_to_current_time(...)
    t0 = time()
    sleep(0.1)
    Prometheus.set_to_current_time(c)
    sleep(0.1)
    @test t0 < c.value < time()
    # Prometheus.set(...)
    Prometheus.set(c, 42)
    @test c.value == 42
    # Prometheus.collect(...)
    metrics = Prometheus.collect(c)
    @test length(metrics) == 1
    metric = metrics[1]
    @test metric.metric_name == c.metric_name
    @test metric.help == c.help
    @test metric.samples.value == c.value
    # Prometheus.expose_metric(...)
    @test sprint(Prometheus.expose_metric, metric) ==
        sprint(Prometheus.expose_io, r) ==
        """
        # HELP metric_name_gauge A gauge.
        # TYPE metric_name_gauge gauge
        metric_name_gauge 42
        """
end

@testset "Prometheus.Summary" begin
    # Constructors and implicit registration
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    c = Prometheus.Summary("metric_name_summary", "A summary.")
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Summary("metric_name_summary", "A summary."; registry = r)
    @test c in r.collectors
    @test c._count == 0
    @test c._sum == 0
    @test_throws(
        Prometheus.ArgumentError("metric name \"invalid-name\" is invalid"),
        Prometheus.Summary("invalid-name", "help"),
    )
    # Prometheus.observe(...)
    Prometheus.observe(c, 1)
    @test c._count == 1
    @test c._sum == 1
    Prometheus.observe(c, 10)
    @test c._count == 2
    @test c._sum == 11
    # Prometheus.collect(...)
    metrics = Prometheus.collect(c)
    @test length(metrics) == 1
    metric = metrics[1]
    @test metric.metric_name == c.metric_name
    @test metric.help == c.help
    @test length(metric.samples) == 3
    s1, s2, s3 = metric.samples[1], metric.samples[2], metric.samples[3]
    @test s1.suffix == "_count"
    @test s2.suffix == "_sum"
    @test s3.suffix == "_created"
    @test s1.label_values === nothing
    @test s2.label_values === nothing
    @test s3.label_values === nothing
    @test s1.value == 2
    @test s2.value == 11
    @test s3.value == c.created
    # Prometheus.expose_metric(...): Prom text hides `_created`.
    @test sprint(Prometheus.expose_metric, metric) ==
        sprint(Prometheus.expose_io, r) ==
        """
        # HELP metric_name_summary A summary.
        # TYPE metric_name_summary summary
        metric_name_summary_count 2
        metric_name_summary_sum 11
        """
    # OpenMetrics includes `_created`.
    om = sprint((io, m) -> Prometheus.expose_metric(io, m, Prometheus.OPENMETRICS_TEXT_100), metric)
    @test occursin("metric_name_summary_created $(c.created)\n", om)
end

@testset "Prometheus.Histogram" begin
    # Constructors and implicit registration
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    c = Prometheus.Histogram("metric_name_histogram", "A histogram.")
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Histogram("metric_name_histogram", "A histogram."; registry = r)
    @test c in r.collectors
    @test c.buckets == Prometheus.DEFAULT_BUCKETS
    @test c._count == 0
    @test c._sum == 0
    @test all(x -> x[] == 0, c.bucket_counters)
    @test_throws(
        Prometheus.ArgumentError("metric name \"invalid-name\" is invalid"),
        Prometheus.Histogram("invalid-name", "help"),
    )
    # Prometheus.observe(...)
    v1 = 0.9
    Prometheus.observe(c, v1)
    @test c._count == 1
    @test c._sum == v1
    for (ub, counter, known_count) in zip(c.buckets, c.bucket_counters, [zeros(Int, 9); ones(Int, 6)])
        @test counter[] == (v1 > ub ? 0 : 1) == known_count
    end
    v2 = 10v1
    Prometheus.observe(c, v2)
    @test c._count == 2
    @test c._sum == v1 + v2
    for (ub, counter, known_count) in zip(c.buckets, c.bucket_counters, [zeros(Int, 9); [1, 1, 1, 1]; [2, 2]])
        @test counter[] == ((v2 > ub && v1 > ub) ? 0 : v2 > ub ? 1 : 2) == known_count
    end
    # Prometheus.collect(...)
    r = Prometheus.CollectorRegistry()
    buckets = [1.0, 2.0, Inf]
    c = Prometheus.Histogram("metric_name_histogram", "A histogram."; buckets = buckets, registry = r)
    Prometheus.observe(c, 0.5)
    Prometheus.observe(c, 1.6)
    metrics = Prometheus.collect(c)
    @test length(metrics) == 1
    metric = metrics[1]
    @test metric.metric_name == c.metric_name
    @test metric.help == c.help
    @test length(metric.samples) == length(buckets) + 3
    s1, s2 = metric.samples[1], metric.samples[2]
    @test s1.suffix == "_count"
    @test s2.suffix == "_sum"
    @test s1.label_values === nothing
    @test s2.label_values === nothing
    @test s1.value == 2
    @test s2.value == 0.5 + 1.6
    @test metric.samples[end].suffix == "_created"
    @test metric.samples[end].value == c.created
    for (ub, counter, sample, known_count) in zip(c.buckets, c.bucket_counters, metric.samples[3:(end - 1)], [1, 2, 2])
        @test sample.suffix == "_bucket"
        @test (sample.label_names::Prometheus.LabelNames{1}).label_names === (:le,)
        @test (sample.label_values::Prometheus.LabelValues{1}).label_values ==
            (Prometheus.format_bucket_boundary(ub),)
        @test sample.value == counter[] == known_count
    end
    # Prometheus.expose_metric(...): Prom text hides `_created`.
    @test sprint(Prometheus.expose_metric, metric) ==
        sprint(Prometheus.expose_io, r) ==
        """
        # HELP metric_name_histogram A histogram.
        # TYPE metric_name_histogram histogram
        metric_name_histogram_count 2
        metric_name_histogram_sum 2.1
        metric_name_histogram_bucket{le="1.0"} 1
        metric_name_histogram_bucket{le="2.0"} 2
        metric_name_histogram_bucket{le="+Inf"} 2
        """
    # OpenMetrics includes `_created`.
    om = sprint((io, m) -> Prometheus.expose_metric(io, m, Prometheus.OPENMETRICS_TEXT_100), metric)
    @test occursin("metric_name_histogram_created $(c.created)\n", om)
end

@testset "Prometheus.LabelNames and Prometheus.LabelValues" begin
    @test_throws(
        Prometheus.ArgumentError("label name \"invalid-label\" is invalid"),
        Prometheus.LabelNames(("invalid-label",)),
    )
    # Custom hashing of values
    v1 = Prometheus.LabelValues(("foo", "bar"))
    v2 = Prometheus.LabelValues(("foo", "bar"))
    v3 = Prometheus.LabelValues(("foo", "baz"))
    @test hash(v1) == hash(v2)
    @test hash(v1) != hash(v3)
    @test v1 == v2
    @test v1 != v3
    @test isequal(v1, v2)
    @test !isequal(v1, v3)
end

@testset "Prometheus.Family{$(Collector)}" for Collector in (Prometheus.Counter, Prometheus.Gauge)
    # Constructors and implicit registration
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    c = Prometheus.Family{Collector}(
        "http_requests", "Number of HTTP requests.",
        ("endpoint", "status_code"),
    )
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Family{Collector}(
        "http_requests", "Number of HTTP requests.",
        ("endpoint", "status_code");
        registry = r,
    )
    @test c in r.collectors
    @test length(c.children) == 0
    @test_throws(
        Prometheus.ArgumentError("metric name \"invalid-name\" is invalid"),
        Prometheus.Family{Collector}("invalid-name", "help", ("label",)),
    )
    @test_throws(
        Prometheus.ArgumentError("label name \"invalid-label\" is invalid"),
        Prometheus.Family{Collector}("valid_name", "help", ("invalid-label",)),
    )
    # Prometheus.labels(...), Prometheus.remove(...), Prometheus.clear()
    l1 = ("/foo/", "200")
    l2 = ("/bar/", "404")
    @test Prometheus.labels(c, l1) === Prometheus.labels(c, l1) === c[l1]
    @test Prometheus.labels(c, l2) === Prometheus.labels(c, l2) === c[l2]
    @test length(c.children) == 2
    @test Prometheus.labels(c, l1).value == 0
    @test Prometheus.labels(c, l2).value == 0
    Prometheus.remove(c, l1)
    @test length(c.children) == 1
    Prometheus.clear(c)
    @test length(c.children) == 0
    # Prometheus.inc(...)
    Prometheus.inc(Prometheus.labels(c, l1))
    Prometheus.inc(Prometheus.labels(c, l2))
    @test Prometheus.labels(c, l1).value == 1
    @test Prometheus.labels(c, l2).value == 1
    Prometheus.inc(Prometheus.labels(c, l1), 2)
    Prometheus.inc(Prometheus.labels(c, l2), 2)
    @test Prometheus.labels(c, l1).value == 3
    @test Prometheus.labels(c, l2).value == 3
    # Prometheus.collect(...)
    metrics = Prometheus.collect(c)
    @test length(metrics) == 1
    metric = metrics[1]
    @test metric.metric_name == c.metric_name
    @test metric.help == c.help
    # Counter emits an extra `_created` sample per child; Gauge does not.
    samples_per_child = Collector === Prometheus.Counter ? 2 : 1
    @test length(metric.samples) == 2 * samples_per_child
    s1 = metric.samples[1]
    s2 = metric.samples[1 + samples_per_child]
    @test s1.label_values.label_values == ("/bar/", "404")
    @test s2.label_values.label_values == ("/foo/", "200")
    @test s1.value == 3
    @test s2.value == 3
    # Prometheus.expose_metric(...): Prom text hides `_created`, so output is
    # the same regardless of Collector type.
    type = Collector === Prometheus.Counter ? "counter" : "gauge"
    @test sprint(Prometheus.expose_metric, metric) ==
        sprint(Prometheus.expose_io, r) ==
        """
        # HELP http_requests Number of HTTP requests.
        # TYPE http_requests $(type)
        http_requests{endpoint="/bar/",status_code="404"} 3
        http_requests{endpoint="/foo/",status_code="200"} 3
        """
end

@testset "Prometheus.@time gauge::Gauge" begin
    # NOTE: `sleep(x)` is not guaranteed to sleep for at least `x` seconds
    # (Julia's libuv timer can fire slightly early due to clock resolution),
    # and on loaded CI runners a sleep can overshoot significantly. Assertions
    # here use a loose lower bound and no upper bound; the intent is only to
    # verify that `@time` recorded a plausible elapsed time — not the exact
    # duration.
    gauge = Prometheus.Gauge("call_time_last", "Time of last call"; registry = nothing)
    Prometheus.@time gauge sleep(0.1)
    @test gauge.value > 0.05
    Prometheus.@time gauge let
        sleep(0.1)
    end
    @test gauge.value > 0.05
    Prometheus.@time gauge f() = sleep(0.1)
    @sync begin
        @async f()
        @async f()
    end
    @test gauge.value > 0.05
    Prometheus.@time gauge function g()
        sleep(0.1)
    end
    @sync begin
        @async g()
        @async g()
    end
    @test gauge.value > 0.05
end

@testset "Prometheus.@time collector::$(Collector)" for Collector in (Prometheus.Histogram, Prometheus.Summary)
    ishist = Collector === Prometheus.Histogram
    buckets = [1.0, Inf]
    collector = Collector(
        "call_time", "Time of calls";
        (ishist ? (; buckets = buckets) : (;))...,
        registry = nothing,
    )
    # NOTE: see the @time gauge testset above for why we don't assert tight
    # bounds on _sum. The `_count` and `bucket_counters` assertions here are
    # the semantic checks (right number of observations, correct bucketing);
    # `_sum` is only sanity-checked with a loose lower bound.
    Prometheus.@time collector sleep(0.1)
    @test collector._sum > 0.05
    @test collector._count == 1
    ishist && @test (x -> x[]).(collector.bucket_counters) == [1, 1]
    Prometheus.@time collector let
        sleep(0.1)
    end
    @test collector._sum > 0.1
    @test collector._count == 2
    ishist && @test (x -> x[]).(collector.bucket_counters) == [2, 2]
    Prometheus.@time collector f() = sleep(0.1)
    @sync begin
        @async f()
        @async f()
    end
    @test collector._sum > 0.2
    @test collector._count == 4
    ishist && @test (x -> x[]).(collector.bucket_counters) == [4, 4]
    Prometheus.@time collector function g()
        sleep(0.1)
    end
    @sync begin
        @async g()
        @async g()
    end
    @test collector._sum > 0.3
    @test collector._count == 6
    ishist && @test (x -> x[]).(collector.bucket_counters) == [6, 6]
    if ishist
        # Use a value with enough headroom over the 1.0 bucket boundary that
        # a sleep undershoot cannot push us into the wrong bucket.
        Prometheus.@time collector sleep(2.0)
        @test (x -> x[]).(collector.bucket_counters) == [6, 7]
    end
end

@testset "Prometheus.@inprogress gauge::Gauge" begin
    gauge = Prometheus.Gauge("calls_inprogres", "Number of calls in progress"; registry = nothing)
    Prometheus.@inprogress gauge sleep(0.01)
    @test gauge.value == 0.0
    Prometheus.@inprogress gauge let
        sleep(0.01)
    end
    @test gauge.value == 0.0
    Prometheus.@inprogress gauge f() = sleep(0.01)
    @sync begin
        @async f()
        @async f()
    end
    @test gauge.value == 0.0
    Prometheus.@inprogress gauge function g()
        sleep(0.01)
    end
    @sync begin
        @async g()
        @async g()
    end
    @test gauge.value == 0.0
    # Concurrency tests
    @sync begin
        tsks = Vector{Task}(undef, 100)
        for i in 1:100
            tsk = @async begin
                0 <= gauge.value <= 100 || error()
                Prometheus.@inprogress gauge sleep(1 + rand())
            end
            tsks[i] = tsk
        end
        # Make sure all tasks have started before testing the value
        while any(!istaskstarted, tsks)
            sleep(0.1)
        end
        @test gauge.value == 100
    end
    @test gauge.value == 0
end

# TODO: Document interface and test it
@testset "Custom collector with @time/@inprogress" begin
    # struct Coll <: Prometheus.Collector end
    @test_throws Prometheus.ArgumentError Prometheus.expr_gen(:unknown, nothing, nothing)
end

@testset "Prometheus.Family{$Collector}" for Collector in (Prometheus.Histogram, Prometheus.Summary)
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Family{Collector}(
        "http_request_time", "Time to process requests.",
        ("endpoint", "status_code");
        (Collector === Prometheus.Histogram ? (; buckets = [2.0, Inf]) : (;))...,
        registry = r,
    )
    @test c in r.collectors
    @test length(c.children) == 0
    # Prometheus.inc(...)
    l1 = ("/foo/", "200")
    l2 = ("/bar/", "404")
    @test Prometheus.labels(c, l1) === Prometheus.labels(c, l1) === c[l1]
    @test Prometheus.labels(c, l2) === Prometheus.labels(c, l2) === c[l2]
    @test length(c.children) == 2
    @test Prometheus.labels(c, l1)._count == 0
    @test Prometheus.labels(c, l1)._sum == 0
    @test Prometheus.labels(c, l2)._count == 0
    @test Prometheus.labels(c, l2)._sum == 0
    Prometheus.observe(Prometheus.labels(c, l1), 1.2)
    Prometheus.observe(Prometheus.labels(c, l2), 2.1)
    @test Prometheus.labels(c, l1)._count == 1
    @test Prometheus.labels(c, l1)._sum == 1.2
    @test Prometheus.labels(c, l2)._count == 1
    @test Prometheus.labels(c, l2)._sum == 2.1
    Prometheus.observe(Prometheus.labels(c, l1), 3.4)
    Prometheus.observe(Prometheus.labels(c, l2), 4.3)
    @test Prometheus.labels(c, l1)._count == 2
    @test Prometheus.labels(c, l1)._sum == 4.6
    @test Prometheus.labels(c, l2)._count == 2
    @test Prometheus.labels(c, l2)._sum == 6.4
    # Prometheus.collect(...)
    metrics = Prometheus.collect(c)
    @test length(metrics) == 1
    metric = metrics[1]
    @test metric.metric_name == c.metric_name
    @test metric.help == c.help
    if Collector === Prometheus.Histogram
        buckets = Prometheus.labels(c, l1).buckets
        @test length(buckets) == length(Prometheus.labels(c, l2).buckets)
        # Per child: _count, _sum, N buckets, _created.
        per_child = length(buckets) + 3
        @test length(metric.samples) == 2 * per_child
        # _count and _sum samples (l2 first — sorted by label_values ascending)
        s_count_l2 = metric.samples[1]
        s_sum_l2 = metric.samples[2]
        s_count_l1 = metric.samples[per_child + 1]
        s_sum_l1 = metric.samples[per_child + 2]
        @test s_count_l2.label_values.label_values == s_sum_l2.label_values.label_values == ("/bar/", "404")
        @test s_count_l1.label_values.label_values == s_sum_l1.label_values.label_values == ("/foo/", "200")
        @test s_count_l2.value == 2   # _count
        @test s_sum_l2.value == 6.4 # _sum
        @test s_count_l1.value == 2   # _count
        @test s_sum_l1.value == 4.6 # _sum
        # {le} samples
        for (ls, subrange) in ((l2, 3:(2 + length(buckets))), (l1, (per_child + 3):(per_child + 2 + length(buckets))))
            for (ub, counter, sample) in zip(buckets, Prometheus.labels(c, ls).bucket_counters, metric.samples[subrange])
                @test sample.suffix == "_bucket"
                @test (sample.label_names::Prometheus.LabelNames{3}).label_names ===
                    (:endpoint, :status_code, :le)
                @test (sample.label_values::Prometheus.LabelValues{3}).label_values ==
                    (ls..., Prometheus.format_bucket_boundary(ub))
                @test sample.value == counter[]
            end
        end
    else # Collector === Prometheus.Summary
        # Per child: _count, _sum, _created.
        @test length(metric.samples) == 6
        s1, s2, s3, s4, s5, s6 = metric.samples
        @test s1.label_values.label_values == s2.label_values.label_values ==
            s3.label_values.label_values == ("/bar/", "404")
        @test s4.label_values.label_values == s5.label_values.label_values ==
            s6.label_values.label_values == ("/foo/", "200")
        @test s1.value == 2   # _count
        @test s2.value == 6.4 # _sum
        @test s3.suffix == "_created"
        @test s4.value == 2   # _count
        @test s5.value == 4.6 # _sum
        @test s6.suffix == "_created"
        # Prometheus.expose_metric(...)
        @test sprint(Prometheus.expose_metric, metric) ==
            sprint(Prometheus.expose_io, r) ==
            """
            # HELP http_request_time Time to process requests.
            # TYPE http_request_time summary
            http_request_time_count{endpoint="/bar/",status_code="404"} 2
            http_request_time_sum{endpoint="/bar/",status_code="404"} 6.4
            http_request_time_count{endpoint="/foo/",status_code="200"} 2
            http_request_time_sum{endpoint="/foo/",status_code="200"} 4.6
            """
    end
end

@testset "Prometheus.Family{Histogram} bucket ordering (default buckets)" begin
    # Regression test: buckets must be emitted with `le` in ascending numeric
    # order, not lexicographic string order. With default buckets, "10.0" sorts
    # before "2.5" as strings, so a naive sort violates the Prometheus text
    # format ("buckets MUST appear in increasing numerical order of `le`") and
    # produces non-monotonic cumulative counts.
    r = Prometheus.CollectorRegistry()
    fam = Prometheus.Family{Prometheus.Histogram}(
        "req_latency_seconds", "Request latency", ("endpoint",); registry = r,
    )
    # Three observations with distinct bucket footprints in the misordered range
    Prometheus.observe(Prometheus.labels(fam, ("/api",)), 1.5)
    Prometheus.observe(Prometheus.labels(fam, ("/api",)), 3.0)
    Prometheus.observe(Prometheus.labels(fam, ("/api",)), 6.0)
    metric = only(Prometheus.collect(fam))
    bucket_samples = filter(s -> s.suffix == "_bucket", metric.samples)
    le_values = [parse(Float64, s.label_values.label_values[end]) for s in bucket_samples]
    @test issorted(le_values)
    counts = [s.value for s in bucket_samples]
    @test issorted(counts)
    # And with multiple label sets: the outer label ordering must still hold
    Prometheus.observe(Prometheus.labels(fam, ("/other",)), 0.5)
    metric = only(Prometheus.collect(fam))
    endpoints = unique(s.label_values.label_values[1] for s in metric.samples)
    @test endpoints == ["/api", "/other"]
    for endpoint in endpoints
        subset = filter(s -> s.suffix == "_bucket" && s.label_values.label_values[1] == endpoint, metric.samples)
        les = [parse(Float64, s.label_values.label_values[end]) for s in subset]
        @test issorted(les)
        @test issorted([s.value for s in subset])
    end
end

@testset "Label types for Prometheus.Family{C}" begin
    struct RequestLabels
        target::String
        status_code::Int
    end
    for fam in (
            # Constructor with NTuple{N, String} names
            Prometheus.Family{Prometheus.Counter}(
                "http_requests", "Total number of HTTP requests", ("target", "status_code");
                registry = nothing,
            ),
            # Constructor with NTuple{N, Symbol} names
            Prometheus.Family{Prometheus.Counter}(
                "http_requests", "Total number of HTTP requests", (:target, :status_code);
                registry = nothing,
            ),
            # Constructor with NamedTuple type
            Prometheus.Family{Prometheus.Counter}(
                "http_requests", "Total number of HTTP requests",
                @NamedTuple{target::String, status_code::Int};
                registry = nothing,
            ),
            # Constructor with custom struct
            Prometheus.Family{Prometheus.Counter}(
                "http_requests", "Total number of HTTP requests", RequestLabels;
                registry = nothing,
            ),
        )
        @test Prometheus.labels(fam, ("/api", "200")) ===
            fam[("/api", "200")] ===
            Prometheus.labels(fam, ("/api", 200)) ===
            fam[("/api", 200)] ===
            Prometheus.labels(fam, (target = "/api", status_code = "200")) ===
            fam[(target = "/api", status_code = "200")] ===
            Prometheus.labels(fam, (target = "/api", status_code = 200)) ===
            fam[(target = "/api", status_code = 200)] ===
            Prometheus.labels(fam, (status_code = "200", target = "/api")) ===
            fam[(status_code = "200", target = "/api")] ===
            Prometheus.labels(fam, (status_code = 200, target = "/api")) ===
            fam[(status_code = 200, target = "/api")] ===
            Prometheus.labels(fam, RequestLabels("/api", 200)) ===
            fam[RequestLabels("/api", 200)]
    end
end

@testset "Prometheus.GCCollector" begin
    r = Prometheus.CollectorRegistry()
    c = Prometheus.GCCollector(; registry = r)
    @test c in r.collectors
    # Record before and after stats and test that the metrics are in between
    old_stats = Base.gc_num()
    x = zeros(1024^2); x = nothing; GC.gc(); GC.gc()
    metrics = Prometheus.collect(c)
    x = zeros(1024^2); x = nothing; GC.gc(); GC.gc()
    new_stats = Base.gc_num()
    @test length(metrics) == 6
    gc_alloc_total = metrics[findfirst(x -> x.metric_name == "julia_gc_alloc_total", metrics)]
    @test old_stats.bigalloc <= gc_alloc_total.samples[1].value <= new_stats.bigalloc
    @test old_stats.malloc <= gc_alloc_total.samples[2].value <= new_stats.malloc
    @test old_stats.poolalloc <= gc_alloc_total.samples[3].value <= new_stats.poolalloc
    @test old_stats.realloc <= gc_alloc_total.samples[4].value <= new_stats.realloc
    gc_free_total = metrics[findfirst(x -> x.metric_name == "julia_gc_free_total", metrics)]
    @test old_stats.freecall <= gc_free_total.samples.value <= new_stats.freecall
    gc_alloc_bytes_total = metrics[findfirst(x -> x.metric_name == "julia_gc_alloc_bytes_total", metrics)]
    @test Base.gc_total_bytes(old_stats) <= gc_alloc_bytes_total.samples.value <= Base.gc_total_bytes(new_stats)
    gc_seconds_total = metrics[findfirst(x -> x.metric_name == "julia_gc_seconds_total", metrics)]
    @test old_stats.total_time / 10^9 <= gc_seconds_total.samples.value <= new_stats.total_time / 10^9
    # Prometheus.expose_metric(...)
    str = sprint(Prometheus.expose_metric, gc_alloc_total)
    @test occursin(
        r"""
        # HELP julia_gc_alloc_total Total number of allocations \(calls to malloc, realloc, etc\)
        # TYPE julia_gc_alloc_total counter
        julia_gc_alloc_total{type="bigalloc"} \d+
        julia_gc_alloc_total{type="malloc"} \d+
        julia_gc_alloc_total{type="poolalloc"} \d+
        julia_gc_alloc_total{type="realloc"} \d+
        """,
        sprint(Prometheus.expose_metric, gc_alloc_total),
    )
end

@testset "Prometheus.ProcessCollector" begin
    r = Prometheus.CollectorRegistry()
    c = Prometheus.ProcessCollector(; registry = r)
    @test c in r.collectors
    metrics = Prometheus.collect(c)
    procfs_available = c.system_boot_time > 0
    if procfs_available
        # Prometheus.expose_metric(...)
        str = sprint(Prometheus.expose_io, r)
        @test occursin(
            r"""
            # HELP process_cpu_seconds_total Total CPU time \(user and system mode\) in seconds.
            # TYPE process_cpu_seconds_total counter
            process_cpu_seconds_total{mode="system"} [0-9\.]+
            process_cpu_seconds_total{mode="user"} [0-9\.]+
            # HELP process_io_rchar_bytes_total Total number of bytes read in bytes \(rchar from /proc/\[pid\]/io\).
            # TYPE process_io_rchar_bytes_total counter
            process_io_rchar_bytes_total \d+
            # HELP process_io_read_bytes_total Total number of bytes read from the file system \(read_bytes from /proc/\[pid\]/io\).
            # TYPE process_io_read_bytes_total counter
            process_io_read_bytes_total \d+
            # HELP process_io_syscr_total Total number of read I/O operations \(syscalls\) \(syscr from /proc/\[pid\]/io\).
            # TYPE process_io_syscr_total counter
            process_io_syscr_total \d+
            # HELP process_io_syscw_total Total number of write I/O operations \(syscalls\) \(syscw from /proc/\[pid\]/io\).
            # TYPE process_io_syscw_total counter
            process_io_syscw_total \d+
            # HELP process_io_wchar_bytes_total Total number of bytes written in bytes \(wchar from /proc/\[pid\]/io\).
            # TYPE process_io_wchar_bytes_total counter
            process_io_wchar_bytes_total \d+
            # HELP process_io_write_bytes_total Total number of bytes written to the file system \(write_bytes from /proc/\[pid\]/io\).
            # TYPE process_io_write_bytes_total counter
            process_io_write_bytes_total \d+
            # HELP process_open_fds Number of open file descriptors.
            # TYPE process_open_fds gauge
            process_open_fds \d+
            # HELP process_resident_memory_bytes Resident memory size \(RSS\) in bytes.
            # TYPE process_resident_memory_bytes gauge
            process_resident_memory_bytes \d+
            # HELP process_start_time_seconds Start time since unix epoch in seconds.
            # TYPE process_start_time_seconds gauge
            process_start_time_seconds .*
            # HELP process_virtual_memory_bytes Virtual memory size in bytes.
            # TYPE process_virtual_memory_bytes gauge
            process_virtual_memory_bytes \d+
            """,
            sprint(Prometheus.expose_io, r),
        )
    else
        @test isempty(metrics)
    end
    # Test that pid function works
    procc = Prometheus.ProcessCollector(() -> getpid(); registry = nothing)
    metrics = Prometheus.collect(procc)
    if procfs_available
        @test length(metrics) > 0
    else
        @test length(metrics) == 0
    end
    if procfs_available
        # Not a pid
        empty!(Prometheus.DEFAULT_REGISTRY.collectors)
        procc = Prometheus.ProcessCollector(() -> "notapid")
        empty!(Prometheus.DEFAULT_REGISTRY.collectors)
        metrics = @test_logs (:error, r"/proc/notapid/ does not exist") Prometheus.collect(procc)
        @test length(metrics) == 0
        # Pid function error
        empty!(Prometheus.DEFAULT_REGISTRY.collectors)
        procc = Prometheus.ProcessCollector(() -> error())
        metrics = @test_logs (:error, r"pid from the lambda") Prometheus.collect(procc)
        @test length(metrics) == 0
    end
end

@testset "Character escaping in exposition" begin
    counter = Prometheus.Family{Prometheus.Counter}(
        "counter_name", "Help with slash \\ and newline \n", ("label_name",);
        registry = nothing,
    )
    Prometheus.inc(Prometheus.labels(counter, ("backslash \\, quote \", newline \n",)))
    metric = first(Prometheus.collect(counter))
    @test sprint(Prometheus.expose_metric, metric) ==
        """
        # HELP counter_name Help with slash \\\\ and newline \\n
        # TYPE counter_name counter
        counter_name{label_name="backslash \\\\, quote \\", newline \\n"} 1
        """
end

@testset "Value formatting at the Int/Float64 boundary" begin
    # Integer-valued floats within Int range are printed without a trailing
    # `.0` (matches Go's `%g` shortest form). Values outside Int range fall
    # back to the float representation instead of throwing InexactError.
    gauge = Prometheus.Gauge("g", "help"; registry = nothing)
    # Normal integer-valued case.
    Prometheus.set(gauge, 42)
    @test occursin("g 42\n", sprint(Prometheus.expose_metric, only(Prometheus.collect(gauge))))
    # Fractional case.
    Prometheus.set(gauge, 1.5)
    @test occursin("g 1.5\n", sprint(Prometheus.expose_metric, only(Prometheus.collect(gauge))))
    # Beyond typemax(Int64) — must not throw, must fall through to float.
    Prometheus.set(gauge, 1.0e19)
    @test occursin("g 1.0e19\n", sprint(Prometheus.expose_metric, only(Prometheus.collect(gauge))))
end

@testset "Prometheus.expose(::Union{String, IO})" begin
    r = Prometheus.DEFAULT_REGISTRY
    empty!(r.collectors)
    Prometheus.inc(Prometheus.Counter("prom_counter", "Counting things"; registry = r))
    Prometheus.set(Prometheus.Gauge("prom_gauge", "Gauging things"; registry = r), 1.2)
    mktempdir() do dir
        default = joinpath(dir, "default.prom")
        Prometheus.expose(default)
        reg = joinpath(dir, "reg.prom")
        Prometheus.expose(reg, r)
        default_io = IOBuffer()
        Prometheus.expose(default_io)
        reg_io = IOBuffer()
        Prometheus.expose(reg_io, r)
        @test read(default, String) ==
            read(reg, String) ==
            String(take!(default_io)) ==
            String(take!(reg_io)) ==
            """
            # HELP prom_counter Counting things
            # TYPE prom_counter counter
            prom_counter 1
            # HELP prom_gauge Gauging things
            # TYPE prom_gauge gauge
            prom_gauge 1.2
            """
    end
end

@testset "Prometheus.expose(::HTTP.Stream)" begin
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    Prometheus.inc(Prometheus.Counter("prom_counter", "Counting things"))
    Prometheus.set(Prometheus.Gauge("prom_gauge", "Gauging things"), 1.2)
    iob = IOBuffer()
    Prometheus.expose(iob)
    reference_output = String(take!(iob))
    Prometheus.expose_io(iob, Prometheus.DEFAULT_REGISTRY, Prometheus.OPENMETRICS_TEXT_100)
    openmetrics_output = String(take!(iob))
    # Spin up the server
    server = HTTP.listen!(8123) do http
        if http.message.target == "/metrics/default"
            return Prometheus.expose(http)
        elseif http.message.target == "/metrics/reg"
            return Prometheus.expose(http, Prometheus.DEFAULT_REGISTRY)
        elseif http.message.target == "/metrics/nogzip"
            return Prometheus.expose(http; compress = false)
        else
            HTTP.setstatus(http, 404)
            HTTP.startwrite(http)
        end
    end
    # Normal requests
    r_default = HTTP.request("GET", "http://localhost:8123/metrics/default")
    r_ref = HTTP.request("GET", "http://localhost:8123/metrics/reg")
    @test String(r_default.body) == String(r_ref.body) == reference_output
    # Vary header is set so shared caches key on Accept and Accept-Encoding.
    @test HTTP.header(r_default, "Vary") == "Accept, Accept-Encoding"
    # HEAD
    @test isempty(HTTP.request("HEAD", "http://localhost:8123/metrics/default").body)
    # POST (no filtering in the server above)
    r_post = HTTP.request("POST", "http://localhost:8123/metrics/default")
    @test String(r_post.body) == reference_output
    # Bad URI
    r_bad = HTTP.request("GET", "http://localhost:8123"; status_exception = false)
    @test r_bad.status == 404
    # Compression
    for enc in ("gzip", "br, compress, gzip", "br;q=1.0, gzip;q=0.8, *;q=0.1")
        r_gzip = HTTP.request(
            "GET", "http://localhost:8123/metrics/default", ["Accept-Encoding" => enc]
        )
        @test HTTP.header(r_gzip, "Content-Encoding") == "gzip"
        @test String(r_gzip.body) == reference_output # HTTP.jl decompresses gzip
        r_nogzip = HTTP.request(
            "GET", "http://localhost:8123/metrics/nogzip", ["Accept-Encoding" => enc]
        )
        @test HTTP.header(r_nogzip, "Content-Encoding", nothing) === nothing
        @test String(r_nogzip.body) == reference_output
    end
    # Client that does not accept gzip. "identity" is the spec token for "no encoding"
    r_nogzip = HTTP.request(
        "GET", "http://localhost:8123/metrics/default", ["Accept-Encoding" => "identity"],
    )
    @test HTTP.header(r_nogzip, "Content-Encoding", nothing) === nothing
    @test String(r_nogzip.body) == reference_output
    # An empty Accept-Encoding also means "no encoding acceptable" (RFC 9110), so the
    # server must not gzip.
    r_empty = HTTP.request(
        "GET", "http://localhost:8123/metrics/default", ["Accept-Encoding" => ""],
    )
    @test HTTP.header(r_empty, "Content-Encoding", nothing) === nothing

    # The body matches on both versions: HTTP.jl transparently decompresses gzip, so
    # this stays a plain @test
    @test String(r_empty.body) == reference_output
    # Server side content negotiation (RFC 9110 §12.5.3), independent of the HTTP.jl
    # client behavior. Both an empty Accept-Encoding and "identity" mean "no gzip".
    @test Prometheus.gzip_accepted("") == false
    @test Prometheus.gzip_accepted("identity") == false
    @test Prometheus.gzip_accepted("gzip") == true
    @test Prometheus.gzip_accepted("br, deflate") == false
    @test Prometheus.gzip_accepted("br;q=1.0, gzip;q=0.8, *;q=0.1") == true
    # A qvalue of zero is an explicit refusal and must not be treated as acceptance.
    @test Prometheus.gzip_accepted("gzip;q=0") == false
    @test Prometheus.gzip_accepted("identity, gzip;q=0") == false
    @test Prometheus.gzip_accepted("gzip; q=0.0") == false
    # * wildcard matches gzip when gzip is not explicitly listed.
    @test Prometheus.gzip_accepted("*") == true
    @test Prometheus.gzip_accepted("br, *") == true
    @test Prometheus.gzip_accepted("*;q=0") == false
    # An explicit gzip entry takes precedence over the wildcard.
    @test Prometheus.gzip_accepted("gzip;q=0, *;q=1") == false

    # Server-side Accept negotiation (RFC 9110 §12.5.1). Picks the highest-q
    # supported format; returns `nothing` (→ HTTP 406) if none is acceptable.
    let PT = Prometheus.PROM_TEXT_004, OM = Prometheus.OPENMETRICS_TEXT_100
        # Absent/empty Accept => any acceptable, default to Prom text.
        @test Prometheus.pick_format("") === PT
        # Direct hits on Prom text.
        @test Prometheus.pick_format("text/plain") === PT
        @test Prometheus.pick_format("text/plain; version=0.0.4") === PT
        @test Prometheus.pick_format("text/plain;version=0.0.4;q=0.25") === PT
        # Direct hits on OpenMetrics.
        @test Prometheus.pick_format("application/openmetrics-text") === OM
        @test Prometheus.pick_format("application/openmetrics-text; version=1.0.0") === OM
        # Wildcards match; tuple order breaks the tie in favor of Prom text.
        @test Prometheus.pick_format("*/*") === PT
        @test Prometheus.pick_format("text/*") === PT
        # A non-matching version param is a mismatch.
        @test Prometheus.pick_format("text/plain; version=1.0.0") === nothing
        @test Prometheus.pick_format("application/openmetrics-text; version=2.0.0") === nothing
        # OpenMetrics 0.0.1 isn't matched (we emit 1.0.0), but the same request
        # in the Prom-3 default Accept lists 1.0.0 too — that one wins.
        @test Prometheus.pick_format("application/openmetrics-text;version=0.0.1") === nothing
        # A modern Prometheus scrape prefers OpenMetrics (q=0.75) over our text (q=0.25).
        @test Prometheus.pick_format(
            "application/openmetrics-text;version=1.0.0;q=0.75," *
                "application/openmetrics-text;version=0.0.1;q=0.5," *
                "text/plain;version=0.0.4;q=0.25,*/*;q=0.1",
        ) === OM
        # q=0 is explicit refusal; RFC precedence means the more specific range wins.
        @test Prometheus.pick_format("text/plain;q=0") === nothing
        # PT is explicitly excluded; the `*/*` fallback still matches OM.
        @test Prometheus.pick_format("text/plain;version=0.0.4;q=0, */*") === OM
        @test Prometheus.pick_format("*/*;q=0") === nothing
        # A protobuf-only scraper gets no match.
        @test Prometheus.pick_format(
            "application/vnd.google.protobuf; " *
                "proto=io.prometheus.client.MetricFamily; encoding=delimited",
        ) === nothing
    end

    # End-to-end: a scraper that only accepts a format we don't support (protobuf) gets 406.
    r_406 = HTTP.request(
        "GET", "http://localhost:8123/metrics/default",
        [
            "Accept" => "application/vnd.google.protobuf; " *
                "proto=io.prometheus.client.MetricFamily; encoding=delimited",
        ];
        status_exception = false,
    )
    @test r_406.status == 406
    @test occursin("406 Not Acceptable", String(r_406.body))
    # Vary header is also set on the 406 response — it, too, is a function of Accept.
    @test HTTP.header(r_406, "Vary") == "Accept, Accept-Encoding"
    # A Prometheus 3-style scrape prefers OpenMetrics: response Content-Type
    # and body switch to OpenMetrics.
    r_om = HTTP.request(
        "GET", "http://localhost:8123/metrics/default",
        [
            "Accept" => "application/openmetrics-text;version=1.0.0;q=0.75," *
                "text/plain;version=0.0.4;q=0.25,*/*;q=0.1",
        ],
    )
    @test r_om.status == 200
    @test HTTP.header(r_om, "Content-Type") ==
        "application/openmetrics-text; version=1.0.0; charset=utf-8"
    @test String(r_om.body) == openmetrics_output
    # Body-level OpenMetrics differences vs Prom text: counter `_total` suffix + `# EOF` trailer.
    @test occursin("prom_counter_total 1", openmetrics_output)
    @test endswith(openmetrics_output, "# EOF\n")
    @test !occursin("_total", reference_output)
    @test !occursin("# EOF", reference_output)
    # A client that only accepts Prom text gets Prom text (matching Content-Type
    # and body, no `# EOF` trailer).
    r_pt_only = HTTP.request(
        "GET", "http://localhost:8123/metrics/default",
        ["Accept" => "text/plain;version=0.0.4"],
    )
    @test r_pt_only.status == 200
    @test HTTP.header(r_pt_only, "Content-Type") ==
        "text/plain; version=0.0.4; charset=utf-8"
    @test String(r_pt_only.body) == reference_output
    # Reversed q-values: Prom text q=0.9, OpenMetrics q=0.5 — Prom text wins.
    r_pt_pref = HTTP.request(
        "GET", "http://localhost:8123/metrics/default",
        [
            "Accept" => "text/plain;version=0.0.4;q=0.9," *
                "application/openmetrics-text;version=1.0.0;q=0.5",
        ],
    )
    @test r_pt_pref.status == 200
    @test HTTP.header(r_pt_pref, "Content-Type") ==
        "text/plain; version=0.0.4; charset=utf-8"
    @test String(r_pt_pref.body) == reference_output
    # Equal q-values on both formats — tuple-order tiebreak resolves to Prom text.
    r_tie = HTTP.request(
        "GET", "http://localhost:8123/metrics/default",
        [
            "Accept" => "application/openmetrics-text;version=1.0.0;q=0.5," *
                "text/plain;version=0.0.4;q=0.5",
        ],
    )
    @test r_tie.status == 200
    @test HTTP.header(r_tie, "Content-Type") ==
        "text/plain; version=0.0.4; charset=utf-8"
    @test String(r_tie.body) == reference_output

    # Counter already named with `_total` (Prom text idiom) must not double-suffix:
    # HELP/TYPE strip `_total`, sample line stays `foo_total`. Spec says the
    # counter MetricFamily name MUST NOT include `_total`.
    let r = Prometheus.CollectorRegistry()
        Prometheus.inc(Prometheus.Counter("http_requests_total", "Total HTTP requests"; registry = r))
        pt = sprint(Prometheus.expose_io, r)
        om = sprint((io, r) -> Prometheus.expose_io(io, r, Prometheus.OPENMETRICS_TEXT_100), r)
        # Prom text: name is emitted as-given; sample line matches the family name.
        @test occursin("# TYPE http_requests_total counter", pt)
        @test occursin("http_requests_total 1", pt)
        # OpenMetrics: HELP/TYPE lose the `_total`, sample line keeps it (no doubling).
        @test occursin("# TYPE http_requests counter", om)
        @test !occursin("# TYPE http_requests_total ", om)
        @test occursin("http_requests_total 1", om)
        @test !occursin("http_requests_total_total", om)
    end

    # Clean up
    close(server)
    wait(server)
end

@testset "Utilities" begin
    x = 1
    err = try
        Prometheus.@assert x === nothing
    catch e
        e
    end
    @test err isa Prometheus.AssertionError
    @test err.msg == "x === nothing"
    @test occursin("`x === nothing`", sprint(showerror, err))
    @test occursin("please file an issue", sprint(showerror, err))
    @test occursin(
        "Prometheus.ArgumentError: err",
        sprint(showerror, Prometheus.ArgumentError("err")),
    )
end
