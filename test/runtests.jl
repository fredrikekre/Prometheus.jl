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
    c = Prometheus.Counter("metric_name_counter", "A counter."; registry=r)
    @test c in r.collectors
    @test !(c in Prometheus.DEFAULT_REGISTRY.collectors)
    # No registry on construction, register after
    c = Prometheus.Counter("metric_name_counter", "A counter."; registry=nothing)
    @test !(c in Prometheus.DEFAULT_REGISTRY.collectors)
    r = Prometheus.CollectorRegistry()
    Prometheus.register(r, c)
    @test c in r.collectors
    @test_throws(
        Prometheus.ArgumentError("collector already contains a metric with the name \"metric_name_counter\""),
        Prometheus.register(r, c),
    )
end

@testset "Prometheus.Counter" begin
    # Constructors and implicit registration
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    c = Prometheus.Counter("metric_name_counter", "A counter.")
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Counter("metric_name_counter", "A counter."; registry=r)
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
    @test metric.samples.value == c.value
    # Prometheus.expose_metric(...)
    @test sprint(Prometheus.expose_metric, metric) ==
          sprint(Prometheus.expose_io, r) ==
          """
          # HELP metric_name_counter A counter.
          # TYPE metric_name_counter counter
          metric_name_counter 3
          """
end

@testset "Prometheus.Gauge" begin
    # Constructors and implicit registration
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    c = Prometheus.Gauge("metric_name_gauge", "A gauge.")
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Gauge("metric_name_gauge", "A gauge."; registry=r)
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
    c = Prometheus.Summary("metric_name_summary", "A summary."; registry=r)
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
    @test length(metric.samples) == 2
    s1, s2 = metric.samples[1], metric.samples[2]
    @test s1.suffix == "_count"
    @test s2.suffix == "_sum"
    @test s1.label_values === nothing
    @test s2.label_values === nothing
    @test s1.value == 2
    @test s2.value == 11
    # Prometheus.expose_metric(...)
    @test sprint(Prometheus.expose_metric, metric) ==
          sprint(Prometheus.expose_io, r) ==
          """
          # HELP metric_name_summary A summary.
          # TYPE metric_name_summary summary
          metric_name_summary_count 2
          metric_name_summary_sum 11
          """
end

@testset "Prometheus.Histogram" begin
    # Constructors and implicit registration
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    c = Prometheus.Histogram("metric_name_histogram", "A histogram.")
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Histogram("metric_name_histogram", "A histogram."; registry=r)
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
    c = Prometheus.Histogram("metric_name_histogram", "A histogram."; buckets=buckets, registry=r)
    Prometheus.observe(c, 0.5)
    Prometheus.observe(c, 1.6)
    metrics = Prometheus.collect(c)
    @test length(metrics) == 1
    metric = metrics[1]
    @test metric.metric_name == c.metric_name
    @test metric.help == c.help
    @test length(metric.samples) == length(buckets) + 2
    s1, s2 = metric.samples[1], metric.samples[2]
    @test s1.suffix == "_count"
    @test s2.suffix == "_sum"
    @test s1.label_values === nothing
    @test s2.label_values === nothing
    @test s1.value == 2
    @test s2.value == 0.5 + 1.6
    for (ub, counter, sample, known_count) in zip(c.buckets, c.bucket_counters, metric.samples[3:end], [1, 2, 2])
        @test sample.suffix === nothing
        @test (sample.label_names::Prometheus.LabelNames{1}).label_names === (:le,)
        @test (sample.label_values::Prometheus.LabelValues{1}).label_values == (string(ub),)
        @test sample.value == counter[] == known_count
    end
    # Prometheus.expose_metric(...)
    @test sprint(Prometheus.expose_metric, metric) ==
          sprint(Prometheus.expose_io, r) ==
          """
          # HELP metric_name_histogram A histogram.
          # TYPE metric_name_histogram histogram
          metric_name_histogram_count 2
          metric_name_histogram_sum 2.1
          metric_name_histogram{le="1.0"} 1
          metric_name_histogram{le="2.0"} 2
          metric_name_histogram{le="Inf"} 2
          """
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
    @test Prometheus.labels(c, l1) === Prometheus.labels(c, l1)
    @test Prometheus.labels(c, l2) === Prometheus.labels(c, l2)
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
    @test length(metric.samples) == 2
    s1, s2 = metric.samples[1], metric.samples[2]
    @test s1.label_values.label_values == ("/bar/", "404")
    @test s2.label_values.label_values == ("/foo/", "200")
    @test s1.value == 3
    @test s2.value == 3
    # Prometheus.expose_metric(...)
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
    gauge = Prometheus.Gauge("call_time_last", "Time of last call"; registry=nothing)
    Prometheus.@time gauge sleep(0.1)
    @test 0.3 > gauge.value > 0.1
    Prometheus.@time gauge let
        sleep(0.1)
    end
    @test 0.3 > gauge.value > 0.1
    Prometheus.@time gauge f() = sleep(0.1)
    @sync begin
        @async f()
        @async f()
    end
    @test 0.3 > gauge.value > 0.1
    Prometheus.@time gauge function g()
        sleep(0.1)
    end
    @sync begin
        @async g()
        @async g()
    end
    @test 0.3 > gauge.value > 0.1
end

@testset "Prometheus.@time collector::$(Collector)" for Collector in (Prometheus.Histogram, Prometheus.Summary)
    ishist = Collector === Prometheus.Histogram
    buckets = [1.0, Inf]
    collector = Collector(
        "call_time", "Time of calls";
        (ishist ? (; buckets=buckets) : (;))...,
        registry=nothing,
    )
    Prometheus.@time collector sleep(0.1)
    @test 0.3 > collector._sum > 0.1
    @test collector._count == 1
    ishist && @test (x->x[]).(collector.bucket_counters) == [1, 1]
    Prometheus.@time collector let
        sleep(0.1)
    end
    @test 0.4 > collector._sum > 0.2
    @test collector._count == 2
    ishist && @test (x->x[]).(collector.bucket_counters) == [2, 2]
    Prometheus.@time collector f() = sleep(0.1)
    @sync begin
        @async f()
        @async f()
    end
    @test 0.7 > collector._sum > 0.4
    @test collector._count == 4
    ishist && @test (x->x[]).(collector.bucket_counters) == [4, 4]
    Prometheus.@time collector function g()
        sleep(0.1)
    end
    @sync begin
        @async g()
        @async g()
    end
    @test 0.9 > collector._sum > 0.6
    @test collector._count == 6
    ishist && @test (x->x[]).(collector.bucket_counters) == [6, 6]
    if ishist
        Prometheus.@time collector sleep(1.1)
        @test (x->x[]).(collector.bucket_counters) == [6, 7]
    end
end

@testset "Prometheus.@inprogress gauge::Gauge" begin
    gauge = Prometheus.Gauge("calls_inprogres", "Number of calls in progress"; registry=nothing)
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

@testset "Custom collector with @time/@inprogress" begin
    struct Coll <: Prometheus.Collector end
    @test_throws Prometheus.UnreachableError Prometheus.at_time(Coll(), 1.0)
    @test_throws Prometheus.UnreachableError Prometheus.at_inprogress_enter(Coll())
    @test_throws Prometheus.UnreachableError Prometheus.at_inprogress_exit(Coll())
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
    @test Prometheus.labels(c, l1) === Prometheus.labels(c, l1)
    @test Prometheus.labels(c, l2) === Prometheus.labels(c, l2)
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
        @test length(metric.samples) == 2 * (length(buckets) + 2)
        # _count and _sum samples
        s1, s2, s5, s6 = metric.samples[[1, 2, 5, 6]]
        @test s1.label_values.label_values == s2.label_values.label_values == ("/bar/", "404")
        @test s5.label_values.label_values == s6.label_values.label_values == ("/foo/", "200")
        @test s1.value == 2   # _count
        @test s2.value == 6.4 # _sum
        @test s5.value == 2   # _count
        @test s6.value == 4.6 # _sum
        # {le} samples
        for (ls, subrange) in ((l1, 7:8), (l2, 3:4))
            for (ub, counter, sample) in zip(buckets, Prometheus.labels(c, ls).bucket_counters, metric.samples[subrange])
                @test sample.suffix === nothing
                @test (sample.label_names::Prometheus.LabelNames{3}).label_names ===
                      (:endpoint, :status_code, :le)
                @test (sample.label_values::Prometheus.LabelValues{3}).label_values ==
                      (ls..., string(ub))
                @test sample.value == counter[]
            end
        end
    else # Collector === Prometheus.Summary
        @test length(metric.samples) == 4
        s1, s2, s3, s4 = metric.samples
        @test s1.label_values.label_values == s2.label_values.label_values == ("/bar/", "404")
        @test s3.label_values.label_values == s4.label_values.label_values == ("/foo/", "200")
        @test s1.value == 2   # _count
        @test s2.value == 6.4 # _sum
        @test s3.value == 2   # _count
        @test s4.value == 4.6 # _sum
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

@testset "Label types for Prometheus.Family{C}" begin
    struct RequestLabels
        target::String
        status_code::Int
    end
    for fam in (
            # Constructor with NTuple{N, String} names
            Prometheus.Family{Prometheus.Counter}(
                "http_requests", "Total number of HTTP requests", ("target", "status_code");
                registry=nothing,
            ),
            # Constructor with NTuple{N, Symbol} names
            Prometheus.Family{Prometheus.Counter}(
                "http_requests", "Total number of HTTP requests", (:target, :status_code);
                registry=nothing,
            ),
            # Constructor with NamedTuple type
            Prometheus.Family{Prometheus.Counter}(
                "http_requests", "Total number of HTTP requests",
                @NamedTuple{target::String, status_code::Int};
                registry=nothing,
            ),
            # Constructor with custom struct
            Prometheus.Family{Prometheus.Counter}(
                "http_requests", "Total number of HTTP requests", RequestLabels;
                registry=nothing,
            ),
        )
        @test Prometheus.labels(fam, ("/api", "200")) ===
              Prometheus.labels(fam, ("/api", 200)) ===
              Prometheus.labels(fam, (target="/api", status_code="200")) ===
              Prometheus.labels(fam, (target="/api", status_code=200)) ===
              Prometheus.labels(fam, (status_code="200", target="/api")) ===
              Prometheus.labels(fam, (status_code=200, target="/api")) ===
              Prometheus.labels(fam, RequestLabels("/api", 200))
    end
end

@testset "Prometheus.GCCollector" begin
    r = Prometheus.CollectorRegistry()
    c = Prometheus.GCCollector(; registry=r)
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
    c = Prometheus.ProcessCollector(; registry=r)
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
    procc = Prometheus.ProcessCollector(() -> getpid(); registry=nothing)
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
        "counter_name", "Help with slash \\ and newline \n", ("label_name", );
        registry = nothing,
    )
    Prometheus.inc(Prometheus.labels(counter, ("backslash \\, quote \", newline \n", )))
    metric = first(Prometheus.collect(counter))
    @test sprint(Prometheus.expose_metric, metric) ==
        """
        # HELP counter_name Help with slash \\\\ and newline \\n
        # TYPE counter_name counter
        counter_name{label_name="backslash \\\\, quote \\", newline \\n"} 1
        """
end

@testset "Prometheus.expose(::Union{String, IO})" begin
    r = Prometheus.DEFAULT_REGISTRY
    empty!(r.collectors)
    Prometheus.inc(Prometheus.Counter("prom_counter", "Counting things"; registry=r))
    Prometheus.set(Prometheus.Gauge("prom_gauge", "Gauging things"; registry=r), 1.2)
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
    # Spin up the server
    server = HTTP.listen!(8123) do http
        if http.message.target == "/metrics/default"
            return Prometheus.expose(http)
        elseif http.message.target == "/metrics/reg"
            return Prometheus.expose(http, Prometheus.DEFAULT_REGISTRY)
        elseif http.message.target == "/metrics/nogzip"
            return Prometheus.expose(http; compress=false)
        else
            HTTP.setstatus(http, 404)
            HTTP.startwrite(http)
        end
    end
    # Normal requests
    r_default = HTTP.request("GET", "http://localhost:8123/metrics/default")
    r_ref = HTTP.request("GET", "http://localhost:8123/metrics/reg")
    @test String(r_default.body) == String(r_ref.body) == reference_output
    # HEAD
    @test isempty(HTTP.request("HEAD", "http://localhost:8123/metrics/default").body)
    # POST (no filtering in the server above)
    r_post = HTTP.request("POST", "http://localhost:8123/metrics/default")
    @test String(r_post.body) == reference_output
    # Bad URI
    r_bad = HTTP.request("GET", "http://localhost:8123"; status_exception=false)
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
    # Test missing Accept-Encoding (HTTP.jl adds it automatically unless explicitly set)
    r_nogzip = HTTP.request(
        "GET", "http://localhost:8123/metrics/default", ["Accept-Encoding" => ""],
    )
    @test HTTP.header(r_nogzip, "Content-Encoding", nothing) === nothing
    @test String(r_nogzip.body) == reference_output
    # Clean up
    close(server)
    wait(server)
end

@testset "Utilities" begin
    @test_throws Prometheus.UnreachableError Prometheus.unreachable()
    @test_throws Prometheus.AssertionError   Prometheus.@assert false
    @test occursin("unexpected", sprint(showerror, Prometheus.UnreachableError()))
    @test occursin("unexpected", sprint(showerror, Prometheus.AssertionError()))
    @test occursin(
        "Prometheus.ArgumentError: err",
        sprint(showerror, Prometheus.ArgumentError("err")),
    )
    @test_throws Prometheus.UnreachableError Prometheus.prometheus_type(Int)
end
