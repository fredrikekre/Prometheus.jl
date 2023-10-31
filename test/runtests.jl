using HTTP: HTTP
using Prometheus: Prometheus
using Test: @test, @test_throws, @testset

@testset "Prometheus.CollectorRegistry" begin
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    # Default registry
    c = Prometheus.Counter("metric_name_counter", "A counter.")
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    @test_throws ErrorException Prometheus.Counter("metric_name_counter", "A counter.")
    Prometheus.unregister(Prometheus.DEFAULT_REGISTRY, c)
    @test !(c in Prometheus.DEFAULT_REGISTRY.collectors)
    c2 = Prometheus.Counter("metric_name_counter", "A counter.")
    @test c2 in Prometheus.DEFAULT_REGISTRY.collectors
    # Provided registry
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Counter(r, "metric_name_counter", "A counter.")
    @test c in r.collectors
    @test !(c in Prometheus.DEFAULT_REGISTRY.collectors)
    # No registry on construction, register after
    c = Prometheus.Counter(nothing, "metric_name_counter", "A counter.")
    @test !(c in Prometheus.DEFAULT_REGISTRY.collectors)
    r = Prometheus.CollectorRegistry()
    Prometheus.register(r, c)
    @test c in r.collectors
    @test_throws ErrorException Prometheus.register(r, c)
end

@testset "Prometheus.Counter" begin
    # Constructors and implicit registration
    empty!(Prometheus.DEFAULT_REGISTRY.collectors)
    c = Prometheus.Counter("metric_name_counter", "A counter.")
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Counter(r, "metric_name_counter", "A counter.")
    @test c in r.collectors
    @test c.value == 0
    # Prometheus.inc(...)
    Prometheus.inc(c)
    @test c.value == 1
    Prometheus.inc(c, 0)
    @test c.value == 1
    Prometheus.inc(c, 2)
    @test c.value == 3
    @test_throws ErrorException Prometheus.inc(c, -1)
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
    c = Prometheus.Gauge(r, "metric_name_gauge", "A gauge.")
    @test c in r.collectors
    @test c.value == 0
    # Prometheus.inc(...)
    Prometheus.inc(c)
    @test c.value == 1
    Prometheus.inc(c, 0)
    @test c.value == 1
    Prometheus.inc(c, 2)
    @test c.value == 3
    @test_throws ErrorException Prometheus.inc(c, -1)
    # Prometheus.dec(...)
    Prometheus.dec(c)
    @test c.value == 2
    Prometheus.dec(c, 1)
    @test c.value == 1
    @test_throws ErrorException Prometheus.dec(c, -1)
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
    c = Prometheus.Summary(r, "metric_name_summary", "A summary.")
    @test c in r.collectors
    @test c._count == 0
    @test c._sum == 0
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
    @test s1.labels === nothing
    @test s2.labels === nothing
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

@testset "Prometheus.LabelNames and Prometheus.LabelValues" begin
    # Custom hashing
    v1 = Prometheus.LabelValues(["foo", "bar"])
    v2 = Prometheus.LabelValues(["foo", "bar"])
    v3 = Prometheus.LabelValues(["foo", "baz"])
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
        ["endpoint", "status_code"],
    )
    @test c in Prometheus.DEFAULT_REGISTRY.collectors
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Family{Collector}(
        r, "http_requests", "Number of HTTP requests.",
        ["endpoint", "status_code"],
    )
    @test c in r.collectors
    @test length(c.children) == 0
    # Prometheus.labels(...), Prometheus.remove(...), Prometheus.clear()
    l1 = ["/foo/", "200"]
    l2 = ["/bar/", "404"]
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
    @test s1.labels.labelvalues == ["/bar/", "404"]
    @test s2.labels.labelvalues == ["/foo/", "200"]
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

@testset "Prometheus.Family{Summary}" begin
    r = Prometheus.CollectorRegistry()
    c = Prometheus.Family{Prometheus.Summary}(
        r, "http_request_time", "Time to process requests.",
        ["endpoint", "status_code"],
    )
    @test c in r.collectors
    @test length(c.children) == 0
    # Prometheus.inc(...)
    l1 = ["/foo/", "200"]
    l2 = ["/bar/", "404"]
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
    @test length(metric.samples) == 4
    s1, s2, s3, s4 = metric.samples
    @test s1.labels.labelvalues == s2.labels.labelvalues == ["/bar/", "404"]
    @test s3.labels.labelvalues == s4.labels.labelvalues == ["/foo/", "200"]
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

@testset "Prometheus.expose(::Union{String, IO})" begin
    r = Prometheus.DEFAULT_REGISTRY
    empty!(r.collectors)
    Prometheus.inc(Prometheus.Counter(r, "prom_counter", "Counting things"))
    Prometheus.set(Prometheus.Gauge(r, "prom_gauge", "Gauging things"), 1.2)
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
    # Clean up
    close(server)
    wait(server)
end

@testset "Utilities" begin
    @test_throws Prometheus.PrometheusUnreachable Prometheus.unreachable()
    @test_throws Prometheus.PrometheusAssert      Prometheus.@assert false
    @test_throws Prometheus.PrometheusUnreachable Prometheus.prometheus_type(Int)
end
