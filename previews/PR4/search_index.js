var documenterSearchIndex = {"docs":
[{"location":"#Prometheus.jl","page":"Prometheus.jl","title":"Prometheus.jl","text":"","category":"section"},{"location":"#Introduction","page":"Prometheus.jl","title":"Introduction","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"This package is a Julia client for Prometheus. If you are not familiar with Prometheus it is recommended to browse the upstream documentation. The documentation here focuses on the Julia client.","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Two of the basic concepts of a Prometheus client are Registries and Collectors. Registries are collections of collectors, and the collectors are the units responsible to record and capture metrics. Client libraries implement a default registry which all collectors implicity register with, so for basic usage there is no need to interact with a registry (see Default registry).","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"The third important concept is Exposition of the collected metrics. Typically metrics are exposed over a HTTP server, as in the Quickstart-example just below. See the section about Exposition for more details and examples on how metrics can be exposed.","category":"page"},{"location":"#Quickstart","page":"Prometheus.jl","title":"Quickstart","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Install Prometheus.jl and HTTP.jl using the package manager:\npkg> add Prometheus HTTP\nPaste the following code into a Julia REPL.\n# Load the packages\nusing Prometheus, HTTP\n\n# Create a Counter metric\nconst request_counter = Prometheus.Counter(\"request_count\", \"Number of handled requests\")\n\n# Start a HTTP server on localhost port 8000 to server the metrics\nserver = HTTP.listen!(8000) do http\n    Prometheus.inc(request_counter) # Increment the request counter\n    return Prometheus.expose(http)  # Expose the metrics\nend\nVisit http://localhost:8000 in your browser. You will see something like the following\n# HELP gc_alloc_bytes_total Total number of allocated bytes\n# TYPE gc_alloc_bytes_total counter\ngc_alloc_bytes_total 365578814\n\n[...]\n\n# HELP request_count Number of handled requests\n# TYPE request_count counter\nrequest_count 1\nThe output contains some default metrics related to the running process, as well as the request counter that we added ourselves. Every time you refresh, the counter will increment its value. close(server) will shutdown the server.","category":"page"},{"location":"#Collectors","page":"Prometheus.jl","title":"Collectors","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"This section documents the collectors that are currently supported. This include the \"basic\" collectors (Counter, Gauge, Summary) as well as some custom collectors (GCCollector, ProcessCollector). There is also a section on how to implement your own collector, see Custom collectors.","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Upstream documentation:","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"https://prometheus.io/docs/concepts/metric_types/\nhttps://prometheus.io/docs/instrumenting/writing_clientlibs/#metrics","category":"page"},{"location":"#Counter","page":"Prometheus.jl","title":"Counter","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Quoting the upstream documentation:","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"A counter is a cumulative metric that represents a single monotonically increasing counter whose value can only increase or be reset to zero on restart. For example, you can use a counter to represent the number of requests served, tasks completed, or errors.Do not use a counter to expose a value that can decrease. For example, do not use a counter for the number of currently running processes; instead use a gauge.","category":"page"},{"location":"#Counter-API-reference","page":"Prometheus.jl","title":"Counter API reference","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Prometheus.Counter(::String, ::String; kwargs...)\nPrometheus.inc(::Prometheus.Counter, ::Any)","category":"page"},{"location":"#Prometheus.Counter-Tuple{String, String}","page":"Prometheus.jl","title":"Prometheus.Counter","text":"Prometheus.Counter(name, help; registry=DEFAULT_REGISTRY)\n\nConstruct a Counter collector.\n\nArguments\n\nname :: String: the name of the counter metric.\nhelp :: String: the documentation for the counter metric.\n\nKeyword arguments\n\nregistry :: Prometheus.CollectorRegistry: the registry in which to register the collector. If not specified the default registry is used. Pass registry = nothing to skip registration.\n\n\n\n\n\n","category":"method"},{"location":"#Prometheus.inc-Tuple{Prometheus.Counter, Any}","page":"Prometheus.jl","title":"Prometheus.inc","text":"Prometheus.inc(counter::Counter, v = 1)\n\nIncrement the value of the counter with v. v must be non-negative, and defaults to v = 1.\n\n\n\n\n\n","category":"method"},{"location":"#Gauge","page":"Prometheus.jl","title":"Gauge","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Quoting the upstream documentation:","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"A gauge is a metric that represents a single numerical value that can arbitrarily go up and down.Gauges are typically used for measured values like temperatures or current memory usage, but also \"counts\" that can go up and down, like the number of concurrent requests.","category":"page"},{"location":"#Gauge-API-reference","page":"Prometheus.jl","title":"Gauge API reference","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Prometheus.Gauge(::String, ::String; kwargs...)\nPrometheus.inc(::Prometheus.Gauge, ::Any)\nPrometheus.dec(::Prometheus.Gauge, ::Any)\nPrometheus.set(::Prometheus.Gauge, ::Any)\nPrometheus.set_to_current_time(::Prometheus.Gauge)","category":"page"},{"location":"#Prometheus.Gauge-Tuple{String, String}","page":"Prometheus.jl","title":"Prometheus.Gauge","text":"Prometheus.Gauge(name, help; registry=DEFAULT_REGISTRY)\n\nConstruct a Gauge collector.\n\nArguments\n\nname :: String: the name of the gauge metric.\nhelp :: String: the documentation for the gauge metric.\n\nKeyword arguments\n\nregistry :: Prometheus.CollectorRegistry: the registry in which to register the collector. If not specified the default registry is used. Pass registry = nothing to skip registration.\n\n\n\n\n\n","category":"method"},{"location":"#Prometheus.inc-Tuple{Prometheus.Gauge, Any}","page":"Prometheus.jl","title":"Prometheus.inc","text":"Prometheus.inc(gauge::Gauge, v = 1)\n\nIncrement the value of the gauge with v. v defaults to v = 1.\n\n\n\n\n\n","category":"method"},{"location":"#Prometheus.dec-Tuple{Prometheus.Gauge, Any}","page":"Prometheus.jl","title":"Prometheus.dec","text":"Prometheus.dec(gauge::Gauge, v = 1)\n\nDecrement the value of the gauge with v. v defaults to v = 1.\n\n\n\n\n\n","category":"method"},{"location":"#Prometheus.set-Tuple{Prometheus.Gauge, Any}","page":"Prometheus.jl","title":"Prometheus.set","text":"Prometheus.set(gauge::Gauge, v)\n\nSet the value of the gauge to v.\n\n\n\n\n\n","category":"method"},{"location":"#Prometheus.set_to_current_time-Tuple{Prometheus.Gauge}","page":"Prometheus.jl","title":"Prometheus.set_to_current_time","text":"Prometheus.set_to_current_time(gauge::Gauge)\n\nSet the value of the gauge to the current unixtime in seconds.\n\n\n\n\n\n","category":"method"},{"location":"#Summary","page":"Prometheus.jl","title":"Summary","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Quoting the upstream documentation:","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Similar to a histogram, a summary samples observations (usually things like request durations and response sizes). While it also provides a total count of observations and a sum of all observed values, it calculates configurable quantiles over a sliding time window.","category":"page"},{"location":"#Summary-API-reference","page":"Prometheus.jl","title":"Summary API reference","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Prometheus.Summary(::String, ::String; kwargs...)\nPrometheus.observe(::Prometheus.Summary, ::Any)","category":"page"},{"location":"#Prometheus.Summary-Tuple{String, String}","page":"Prometheus.jl","title":"Prometheus.Summary","text":"Prometheus.Summary(name, help; registry=DEFAULT_REGISTRY)\n\nConstruct a Summary collector.\n\nArguments\n\nname :: String: the name of the summary metric.\nhelp :: String: the documentation for the summary metric.\n\nKeyword arguments\n\nregistry :: Prometheus.CollectorRegistry: the registry in which to register the collector. If not specified the default registry is used. Pass registry = nothing to skip registration.\n\n\n\n\n\n","category":"method"},{"location":"#Prometheus.observe-Tuple{Prometheus.Summary, Any}","page":"Prometheus.jl","title":"Prometheus.observe","text":"Prometheus.observe(summary::Summary, v)\n\nAdd the observed value v to the summary. This increases the sum and count of the summary with v and 1, respectively.\n\n\n\n\n\n","category":"method"},{"location":"#GCCollector","page":"Prometheus.jl","title":"GCCollector","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"A collector that exports metrics about allocations and garbage collection (for example number of allocations, number of bytes allocated, time spent in garbage collection, etc). These metrics have the julia_gc_ prefix in their name.","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"A GCCollector is registered automatically with the default registry, see Default registry for more details.","category":"page"},{"location":"#GCCollector-API-reference","page":"Prometheus.jl","title":"GCCollector API reference","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Prometheus.GCCollector(; kwargs...)","category":"page"},{"location":"#Prometheus.GCCollector-Tuple{}","page":"Prometheus.jl","title":"Prometheus.GCCollector","text":"Prometheus.GCCollector(; registry=DEFAULT_REGISTRY)\n\nCreate a collector that exports metrics about allocations and garbage collection.\n\nKeyword arguments\n\nregistry :: Prometheus.CollectorRegistry: the registry in which to register the collector. The default registry is used by default. Pass registry = nothing to skip registration.\n\nnote: Note\nA GCCollector is registered automatically with the default registry. If necessary it can be removed by callingPrometheus.unregister(Prometheus.DEFAULT_REGISTRY, Prometheus.GC_COLLECTOR)\n\n\n\n\n\n","category":"method"},{"location":"#ProcessCollector","page":"Prometheus.jl","title":"ProcessCollector","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"A collector that exports metrics about a running process, for example CPU seconds and metrics about I/O operations. Metrics from this collector have the process_ prefix in their name. This collector is only available on Linux since it requires the /proc file system.","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"A ProcessCollector for the current process is registered automatically with the default registry, see Default registry for more details.","category":"page"},{"location":"#ProcessCollector-API-reference","page":"Prometheus.jl","title":"ProcessCollector API reference","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Prometheus.ProcessCollector(::Function; kwargs...)","category":"page"},{"location":"#Prometheus.ProcessCollector-Tuple{Function}","page":"Prometheus.jl","title":"Prometheus.ProcessCollector","text":"Prometheus.ProcessCollector(pid; registry=DEFAULT_REGISTRY)\n\nCreate a process collector for the process id given by the pid function. The collector exposes metrics about the process' CPU time, start time, memory usage, file usage, and I/O operations.\n\nArguments\n\npid :: Function: a function returning a process id as a string or integer for which to collect metrics. By default the \"self\" pid is used, i.e. the current process.\n\nKeyword arguments\n\nregistry :: Prometheus.CollectorRegistry: the registry in which to register the collector. The default registry is used by default. Pass registry = nothing to skip registration.\n\nnote: Note\nA ProcessCollector for the current process is registered automatically with the default registry. If necessary it can be removed by callingPrometheus.unregister(Prometheus.DEFAULT_REGISTRY, Prometheus.PROCESS_COLLECTOR)\n\nnote: Note\nThe process collector is currently only available on Linux since it requires the /proc file system. On Windows and macOS this collector will not expose any metrics.\n\n\n\n\n\n","category":"method"},{"location":"#Custom-collectors","page":"Prometheus.jl","title":"Custom collectors","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"RandomCollector","category":"page"},{"location":"#Labels","page":"Prometheus.jl","title":"Labels","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"See https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels for details.","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Prometheus.Family{C}(::String, ::String, ::Any; kwargs...) where C\nPrometheus.labels(::Prometheus.Family, ::Vector{String})\nPrometheus.remove(::Prometheus.Family, ::Vector{String})\nPrometheus.clear(::Prometheus.Family)","category":"page"},{"location":"#Prometheus.Family-Union{Tuple{C}, Tuple{String, String, Any}} where C","page":"Prometheus.jl","title":"Prometheus.Family","text":"Prometheus.Family{C}(name, help, labelnames; registry=DEFAULT_REGISTRY)\n\nCreate a labeled collector family with labels given by labelnames. For every new set of label values encountered a new collector of type C <: Collector will be created.\n\nArguments\n\nname :: String: the name of the family metric.\nhelp :: String: the documentation for the family metric.\nlabelnames :: Vector{String}: the label names.\n\nKeyword arguments\n\nregistry :: Prometheus.CollectorRegistry: the registry in which to register the collector. If not specified the default registry is used. Pass registry = nothing to skip registration.\n\nMethods\n\nPrometheus.labels: return the collector for a specific set of labels.\nPrometheus.remove: remove the collector for a specific set of labels.\nPrometheus.clear: remove all collectors in the family.\n\nExamples\n\n# Construct a family of Counters\ncounter_family = Prometheus.Family{Counter}(\n    \"http_requests\", \"Number of HTTP requests\", [\"status_code\", \"endpoint\"],\n)\n\n# Increment the counter for the labels status_code = \"200\" and endpoint = \"/api\"\nPrometheus.inc(Prometheus.labels(counter_family, [\"200\", \"/api\"]))\n\n\n\n\n\n","category":"method"},{"location":"#Prometheus.labels-Tuple{Prometheus.Family, Vector{String}}","page":"Prometheus.jl","title":"Prometheus.labels","text":"Prometheus.labels(family::Family{C}, labelvalues::Vector{String}) where C\n\nReturn the collector of type C from the family corresponding to the labels given by labelvalues.\n\nnote: Note\nThis method does an acquire/release of a lock, and a dictionary lookup, to find the collector matching the label names. For typical applications this overhead does not matter (below 100ns for some basic benchmarks) but it is safe to cache the returned collector if required.\n\n\n\n\n\n","category":"method"},{"location":"#Prometheus.remove-Tuple{Prometheus.Family, Vector{String}}","page":"Prometheus.jl","title":"Prometheus.remove","text":"Prometheus.remove(family::Family, labelvalues::Vector{String})\n\nRemove the collector corresponding to labelvalues. Effectively this resets the collector since Prometheus.labels will recreate the collector when called with the same label names.\n\nnote: Note\nThis method invalidates cached collectors for the label names.\n\n\n\n\n\n","category":"method"},{"location":"#Prometheus.clear-Tuple{Prometheus.Family}","page":"Prometheus.jl","title":"Prometheus.clear","text":"Prometheus.clear(family::Family)\n\nRemove all collectors in the family. Effectively this resets the collectors since Prometheus.labels will recreate them when needed.\n\nnote: Note\nThis method invalidates all cached collectors.\n\n\n\n\n\n","category":"method"},{"location":"#Registries","page":"Prometheus.jl","title":"Registries","text":"","category":"section"},{"location":"#Default-registry","page":"Prometheus.jl","title":"Default registry","text":"","category":"section"},{"location":"#Exposition","page":"Prometheus.jl","title":"Exposition","text":"","category":"section"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Prometheus support","category":"page"},{"location":"","page":"Prometheus.jl","title":"Prometheus.jl","text":"Prometheus.expose","category":"page"},{"location":"#Prometheus.expose","page":"Prometheus.jl","title":"Prometheus.expose","text":"Prometheus.expose(file::String, reg::CollectorRegistry = DEFAULT_REGISTRY)\n\nExport all metrics in reg by writing them to the file file.\n\n\n\n\n\nexpose(io::IO, reg::CollectorRegistry = DEFAULT_REGISTRY)\n\nExport all metrics in reg by writing them to the I/O stream io.\n\n\n\n\n\nexpose(http::HTTP.Stream, reg::CollectorRegistry = DEFAULT_REGISTRY; kwargs...)\n\nExport all metrics in reg by writing them to the HTTP stream http.\n\nThe caller is responsible for checking e.g. the HTTP method and URI target. For HEAD requests this method do not write a body, however.\n\n\n\n\n\n","category":"function"}]
}
