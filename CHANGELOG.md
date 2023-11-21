# Prometheus.jl changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
 - The fourth basic collector, `Histogram`, have been added. ([#10][github-10])

## [1.1.0] - 2023-11-13
### Added
 - New macro `Prometheus.@time collector <expr>` for timing `<expr>` and pass the elapsed
   time to the collector. `<expr>` can be a single expression, a block, or a function
   *definition*. In the latter case, all calls to the function will be instrumented (no
   matter the call site). See documentation for more details. ([#6][github-6])
 - New macro `Prometheus.@inprogress collector <expr>` to track number of in-progress
   concurrent evalutations of `<expr>`. Just like `Prometheus.@time`, valid `<expr>`s are
   single expressions, blocks, and function definitions. See documentation for more details.
   ([#6][github-6])
 - New ways to specify label names and label values in `Prometheus.Family{C}`. Label names
   can now be passed to the constructor as i) a tuple of strings or symbols, ii) a named
   tuple type (names used for label names), or iii) a custom struct type (field names used
   for label names). Similarly, label values (passed to e.g. `Prometheus.labels`) can be
   passed as i) tuple of strings, ii) named tuple, iii) struct instance. See documentation
   for examples and more details. ([#7][github-7])

## [1.0.1] - 2023-11-06
### Fixed
 - Fixed verification of metric names and label names.
 - Correctly escape special characters in exposition (specifically help and label values).

## [1.0.0] - 2023-11-05

First stable release of Prometheus.jl:

 - Supported basic collectors: Counter, Gauge, Summary
 - GCCollector for metrics about allocations and garbage collection
 - ProcessCollector for process metrics such as CPU time and I/O operations (requires the
   /proc file system).
 - Support for default and custom collector registries
 - Support for metric labeling
 - Support for exposing metrics to file and over HTTP
 - Support for gzip compression when exposing over HTTP

See [README.md](README.md) for details and documentation.


[github-6]: https://github.com/fredrikekre/Prometheus.jl/pull/6
[github-7]: https://github.com/fredrikekre/Prometheus.jl/pull/7
[github-10]: https://github.com/fredrikekre/Prometheus.jl/pull/10

[Unreleased]: https://github.com/fredrikekre/Prometheus.jl/compare/v1.1.0...HEAD
[1.1.0]: https://github.com/fredrikekre/Prometheus.jl/compare/v1.0.1...v1.1.0
[1.0.1]: https://github.com/fredrikekre/Prometheus.jl/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/fredrikekre/Prometheus.jl/tree/v1.0.0
