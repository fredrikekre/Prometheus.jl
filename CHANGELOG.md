# Prometheus.jl changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
 - New macro `Prometheus.@time collector <expr>` for timing `<expr>` and pass the elapsed
   time to the collector. `<expr>` can be a single expression, a block, or a function
   *definition*. In the latter case, all calls to the function will be instrumented (no
   matter the call site). See documentation for more details. ([#6][github-6])
 - New macro `Prometheus.@inprogress collector <expr>` to track number of in-progress
   concurrent evalutations of `<expr>`. Just like `Prometheus.@time`, valid `<expr>`s are
   single expressions, blocks, and function definitions. See documentation for more details.
   ([#6][github-6])

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

[Unreleased]: https://github.com/fredrikekre/Prometheus.jl/compare/v1.0.1...HEAD
[1.0.1]: https://github.com/fredrikekre/Prometheus.jl/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/fredrikekre/Prometheus.jl/tree/v1.0.0
