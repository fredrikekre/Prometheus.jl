# Prometheus.jl changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- ## [Unreleased] -->

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


[Unreleased]: https://github.com/fredrikekre/Literate.jl/compare/v2.15.0...HEAD
[1.0.0]: https://github.com/fredrikekre/Literate.jl/compare/v2.8.1...v2.9.0
