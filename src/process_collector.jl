# SPDX-License-Identifier: MIT

#################################
# ProcessCollector <: Collector #
#################################

mutable struct ProcessCollector <: Collector
    @const pid::Function
    @atomic initialized::Ptr{Nothing}
    @atomic system_boot_time::Int
    @atomic clock_ticks_per_second::Int
    @atomic pagesize::Int
    function ProcessCollector(
            pid::Function = () -> "self";
            registry::Union{CollectorRegistry, Nothing} = DEFAULT_REGISTRY,
        )
        procc = new(pid, C_NULL, 0, 0, 0)
        if registry !== nothing
            register(registry, procc)
        end
        return procc
    end
end

# Initialize the ProcessCollector on first use in a given process. This is necessary because
# typically collectors are defined as global variables which may have been cached during
# precompilation. The struct field initialized::Ptr is used to detect this: if it is NULL,
# then either the collector was constructed in this session (since it is set to null in the
# inner constructor), or it was deserialized from a cache file (since pointers are zeroed in
# the precompilation serialize/deserialize process). Important to note is that this property
# holds even if the collector was initialized in the process that output the serialized
# file. This would not be hold for e.g. a initialized::Bool field.
function initialize_process_collector(procc::ProcessCollector)
    if procc.initialized !== C_NULL
        return
    end
    system_boot_time = 0
    try
        proc_stat = read("/proc/stat", String)
        m = match(r"^btime\s+(\d+)"m, proc_stat)::RegexMatch
        system_boot_time = parse(Int, m.captures[1]::AbstractString)
    catch e
        @debug "ProcessCollector: /proc is not available or not readable, disabling." e
    end
    # Fetch clock ticks per second
    clock_ticks_per_second = 0
    try
        cmd = pipeline(`getconf CLK_TCK`, stderr = devnull)
        str = read(cmd, String)
        clock_ticks_per_second = parse(Int, strip(str))
    catch e
        if system_boot_time > 0
            @debug "ProcessCollector: /proc is available but could not read " *
                "CLK_TCK from getconf, partially disabling." e
        end
    end
    # Fetch pagesize
    pagesize = 0
    try
        cmd = pipeline(`getconf PAGESIZE`, stderr = devnull)
        str = read(cmd, String)
        pagesize = parse(Int, strip(str))
    catch e
        if system_boot_time > 0
            @debug "ProcessCollector: /proc is available but could not read " *
                "PAGESIZE from getconf, partially disabling." e
        end
    end
    # Set the values and return
    @atomic procc.system_boot_time = system_boot_time
    @atomic procc.clock_ticks_per_second = clock_ticks_per_second
    @atomic procc.pagesize = pagesize
    @atomic procc.initialized = Ptr{Nothing}(0xdeadbeef % UInt)
    return
end

"""
    Prometheus.ProcessCollector(pid; registry=DEFAULT_REGISTRY)

Create a process collector for the process id given by the `pid` function. The collector
exposes metrics about the process' CPU time, start time, memory usage, file usage, and I/O
operations.

**Arguments**
 - `pid :: Function`: a function returning a process id as a string or integer for which to
   collect metrics. By default the `"self"` pid is used, i.e. the current process.
**Keyword arguments**
 - `registry :: Prometheus.CollectorRegistry`: the registry in which to register the
   collector. The default registry is used by default. Pass `registry = nothing` to skip
   registration.

!!! note
    A `ProcessCollector` for the current process is registered automatically with the
    default registry. If necessary it can be removed by calling
    ```julia
    Prometheus.unregister(Prometheus.DEFAULT_REGISTRY, Prometheus.PROCESS_COLLECTOR)
    ```

!!! note
    The process collector is currently only available on Linux since it requires the `/proc`
    file system. On Windows and macOS this collector will not expose any metrics.
"""
ProcessCollector(::Function; kwargs...)

function metric_names(::ProcessCollector)
    return (
        "process_cpu_seconds_total", "process_start_time_seconds",
        "process_virtual_memory_bytes", "process_resident_memory_bytes", "process_open_fds",
        "process_io_rchar_bytes_total", "process_io_wchar_bytes_total",
        "process_io_syscr_total", "process_io_syscw_total", "process_io_read_bytes_total",
        "process_io_write_bytes_total",
    )
end

function collect!(metrics::Vector, procc::ProcessCollector)
    initialize_process_collector(procc)
    @assert procc.initialized !== C_NULL
    # Unpack variables
    system_boot_time = procc.system_boot_time
    clock_ticks_per_second = procc.clock_ticks_per_second
    pagesize = procc.pagesize
    # If reading the system boot time from /proc/stat failed then that is used as an
    # indicator for a missing or unreadable /proc fs so then return early
    procc.system_boot_time == 0 && return metrics
    # Fetch the pid
    pid = try
        String(strip(string(procc.pid()::Union{AbstractString, Integer})))::String
    catch e
        @error "ProcessCollector: could not look up the pid from the lambda" e
        return metrics
    end
    if isempty(pid) || !isdir("/proc/$(pid)")
        @error "ProcessCollector: invalid pid '$(pid)' from lamba: /proc/$(pid)/ does not exist"
        return metrics
    end
    # Read /proc/$(pid)/stat
    proc_stat = nothing
    try
        proc_stat = read("/proc/$(pid)/stat", String)
    catch e
        @error "ProcessCollector: could not read /proc/$(pid)/stat" e
    end
    if proc_stat !== nothing
        fields = split(split(proc_stat, ')')[end]) # This strips off the first two fields
        # CPU time and start time requires clock_ticks_per_second
        if clock_ticks_per_second > 0
            utime = parse(Int, fields[14 - 2]) / clock_ticks_per_second
            stime = parse(Int, fields[15 - 2]) / clock_ticks_per_second
            label_names = LabelNames(("mode",))
            proc_cpu_seconds = Metric(
                "counter", "process_cpu_seconds_total",
                "Total CPU time (user and system mode) in seconds.",
                [
                    Sample(nothing, label_names, LabelValues(("system",)), stime),
                    Sample(nothing, label_names, LabelValues(("user",)), utime),
                ],
            )
            push!(metrics, proc_cpu_seconds)
            # Process start time
            starttime = parse(Int, fields[22 - 2]) / clock_ticks_per_second
            proc_start_time = Metric(
                "gauge", "process_start_time_seconds",
                "Start time since unix epoch in seconds.",
                Sample(nothing, nothing, nothing, starttime + system_boot_time),
            )
            push!(metrics, proc_start_time)
        end
        # Virtual memory
        vsize = parse(Int, fields[23 - 2])
        proc_virtual_memory = Metric(
            "gauge", "process_virtual_memory_bytes", "Virtual memory size in bytes.",
            Sample(nothing, nothing, nothing, vsize),
        )
        push!(metrics, proc_virtual_memory)
        if pagesize > 0
            # Resident memory
            rss = parse(Int, fields[24 - 2])
            proc_resident_memory = Metric(
                "gauge", "process_resident_memory_bytes",
                "Resident memory size (RSS) in bytes.",
                Sample(nothing, nothing, nothing, rss * pagesize),
            )
            push!(metrics, proc_resident_memory)
        end
    end
    # Read /proc/$(pid)/fds
    proc_fd = nothing
    try
        proc_fd = length(readdir("/proc/$(pid)/fd"))
    catch e
        @error "ProcessCollector: could not read /proc/$(pid)/fd" e
    end
    if proc_fd !== nothing
        # Open file descriptors
        proc_open_fds = Metric(
            "gauge", "process_open_fds",
            "Number of open file descriptors.",
            Sample(nothing, nothing, nothing, proc_fd),
        )
        push!(metrics, proc_open_fds)
        # TODO: Maybe add maximum open fds from /proc/$(pid)/limits like the Python client
    end
    # Read /proc/$(pid)/io
    proc_io = nothing
    try
        proc_io = read("/proc/$(pid)/io", String)
    catch e
        @error "ProcessCollector: could not read /proc/$(pid)/io" e
    end
    if proc_io !== nothing
        rchar = match(r"rchar:\s+(\d+)", proc_io)
        if rchar !== nothing
            proc_io_rchar = Metric(
                "counter", "process_io_rchar_bytes_total",
                "Total number of bytes read in bytes (rchar from /proc/[pid]/io).",
                Sample(nothing, nothing, nothing, parse(Int, rchar.captures[1]::AbstractString)),
            )
            push!(metrics, proc_io_rchar)
        end
        wchar = match(r"wchar:\s+(\d+)", proc_io)
        if wchar !== nothing
            proc_io_wchar = Metric(
                "counter", "process_io_wchar_bytes_total",
                "Total number of bytes written in bytes (wchar from /proc/[pid]/io).",
                Sample(nothing, nothing, nothing, parse(Int, wchar.captures[1]::AbstractString)),
            )
            push!(metrics, proc_io_wchar)
        end
        syscr = match(r"syscr:\s+(\d+)", proc_io)
        if syscr !== nothing
            proc_io_syscr = Metric(
                "counter", "process_io_syscr_total",
                "Total number of read I/O operations (syscalls) (syscr from /proc/[pid]/io).",
                Sample(nothing, nothing, nothing, parse(Int, syscr.captures[1]::AbstractString)),
            )
            push!(metrics, proc_io_syscr)
        end
        syscw = match(r"syscw:\s+(\d+)", proc_io)
        if syscw !== nothing
            proc_io_syscw = Metric(
                "counter", "process_io_syscw_total",
                "Total number of write I/O operations (syscalls) (syscw from /proc/[pid]/io).",
                Sample(nothing, nothing, nothing, parse(Int, syscw.captures[1]::AbstractString)),
            )
            push!(metrics, proc_io_syscw)
        end
        read_bytes = match(r"read_bytes:\s+(\d+)", proc_io)
        if read_bytes !== nothing
            proc_io_read_bytes = Metric(
                "counter", "process_io_read_bytes_total",
                "Total number of bytes read from the file system (read_bytes from /proc/[pid]/io).",
                Sample(nothing, nothing, nothing, parse(Int, read_bytes.captures[1]::AbstractString)),
            )
            push!(metrics, proc_io_read_bytes)
        end
        write_bytes = match(r"write_bytes:\s+(\d+)", proc_io)
        if write_bytes !== nothing
            proc_io_write_bytes = Metric(
                "counter", "process_io_write_bytes_total",
                "Total number of bytes written to the file system (write_bytes from /proc/[pid]/io).",
                Sample(nothing, nothing, nothing, parse(Int, write_bytes.captures[1]::AbstractString)),
            )
            push!(metrics, proc_io_write_bytes)
        end
    end
    return metrics
end
