param(
    [string]$StartAt = "",
    [string]$Output = "check-output/validation-suite-60.json",
    [ValidateSet("all", "smoke", "full", "external", "performance")]
    [string]$Level = "all"
)

$ErrorActionPreference = "Stop"
$registryPath = "inst/spec/validation-registry-6.0.csv"
if (-not (Test-Path -LiteralPath $registryPath)) {
    throw "Missing validation registry: $registryPath"
}
$registry = @(Import-Csv -LiteralPath $registryPath)
if ($Level -ne "all") {
    $registry = @($registry | Where-Object { $_.level -eq $Level })
}
if ($StartAt) {
    $startIndex = -1
    for ($i = 0; $i -lt $registry.Count; $i++) {
        if ($registry[$i].suite_id -eq $StartAt -or
            $registry[$i].script -eq $StartAt) {
            $startIndex = $i
            break
        }
    }
    if ($startIndex -lt 0) {
        throw "Unknown validation suite or script: $StartAt"
    }
    $registry = @($registry[$startIndex..($registry.Count - 1)])
}

$started = [DateTime]::UtcNow
$results = @()
foreach ($entry in $registry) {
    if (-not (Test-Path -LiteralPath $entry.script)) {
        throw "Missing registered validation script: $($entry.script)"
    }
    Write-Output "Running $($entry.suite_id): $($entry.script)"
    $scriptArguments = @()
    if ($entry.arguments) {
        $scriptArguments = @($entry.arguments -split ';' | Where-Object { $_ })
    }
    $timer = [System.Diagnostics.Stopwatch]::StartNew()
    & Rscript.exe $entry.script @scriptArguments
    $timer.Stop()
    if ($LASTEXITCODE -ne 0) {
        throw "$($entry.script) failed with exit code $LASTEXITCODE"
    }
    if ($timer.Elapsed.TotalSeconds -gt [double]$entry.timeout_seconds) {
        throw "$($entry.suite_id) exceeded its registry timeout"
    }
    if ($entry.output -and -not (Test-Path -LiteralPath $entry.output)) {
        throw "$($entry.suite_id) did not create registered output $($entry.output)"
    }
    $results += [ordered]@{
        suite_id = $entry.suite_id
        level = $entry.level
        script = $entry.script
        output = $entry.output
        elapsed_seconds = [Math]::Round($timer.Elapsed.TotalSeconds, 3)
        pass = $true
    }
}

$description = Get-Content -LiteralPath "DESCRIPTION"
$version = ($description | Where-Object { $_ -match '^Version:' }) -replace '^Version:\s*', ''
$registrySha = (Get-FileHash -LiteralPath $registryPath -Algorithm SHA256).Hash.ToLowerInvariant()
$sourceCommit = (& git rev-parse HEAD).Trim()
$report = [ordered]@{
    schema = "neurogeo/validation-suite-6.0"
    package_version = $version
    source_commit = $sourceCommit
    validation_registry_sha256 = $registrySha
    generated_at_utc = [DateTime]::UtcNow.ToString("yyyy-MM-dd HH:mm:ss 'UTC'")
    started_at_utc = $started.ToString("yyyy-MM-dd HH:mm:ss 'UTC'")
    level = $Level
    suites = $results
    pass = $true
}
$parent = Split-Path -Parent $Output
if ($parent) {
    New-Item -ItemType Directory -Force -Path $parent | Out-Null
}
$absoluteOutput = [System.IO.Path]::GetFullPath($Output)
$json = $report | ConvertTo-Json -Depth 5
$utf8NoBom = New-Object System.Text.UTF8Encoding($false)
[System.IO.File]::WriteAllText($absoluteOutput, $json, $utf8NoBom)
Write-Output (Resolve-Path -LiteralPath $Output)
