param(
    [string]$StartAt = "",
    [string]$Output = "check-output/validation-suite-60.json"
)

$ErrorActionPreference = "Stop"

$scripts = @(
    "tools/run-conformance.R",
    "tools/validate-external-workflows.R",
    "tools/run-simulation-validation.R",
    "tools/run-support-map-conformance.R",
    "tools/run-support-builder-conformance.R",
    "tools/validate-support-workflows.R",
    "tools/run-support-inference-validation.R",
    "tools/run-support-uncertainty-validation.R",
    "tools/run-support-inference-23-validation.R",
    "tools/run-spatial-models-24-validation.R",
    "tools/run-scalable-io-25-validation.R",
    "tools/run-execution-26-validation.R",
    "tools/run-model-uncertainty-27-validation.R",
    "tools/run-space-graph-28-validation.R",
    "tools/run-interoperability-29-validation.R",
    "tools/run-installed-conformance-291.R",
    "tools/run-schema-30-validation.R",
    "tools/run-file-backed-31-validation.R",
    "tools/run-resampling-32-validation.R",
    "tools/run-spatiotemporal-33-validation.R",
    "tools/run-iterative-models-34-validation.R",
    "tools/run-reproducibility-35-validation.R",
    "tools/run-format-reference-41-validation.R",
    "tools/run-scientific-validation-42.R",
    "tools/run-coverage-421.R",
    "tools/fetch-reference-422.R",
    "tools/run-real-data-validation-422.R",
    "tools/run-cartography-43-validation.R",
    "tools/run-multilayer-45-validation.R",
    "tools/run-coupling-46-validation.R",
    "tools/run-group-inference-47-validation.R",
    "tools/run-support-family-48-validation.R",
    "tools/run-experimental-49-validation.R",
    "tools/run-contract-60-audit.R",
    "tools/fetch-reference-50.R",
    "tools/run-real-multilayer-50-validation.R",
    "tools/run-multilayer-50-performance.R",
    "tools/run-full-performance.R"
)

if ($StartAt) {
    $startIndex = [Array]::IndexOf($scripts, $StartAt)
    if ($startIndex -lt 0) {
        throw "Unknown validation script: $StartAt"
    }
    $scripts = $scripts[$startIndex..($scripts.Count - 1)]
}

$started = [DateTime]::UtcNow
$results = @()
foreach ($script in $scripts) {
    Write-Output "Running $script"
    $timer = [System.Diagnostics.Stopwatch]::StartNew()
    & Rscript.exe $script
    $timer.Stop()
    if ($LASTEXITCODE -ne 0) {
        throw "$script failed with exit code $LASTEXITCODE"
    }
    $results += [ordered]@{
        script = $script
        elapsed_seconds = [Math]::Round($timer.Elapsed.TotalSeconds, 3)
        pass = $true
    }
}

$description = Get-Content -LiteralPath "DESCRIPTION"
$version = ($description | Where-Object { $_ -match '^Version:' }) -replace '^Version:\s*', ''
$report = [ordered]@{
    schema = "neurogeo/validation-suite-6.0"
    package_version = $version
    generated_at_utc = [DateTime]::UtcNow.ToString("yyyy-MM-dd HH:mm:ss 'UTC'")
    started_at_utc = $started.ToString("yyyy-MM-dd HH:mm:ss 'UTC'")
    scripts = $results
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
