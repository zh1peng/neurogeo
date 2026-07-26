param(
    [string]$StartAt = ""
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
    "tools/run-release-performance.R"
)

if ($StartAt) {
    $startIndex = [Array]::IndexOf($scripts, $StartAt)
    if ($startIndex -lt 0) {
        throw "Unknown validation script: $StartAt"
    }
    $scripts = $scripts[$startIndex..($scripts.Count - 1)]
}

foreach ($script in $scripts) {
    Write-Output "Running $script"
    & Rscript.exe $script
    if ($LASTEXITCODE -ne 0) {
        throw "$script failed with exit code $LASTEXITCODE"
    }
}
