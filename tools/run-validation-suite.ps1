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
    "tools/run-full-performance.R"
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
