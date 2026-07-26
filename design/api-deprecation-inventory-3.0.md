# neurogeo public API inventory at the 3.0 boundary

Generated policy for neurogeo 2.9:

- every namespace export is `stable` in 2.9;
- every export has planned 3.0 action `retain`;
- no 2.x export is deprecated or scheduled for silent removal;
- future 3.0 changes require a new reviewed inventory and migration guide.

The machine-readable inventory is returned by `ngeo_api_inventory()`. It is
derived from the installed namespace so new undocumented disposition gaps
cannot be hidden by a manually maintained symbol list.

This audit establishes a boundary, not a promise that 3.0 will add no new
APIs. Any later removal or semantic break must be explicit, documented, and
covered by migration and conformance evidence.
