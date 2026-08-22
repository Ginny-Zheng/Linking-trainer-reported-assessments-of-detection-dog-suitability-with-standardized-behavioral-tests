# Data requirements

The repository intentionally excludes individual-level research data.

Place the authorized workbook at:

```text
data/private/time_variant_and_invariant_data.xlsx
```

The workbook must contain one row per dog in the complete longitudinal analysis and these identifiers/outcomes:

- `DogID`
- `final_disposition`, with values `Washout` or `Sale Quality`

At each timepoint (`Time3`, `Time6`, `Time10`, and `Time12`), it must contain trainer-reported suitability and the 11 retained behavioral components:

- `Retrieve`
- `Hunt`
- `Focus.on.Toy/Reward`
- `Physical.Possessiveness.of.Toy`
- `Independence`
- `Work/Effort`
- `Air.Scenting`
- `Surfaces`
- `People`
- `Vehicles.&.Urban.Clutter`
- `Excitability`
- `Trainability.Score`

For example: `Hunt.Time12`, `Work/Effort.Time12`, and `Trainability.Score.Time12`.

If source data encode an uncollected behavioral assessment as zero, convert those structural zeros to `NA` before running the complete-case analysis. Do not convert genuine measured zeros without confirming the scoring convention.

Before public release, remove direct and indirect identifiers, including operational IDs, dates, handler/evaluator free text, location, litter identifiers where disclosure risk exists, and internal disposition notes.
