# Tooling

Requires Rust, Typst 0.15.1, jq, curl, and tar. Vale downloads automatically.

From the repository root:

```sh
./tools/scripts/check.sh all
./tools/scripts/render.sh dist
```

Pass `structure`, `drafting`, `language`, `integrity` or `pdf` to `check.sh` for one check.
The PDF name uses the UTC publication year; `PUBLICATION_YEAR` and
`SOURCE_DATE_EPOCH` reproduce a prior build.
Rust checks structure, references, governance and drafting; Vale checks spelling and repetition.
Strict clause checks activate when the source gains canonical clause numbers.
Vale's required style files are in `config/Constitution/`.

Set `CONSTITUTION_ALLOW_GAPS=true` for a topic draft with intentional numbering
gaps; both `structure` and `all` honour this setting. Use strict checks for `final`.
Run `./tools/scripts/check.sh selfcheck` to check the validators and verify that
failed or invalid PDF builds preserve the previous PDF and remove temporary files.
