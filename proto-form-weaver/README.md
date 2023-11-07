This is a quick hack-y, experimental prototype of what I wanted to do with `form-weaver`. Any interfaces or API exposed here should be considered experimental and subject to change.

# Current assumptions

* The functions `is_bool_date_or_int_date` and `schema_not_leaf_sch` assume certain things about the current structure of the json schema. This does make the json parsing less robust to future change.
  - [ ] TODO] I have an idea for how to make this more generic / make the assumptions a lot more minimal; I will do this when time permits.


# Extract relevant fields from json schema

Run, from `form-weaver/proto-form-weaver`
```
poetry run python -m app.main extract-schema-fields
```

This will print in your terminmal:
```
# Normalized fields from schema at <path in config.yaml>:
<\n-separated list of fields>
```

The fields that are extracted are:
* leaf fields (loosely construed), 
* their parents, 
* and their grandparents

See the code for more details on the extraction.

Field names are normalized to: 
* lowercase
* underscores replaced with spaces
* third period in things like "1.1.1" replaced with " period"

# Check LE encoding against schema

Run, from `form-weaver/proto-form-weaver`
```
poetry run python -m app.main check-le-with-schema
```

In the terminal printout, the fields are normalized as per the above.

What I currently do, ish:
* extract fields as detailed above from json schema
* for each of those fields, check if it occurs in one of the pre-defined regex patterns in the rules encoding of the .le file (whose path has been declared in the `config.yaml`)
