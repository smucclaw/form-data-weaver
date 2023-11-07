This is a quick hack-y, experimental prototype of what I wanted to do with `form-weaver`. Any interfaces or API exposed here should be considered experimental and subject to change.

# Current assumptions

* The functions `is_bool_date_or_int_date` and `schema_not_leaf_sch` assume certain things about the current structure of the json schema. This makes the json parsing less robust to future change, but unfortunately I don't see a purely structural way to do this that wouldn't over-generate.


# Extract relevant fields from json schema

```
poetry run python -m app.main extract-schema-fields
```

Will print in your terminmal:
```
# Normalized fields from schema at <path in config.yaml>:
<\n-separated list of fields>
```

Fields are normalized to: 
* lowercase
* lowercases replaced with spaces
* third period in things like "1.1.1" replaced with " period"