This is a quick hack-y, experimental prototype of what I wanted to do with `form-weaver`. Any interfaces or API exposed here should be considered experimental and subject to change.


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