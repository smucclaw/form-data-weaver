from pprint import pprint
from jsonref import replace_refs
import ujson
import re
import doctest

def load_schema(config):
    with open(config.form_schema, 'r') as schema_file:
        schema = ujson.load(schema_file)
        resolved_schema = replace_refs(schema)

    return resolved_schema


def get_all_properties(schema):
    return sorted([prop for prop in get_props_base(schema)])

def get_props_base(schema, property_names=None):
    if property_names is None:
        property_names = set()

    if isinstance(schema, dict):
        # Check if 'properties' is in the schema and it is a dictionary
        if 'properties' in schema and isinstance(schema['properties'], dict):
            # Add the keys (field names) to the property_names set
            property_names.update(schema['properties'].keys())

        # Additionally, if 'allOf', 'anyOf', or 'oneOf' are present,
        # iterate through each and collect property names
        for key in ('allOf', 'anyOf', 'oneOf'):
            if key in schema and isinstance(schema[key], list):
                for sub_schema in schema[key]:
                    get_props_base(sub_schema, property_names)

        # If the current dict is not a schema with 'properties' or schema constructs,
        # recursively continue searching for nested properties
        for value in schema.values():
            get_props_base(value, property_names)

    elif isinstance(schema, list):
        # If it's a list, iterate over items which could be schemas
        for item in schema:
            get_props_base(item, property_names)

    return property_names

"""
>>> normalize_property_name("gah_15.1_or_14.1.1_or_14.1.2") == "gah 15.1 or 14.1 PERIOD 1 or 14.1 PERIOD 2"
"""
def normalize_property_name(str):
    # Replace underscores with spaces
    str = str.replace('_', ' ')
    # Replace the third period in things like "Clause 1.1.1" with "PERIOD", lol
    str = re.sub(r'(\d+)\.(\d+)\.(\d+)', r'\1.\2 PERIOD \3', str)

    return str

# sch = load_schema(config)
# [normalize_property_name(p) for p in get_all_properties(sch["$defs"])]


if __name__ == "__main__":
    import doctest
    doctest.testmod()