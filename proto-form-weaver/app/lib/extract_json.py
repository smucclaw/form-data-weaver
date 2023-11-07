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


def get_relevant_fields(schema):
    fields = extract_fields_up_to_two_ancestors(schema)
    fields = sorted([field for field in fields])
    return fields


def extract_fields_up_to_two_ancestors(schema, path=None, is_root=True):
    """
    Extracts leaves, immed parents, and grandparents of leaves
    """
    fields = set()

    if path is None:
        path = []

    if isinstance(schema, dict):
        # If 'properties' is a key, this is an object; update the path
        if 'properties' in schema:
            # Continue down the schema tree and update the path
            for key, sub_schema in schema['properties'].items():
                # If we're at the root object, we do not add its name to the path
                new_path = [] if is_root else path + [key]
                fields |= extract_fields_up_to_two_ancestors(sub_schema, new_path, is_root=False)
        else:
            # Check for leaf node types
            if 'type' in schema:
                # Add the parent name if available
                if path:
                    fields.add(path[-1])
                # Add the grandparent name if available
                if len(path) > 1:
                    fields.add(path[-2])

        # Recursively check for nested schemas within 'allOf', 'anyOf', 'oneOf'
        for key in ['allOf', 'anyOf', 'oneOf']:
            if key in schema:
                for sub_schema in schema[key]:
                    # When diving into these keys, we are no longer at the root
                    fields |= extract_fields_up_to_two_ancestors(sub_schema, path, is_root=False)

    return fields

"""
>>> normalize_field_name("gah_15.1_or_14.1.1_or_14.1.2") == 'gah 15.1 or 14.1 period 1 or 14.1 period 2'
"""
def normalize_field_name(str):
    # Replace underscores with spaces
    str = str.replace('_', ' ')
    # Replace the third period in things like "Clause 1.1.1" with "PERIOD", lol
    str = re.sub(r'(\d+)\.(\d+)\.(\d+)', r'\1.\2 PERIOD \3', str)

    return str.lower() # we lowercase the LE source code when comparing

sch = load_schema(config)
[normalize_field_name(p) for p in get_relevant_fields(sch)]


if __name__ == "__main__":
    import doctest
    doctest.testmod()