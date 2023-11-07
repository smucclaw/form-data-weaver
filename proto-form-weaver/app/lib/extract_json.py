from pprint import pprint
from jsonref import replace_refs
import ujson
import re
import doctest

def get_fields_from_cfg(config):
    sch = load_schema(config)
    return [normalize_field_name(f) for f in get_relevant_fields(sch)]

def load_schema(config):
    with open(config.form_schema, 'r') as schema_file:
        schema = ujson.load(schema_file)
        resolved_schema = replace_refs(schema)

    return resolved_schema

def get_relevant_fields(schema):
    fields = extract_fields_up_to_two_ancestors(schema)
    fields = sorted([field for field in fields])
    return fields


def extract_fields_up_to_two_ancestors(schema, path=None):
    """
    Extracts leaves, immed parents, and grandparents of leaves
    """
    def is_object(sch): return 'properties' in sch
    def is_bool_date_or_int_date(sch): 
        return is_object(sch) and "happened" in sch['properties'] or "number" in sch['properties']
        # technically don't need is_object(sch) bc of short-circuit evaln, but let's play it safe
        # TODO: think more about whether there's a way to make this more general or at least make it part of config. But rn I don't think it's easy to make this more general, since going purely structural will result in capturing things we don't want
    def schema_not_leaf_sch(sch): return is_object(sch) and not is_bool_date_or_int_date(sch) 

    fields = set()
    if path is None:
        path = []

    if isinstance(schema, dict):
        if is_object(schema):
            for child_key, child_schema in schema['properties'].items():
                
                new_path = [] if schema_not_leaf_sch(child_schema) else path + [child_key]
                fields |= extract_fields_up_to_two_ancestors(child_schema, new_path)
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
                    fields |= extract_fields_up_to_two_ancestors(sub_schema, path)

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


if __name__ == "__main__":
    import doctest
    doctest.testmod()