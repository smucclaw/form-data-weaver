import typer
from typing import Optional
from pathlib import Path
from dataclasses import dataclass
import yaml
from functools import partial 
import re

from lib.extract_le import LeProg, extract_nlas_and_rules, load_le_prog
from lib.extract_json import load_schema, normalize_field_name, get_all_fields


CHKMARK = '\u2713'
CROSSMARK = '\u274C'

def prop_is_field_in_rules(search_string, substring):
    """ Returns True if match found, else false """
    escaped_substring = re.escape(substring)
    pattern = r"'s\s+" + escaped_substring + r"\s+is"
    regex = re.compile(pattern)

    match = regex.search(search_string)
    return match is not None

# TODO: need to fix json parsing so we don't get things like 'leg'
def schema_properties_are_subset_of_encoding_rules_fields(schema, leprog):
    """
    Is there any property in the schema that does not appear in stmg of the form
    "’s <property>" in the rules of encoding?
    """

    schema_properties = [normalize_field_name(p) for p in get_all_fields(schema["$defs"])]

    rules_str = "\n".join(leprog.rules).lower()

    prop_check_passed = True
    for prop in schema_properties:
         if not prop_is_field_in_rules(rules_str, prop):
            typer.echo(f"{CROSSMARK} Prop Check failed: No occurrence of <'s {prop}> in encoding rules")
            prop_check_passed = False
    
    if prop_check_passed: 
        typer.echo(f"{CHKMARK} Prop Check passed: for every property in form json schema, there is >=1 occurrence of ''s {prop}' in encoding rules")
        return True
    else: return False


def check_encoding_with_schema(config):
    typer.echo("Checking .le encoding against JSON schema...")

    leprog = extract_nlas_and_rules(load_le_prog(config))
    schema = load_schema(config)

    return schema_properties_are_subset_of_encoding_rules_fields(schema, leprog)


def check_for_typos_in_nlas():
    pass

def check_encoding_typos(config): 
    typer.echo("Checking .le encoding for typos...")


def do_all_checks(config): 
    pass
