import typer
from typing import Optional
from pathlib import Path
from dataclasses import dataclass
import yaml
from functools import partial 
import re

from .extract_le import LeProg
from . import extract_le as exle
from . import extract_json as jsch
# from .extract_json import load_schema, normalize_field_name, get_fields_from_cfg


CHKMARK = '\u2713'
CROSSMARK = '\u274C'

def match2bool(match_result):
    return match_result is not None

def sch_field_is_used_in_rules(lowercased_rules_str, substring):
    """ Returns True if any match found, else false """
    escaped_substring = re.escape(substring)

    patterns = [r"'s\s+" + escaped_substring + r"\s+is",
                escaped_substring + r"'s",
                escaped_substring + r"\sis\s"
                ]    
    rgxes = [re.compile(pattrn) for pattrn in patterns]

    any_match = any(
                        match2bool(rgx.search(lowercased_rules_str)) 
                        for rgx in rgxes
                    )
    return any_match

def schema_properties_are_subset_of_encoding_rules_fields(config, schema, leprog):
    """
    For any of the relevant fields in the schema,
    is it the case that 
    (i) it does not appear in stmg of the form
        "'s <field>" in the rules of encoding?
    and 
    (ii) it does not appear in smtg of the form 
        "<field>'s"
    """

    fields = jsch.get_fields_from_cfg(config)
    rules_str = "\n".join(leprog.rules).lower()

    check_passed = True
    for field in fields:
         if not sch_field_is_used_in_rules(rules_str, field):
            typer.echo(f"{CROSSMARK} Fields Check failed: {field} does not seem to be used in encoding rules")
            check_passed = False
    
    if check_passed: 
        typer.echo(f"{CHKMARK} Fields Check passed: for every field in form json schema, there is >=1 use of {field} in encoding rules")
        return True
    else: return False


def check_encoding_with_schema(config):
    typer.echo("Checking .le encoding against JSON schema...")

    leprog = exle.extract_nlas_and_rules(exle.load_le_prog(config))
    schema = jsch.load_schema(config)

    schema_properties_are_subset_of_encoding_rules_fields(config, schema, leprog)


def check_for_typos_in_nlas():
    pass

def check_encoding_typos(config): 
    typer.echo("Checking .le encoding for typos...")


def do_all_checks(config): 
    pass
