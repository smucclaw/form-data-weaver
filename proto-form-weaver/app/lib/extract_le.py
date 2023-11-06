from pprint import pprint
import ujson
import re
import doctest
from dataclasses import dataclass


@dataclass(frozen=True)
class LeProg:
    nlas: list[str]
    rules: list[str]     

def load_le_prog(config):
    with open(config.program_le, 'r', encoding='utf-8') as le_prog_file:
        return le_prog_file.readlines()


def parse_section_to_paras(file_lines, start_line):
    """Extracts sections from the file lines starting with the start_line until an unindented line or EOF.
    Captures paragraphs as individual strings."""
    paras = []
    current_section = []
    capture = False
    
    for line in file_lines:
        # Start capturing after the start_line
        if start_line in line:
            capture = True
            continue
        
        # Check if we've started capturing and if the line is not unindented
        if capture:
            if line.strip() == '' and current_section:
                # Empty line signifies end of current paragraph/section
                paras.append(' '.join(current_section).replace(' .', '.'))
                current_section = []
            elif line.startswith(' '):
                # Append line to current section, strip leading/trailing whitespace
                current_section.append(line.strip())
            elif not line.startswith(' ') and line.strip():
                # Non-indented and non-empty line stops capturing
                break

    # Capture any final section if the file didn't end with an empty line
    if current_section:
        paras.append(' '.join(current_section).replace(' .', '.'))

    return paras
    
def extract_nlas_and_rules(lines_of_file):
    """ 
    >>> templates, rules = extract_nlas_and_rules(lines_of_file)
    """
    templates = parse_section_to_paras(lines_of_file, 'the templates are:')[0].split(",")
    rules = parse_section_to_paras(lines_of_file, 'the knowledge base rules includes:')

    return LeProg(templates, rules)

# leprog = extract_nlas_and_rules(load_le_prog(config))