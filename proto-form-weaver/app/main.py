import typer
from typing import Optional
from pathlib import Path
from dataclasses import dataclass
import yaml
from lib import check

app = typer.Typer()

@dataclass
class AppConfig:
    form_schema: Optional[Path] = None
    program_le: Optional[Path] = None

def load_config(config_path: Path) -> AppConfig:
    with open(config_path, "r") as file:
        config_data = yaml.safe_load(file)
        
        if config_data["form_schema"] is None or config_data["program_le"] is None:
            raise ValueError("Error: 'form_schema' and 'program_le' must be defined in config.yaml")

    return AppConfig(
        form_schema=Path(config_data.get("form_schema")),
        program_le=Path(config_data.get("program_le"))
    )

@app.command("check-encoding-with-schema")
def check_encoding_with_schema(
    config_path: Path = typer.Argument("config.yaml", help="Path to the config file")
):
    """
    Check .le encoding against the JSON schema.
    """    
    
    config = load_config(config_path)

    typer.echo(f"Schema path: {config.form_schema}\nprogram.le path: {config.program_le}")

    check.check_encoding_with_schema(config)

@app.command("check-encoding-typos")
def check_encoding_typos(
    config_path: Path = typer.Argument("config.yaml", help="Path to the config file")
):
    """
    Check .le encoding for typos.
    """
    config = load_config(config_path)

    typer.echo(f"program.le Path: {config.program_le}")

    check.check_encoding_typos(config)

@app.command("all-checks")
def all_checks(
    config_path: Path = typer.Argument("config.yaml", help="Path to the config file")
):
    """
    Perform all checks. I.e.
    (i) Check .le encoding for typos; and (ii) Check .le encoding against the JSON schema.  
    """    
    config = load_config(config_path)

    typer.echo(f"Schema path: {config.form_schema}\nprogram.le path: {config.program_le}")
    
    check.do_all_checks(config)

if __name__ == "__main__":
    app()
