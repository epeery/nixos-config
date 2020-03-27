{ pkgs, ... }:

let
  wrapScripts = path:
    let
      scripts = builtins.attrNames (builtins.readDir path); # Assumes there are no subfolders
    in
      map (x: pkgs.writeScriptBin x (builtins.readFile "${path}/${x}")) scripts;
in
{
  home.packages = wrapScripts ./scripts;
}
