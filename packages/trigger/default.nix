{ stdenv, lib, inotify-tools }:

stdenv.mkDerivation rec {
  name = "trigger";
  version = "1.0";

  src = builtins.fetchGit {
    url =  "https://github.com/sharkdp/trigger";
    rev = "8d039e35010a18f4afba08093d7a2969d7df70de";
  };

  buildInputs = [ inotify-tools ];

  installPhase = ''
    mkdir -p $out/bin
    cp tg trigger $out/bin
  '';

  postFixup = with lib; ''
    sed -i "2 i export PATH=${makeBinPath buildInputs}:\$PATH" $out/bin/trigger
  '';
}
