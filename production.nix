{ mkDerivation, aeson, base, bifunctors, bytestring, cabal-install
, cassava, containers, HaTeX, hindent, hlint, lens, pretty-simple
, stdenv, text, time
}:
mkDerivation {
  pname = "udpretty";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bifunctors bytestring cassava containers HaTeX lens
    pretty-simple text time
  ];
  executableToolDepends = [ cabal-install hindent hlint ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
