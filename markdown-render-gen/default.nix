{ mkDerivation, base, bytestring, cmark, genvalidity
, genvalidity-sydtest, genvalidity-text, lib, markdown-render
, QuickCheck, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "markdown-render-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-text markdown-render QuickCheck text
  ];
  testHaskellDepends = [
    base bytestring cmark genvalidity-sydtest markdown-render
    QuickCheck sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/markdown-render#readme";
  description = "Generators for markdown-render";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
