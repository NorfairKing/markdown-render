{ mkDerivation, base, bytestring, cmark, containers, lib, text
, validity, validity-text
}:
mkDerivation {
  pname = "markdown-render";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cmark containers text validity validity-text
  ];
  homepage = "https://github.com/NorfairKing/markdown-render#readme";
  description = "Build CommonMark documents as values and render them safely";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
