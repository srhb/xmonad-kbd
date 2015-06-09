{ mkDerivation, base, libX11, stdenv, X11, xmonad }:
mkDerivation {
  pname = "xmonad-xkb";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base X11 xmonad ];
  extraLibraries = [ libX11 ];
  license = stdenv.lib.licenses.unfree;
}
