{ lib
, stdenv
, libxslt
, fontconfig
, lndir
, roboto
, twitter-color-emoji
}:

# Chromium reads FONTCONFIG_SYSROOT, and uses that instead of / for all font-related filepaths
# Chromium also reads FONTCONFIG_PATH, but always also reads FONTCONFIG_SYSROOT/etc/fonts/font.conf so you cannot override fonts, only add to them
# so here we have to nix build of something that looks like / with everything font-related, (which I could only do based on the strace output showing me which things it tries to read in FONTCONFIG_SYSROOT)
# and then set FONTCONFIG_SYSROOT to that.
# and I only found FONTCONFIG_SYSROOT by looking through the chromium source code
let
  fonts = {
    "roboto" = roboto;
    "twitter" = twitter-color-emoji;
  };
  fontNames = builtins.attrNames fonts;
in
stdenv.mkDerivation {
  name = "fonts-config";
  unpackPhase = "true";
  nativeBuildInputs = [ libxslt lndir ];
  buildInputs = [ fontconfig ];
  inherit fontNames;
  installPhase =
    let singleDir = name: fontPkg: ''
      echo ${fontPkg}
      ln -s ${fontPkg} $out/etc/fonts/${name}
    '';
    in
    ''
      mkdir -p $out/etc/fonts

      ${lib.concatStringsSep "\n" (lib.mapAttrsToList singleDir fonts)}

      mkdir -p $out/fonts
      xsltproc \
        --stringparam fontNames "$fontNames" \
        --path ${fontconfig.out}/share/xml/fontconfig \
        ${./fonts.xsl} ${fontconfig.out}/etc/fonts/fonts.conf \
        > $out/etc/fonts/fonts.conf
    '';
}
