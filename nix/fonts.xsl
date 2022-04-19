<?xml version="1.0"?>

<!--
  This is an adaptation of https://github.com/NixOS/nixpkgs/blob/068984c00e0d4e54b6684d98f6ac47c92dcb642e/pkgs/development/libraries/fontconfig/make-fonts-conf.xsl
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:str="http://exslt.org/strings"
                extension-element-prefixes="str"
  >

  
  <xsl:param name="fontNames" />

  <xsl:template match="/fontconfig">

    <fontconfig>
      <cachedir prefix="xdg">fontconfig</cachedir>


      <xsl:for-each select="str:tokenize($fontNames)">
        <dir>/etc/fonts/<xsl:value-of select="." />/share/fonts</dir>
        <xsl:text>&#0010;</xsl:text>
      </xsl:for-each>
    </fontconfig>

  </xsl:template>

</xsl:stylesheet>
