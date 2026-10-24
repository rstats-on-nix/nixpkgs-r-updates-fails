let
 pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/32f313e49e42f715491e1ea7b306a87c16fe0388.tar.gz") {};
 system_packages = builtins.attrValues {
  inherit (pkgs) gh glibcLocalesUtf8 pandoc nix R   ;
};
  in
  pkgs.mkShell {
    LOCALE_ARCHIVE = if pkgs.system == "x86_64-linux" then  "${pkgs.glibcLocalesUtf8}/lib/locale/locale-archive" else "";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";

    buildInputs = [
      system_packages
      pkgs.rPackages.BiocPkgTools
      pkgs.rPackages.httr2
      pkgs.rPackages.jsonlite
      pkgs.rPackages.knitr
      pkgs.rPackages.readMDTable
      pkgs.rPackages.packageRank
      pkgs.rPackages.reactable
      pkgs.rPackages.rmarkdown
      pkgs.rPackages.rvest
      pkgs.rPackages.tarchetypes
      pkgs.rPackages.targets
    ];

  }
