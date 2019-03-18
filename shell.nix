with import (fetchGit {
    url = https://github.com/NixOS/nixpkgs.git;
    rev = "5082ab8335be9a0895ca78fe1ae81a5a186ae4a4";
  }) {};

mkShell {
  buildInputs = [
    elmPackages.elm
    elmPackages.elm-format
  ];
}
