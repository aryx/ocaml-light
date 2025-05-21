# Setup a dev environment for ocaml-light using Nix.
# Run 'nix-shell --pure' from the root of the project to get a dev environment
# ready to compile/test/run ocaml-light from Linux or macOS
# (TODO: on amd64 or arm64).
# See also .github/workflows/nix.yml for its use in Github Actions (GHA).
# See also Dockerfile.
# See also xix/shell.nix for more info about Nix.

let
   # fetch a specific nixos version for better reproducibility
   nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-24.05";
   pkgs = import nixpkgs { config = {}; overlays = []; };
   isX86_64 = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
 in

pkgs.mkShell {
  packages = with pkgs; [
    # usually installed by default but does not hurt to add
    gcc
    binutils
    # Some Makefile relies on perl to fix some syncweb comments
    # alt: use sed
    perl

    # Compile-time external libs, ex: pcre
    # alt: disable otherlibs/graph/
    xorg.libX11
    
    # Optional utilities for development/debugging, ex: which
    # Implicit utilities and libs installed by default in nix
    # (see "echo $PATH | sed -e 's/:/\n/g'" and "ldd ./bin/hello-world"):
    # - gnumake bash
    # - binutils gcc/clang glibc linux-headers gnu-config update-autotools
    # - coreutils findutils diffutils file gnugrep gnused
    # - gnutar gzip bzip2 unzip xz brotli zlib zstd curl
    # - not really needed but here: ed gawk patch patchelf
  ] ++ (if isX86_64 then [
    # for -m32
    gcc_multi
    glibc_multi
  ] else []);    
  
  # Needed to let gcc know it's doing multilib
  hardeningDisable = [ "all" ];
  
  #coupling: Dockerfile
  shellHook = ''
    echo "Shell initialized for ${pkgs.stdenv.hostPlatform.system}"
    ${if isX86_64 then "echo '32-bit compilation supported (gcc -m32)'" else "echo '32-bit compilation not available on this architecture'"}

     echo "you can now run:"
     echo "    ./configure"
     echo "    make clean"
     echo "    make coldstart"
     echo "    make world"
     ${if isX86_64 then "echo '    make opt'" else ""}
     echo "    make test"
  '';
}
