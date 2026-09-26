{
  description = "glmnet - Racket bindings for lasso and elastic-net regularized models";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      version = "0.1.0";

      # Source filter shared by both derivations: drop local build artifacts and
      # any dev-staged shared objects so they do not perturb the store hash.
      cleanSrc = pkgs: src:
        pkgs.lib.cleanSourceWith {
          inherit src;
          filter = path: _type:
            let base = baseNameOf path; in
            base != "build"
            && base != "compiled"
            && base != "result"
            && base != "doc"
            && !(pkgs.lib.hasSuffix ".dylib" base)
            && !(pkgs.lib.hasSuffix ".so" base);
        };

      # R environment for the parity harness: the R `glmnet` oracle plus jsonlite
      # for golden output. Used ONLY by the r-parity devShell, the gen-goldens
      # app, and the checks.parity gate -- never by the default build or by the
      # `racket` check, so `nix flake check`'s core stays R-free.
      rEnvFor = pkgs: pkgs.rWrapper.override {
        packages = with pkgs.rPackages; [ glmnet jsonlite survival ];
      };
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };

          # The native C-ABI shim: libglmnetcompat, built from the vendored glmnet
          # Fortran (fortran/vendor/glmnet5dpclean.f, R glmnet 4.1) plus our iso_c_binding wrapper.
          # ctest runs the Fortran self-checks (incl. the 8-byte precision probe).
          native = pkgs.stdenv.mkDerivation {
            pname = "glmnet-compat";
            inherit version;
            src = cleanSrc pkgs ./fortran;

            nativeBuildInputs = [ pkgs.cmake pkgs.gfortran ];
            # libgfortran / libquadmath as a runtime dependency so the shared
            # object's rpath resolves them out of the store.
            buildInputs = [ pkgs.gfortran.cc.lib ];

            cmakeFlags = [ "-DBUILD_TESTING=ON" ];

            doCheck = true;
            checkPhase = ''
              runHook preCheck
              ctest --output-on-failure
              runHook postCheck
            '';
          };

          # The Racket package, with the native library injected. The manual's
          # examples draw the plots of glmnet/plot; the fonts are fixed so that
          # their text renders the same in every sandbox.
          racket = pkgs.stdenv.mkDerivation {
            pname = "glmnet";
            inherit version;
            src = cleanSrc pkgs ./.;

            nativeBuildInputs = [ pkgs.racket pkgs.makeWrapper ];
            buildInputs = [ native ];

            FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ pkgs.dejavu_fonts ]; };

            buildPhase = ''
              runHook preBuild

              export HOME=$TMPDIR
              export PLTUSERHOME=$TMPDIR/racket-home
              export GLMNET_NATIVE_LIB_PATH=${native}
              mkdir -p $PLTUSERHOME

              # Pre-populate native-libs/ so define-runtime-path resolves during
              # the test phase even without the env var.
              mkdir -p ./glmnet/native-libs
              cp ${native}/lib/libglmnetcompat.* ./glmnet/native-libs/ 2>/dev/null || true

              raco pkg install --batch --deps fail --no-setup --copy --scope user \
                --name glmnet ./glmnet

              # Checks the declared dependencies, as the catalog's build server
              # does: plot-lib, pict-lib and draw-lib for glmnet/plot among them.
              raco setup --no-docs --check-pkg-deps --pkgs glmnet

              runHook postBuild
            '';

            doCheck = true;
            checkPhase = ''
              runHook preCheck
              # Recursive: covers tests/ (the plot tests among them) plus the
              # literate examples' companion harnesses under glmnet/examples/test/.
              raco test ./glmnet/

              # Render the Scribble docs to catch errors.
              raco scribble --htmls --dest "$TMPDIR/glmnet-doc" glmnet/scribblings/glmnet.scrbl
              runHook postCheck
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/share $out/bin
              cp -r $PLTUSERHOME $out/share/racket-home

              makeWrapper ${pkgs.racket}/bin/racket $out/bin/glmnet \
                --set PLTUSERHOME $out/share/racket-home \
                --add-flags "-l glmnet"

              runHook postInstall
            '';
          };

          # Stage the native library for non-Nix workflows (dev convenience).
          copy-native-libs = pkgs.writeShellApplication {
            name = "copy-native-libs";
            text = ''
              DEST="$(pwd)/glmnet/native-libs"
              mkdir -p "$DEST"
              cp -v --no-preserve=mode ${native}/lib/libglmnetcompat.* "$DEST/"
              echo "Native library copied to $DEST"
              ls -la "$DEST"
            '';
          };

          # `nix run .#gen-goldens` -- regenerate the R parity goldens (and any
          # R-exported dataset CSVs) from the pinned R glmnet. Run from repo root.
          gen-goldens = pkgs.writeShellApplication {
            name = "gen-goldens";
            runtimeInputs = [ (rEnvFor pkgs) ];
            text = ''
              Rscript "$(pwd)/scripts/r-parity/gen-reference.R" "$@"
            '';
          };

          # Live R-backed parity gate: install the package, generate goldens fresh
          # with the pinned R glmnet oracle (into a temp dir -- nothing committed),
          # then assert our bindings reproduce them. This is the only place R
          # touches CI; the `racket` check stays R-free.
          parity = pkgs.stdenv.mkDerivation {
            pname = "glmnet-parity";
            inherit version;
            src = cleanSrc pkgs ./.;

            nativeBuildInputs = [ pkgs.racket (rEnvFor pkgs) ];
            buildInputs = [ native ];

            buildPhase = ''
              runHook preBuild
              export PLTUSERHOME=$TMPDIR/racket-home
              export GLMNET_NATIVE_LIB_PATH=${native}
              mkdir -p $PLTUSERHOME ./glmnet/native-libs
              cp ${native}/lib/libglmnetcompat.* ./glmnet/native-libs/ 2>/dev/null || true
              raco pkg install --batch --deps fail --no-setup --copy --scope user \
                --name glmnet ./glmnet
              raco setup --no-docs --pkgs glmnet
              runHook postBuild
            '';

            doCheck = true;
            checkPhase = ''
              runHook preCheck
              export PLTUSERHOME=$TMPDIR/racket-home
              export GLMNET_NATIVE_LIB_PATH=${native}

              # Generate goldens fresh from the pinned R glmnet oracle into a temp
              # dir (nothing committed), then assert our bindings reproduce them.
              export GLMNET_GOLDENS_OUT=$TMPDIR/goldens
              Rscript scripts/r-parity/gen-reference.R

              echo "--- live parity: bindings vs fresh R goldens ---"
              GLMNET_PARITY_GOLDENS=$GLMNET_GOLDENS_OUT \
                raco test glmnet/tests/parity-test.rkt
              runHook postCheck
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              echo "glmnet parity check passed" > $out/parity-ok
              runHook postInstall
            '';
          };
        in
        {
          default = racket;
          inherit native racket copy-native-libs gen-goldens parity;
        });

      apps = forAllSystems (system: {
        copy-native-libs = {
          type = "app";
          program = "${self.packages.${system}.copy-native-libs}/bin/copy-native-libs";
        };
        gen-goldens = {
          type = "app";
          program = "${self.packages.${system}.gen-goldens}/bin/gen-goldens";
        };
      });

      checks = forAllSystems (system: {
        inherit (self.packages.${system}) native racket parity;
      });

      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = [ pkgs.racket pkgs.gfortran pkgs.cmake pkgs.gnumake ];
            shellHook = ''
              export PLTUSERHOME="$PWD/.racket-user"
              mkdir -p "$PLTUSERHOME"

              # Build the native library and point the loader + pre-install hook
              # at it, so (require glmnet) and the examples just work.
              echo "Building native library (.#native)..."
              if _np=$(nix build --no-link --print-out-paths .#native 2>/dev/null); then
                export GLMNET_NATIVE_LIB_PATH="$_np"
              else
                echo "  (could not build .#native; see AGENTS.md to build manually)"
              fi

              # Install the package in link mode on first entry.
              _stamp="$PLTUSERHOME/.glmnet-linked"
              if [ ! -f "$_stamp" ]; then
                raco pkg install --batch --auto --link --no-docs --scope user \
                  --skip-installed --name glmnet "$PWD/glmnet" && touch "$_stamp" || true
              fi

              echo ""
              echo "glmnet dev shell ready."
              echo "  Run all examples:  bash scripts/run-examples.sh"
              echo "  Run the tests:     raco test ./glmnet/"
            '';
          };

          # R-enabled shell for (re)generating the parity goldens. Kept separate
          # from `default` so the everyday shell needs no R.
          r-parity = pkgs.mkShell {
            packages = [ pkgs.racket (rEnvFor pkgs) ];
            shellHook = ''
              echo "glmnet R-parity shell."
              echo "  Regenerate goldens:  Rscript scripts/r-parity/gen-reference.R"
            '';
          };
        });
    };
}
