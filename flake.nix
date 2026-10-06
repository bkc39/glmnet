{
  description = "glmnet - Racket bindings for lasso and elastic-net regularized models";

  inputs = {
    # datasets, used by the evaluated guide, currently requires Racket 9.3.
    nixpkgs.url = "github:NixOS/nixpkgs/07e1d92cdc0ed416cfa11ff3ca40d17e61cfba7a";
    nixpkgs-legacy.url = "github:NixOS/nixpkgs/4df1b885d76a54e1aa1a318f8d16fd6005b6401f";
    # rkt-polars, the catalog package `polars` that glmnet/data/polars adapts.
    # The sandboxed builds cannot reach the package catalog, so they install
    # it from this source, whose native library is the prebuilt candidate the
    # catalog install stages, and its Racket dependencies from the flake's own
    # fixed-output `racket-deps`. Its nixpkgs is not followed: that output's
    # hash is taken with rkt-polars' own Racket. `racket-deps` installs from
    # the live catalog, unpinned (bkc39/rkt-polars#146); AGENTS.md says what
    # to do when its hash stops matching.
    rkt-polars.url = "github:bkc39/rkt-polars";
    datasets-src = {
      url = "github:bkc39/datasets/007c85a57b4e5c638227c5e7b90c50ce18cc63fd";
      flake = false;
    };
    data-frame-src = {
      url = "github:alex-hhh/data-frame/ab3980c4da5a99d2b79172a32b9cb86b2c2b63b4";
      flake = false;
    };
    al2-test-runner-src = {
      url = "github:alex-hhh/al2-test-runner/b6757271932151dff6507ee6f1b690d0268da808";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-legacy, rkt-polars, datasets-src, data-frame-src, al2-test-runner-src }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      version = "0.1.0";

      # Keep the previous pin for Intel macOS, which the Racket 9.3 pin
      # dropped, and for the R glmnet 4.1-10 reference environment below.
      pkgsFor = system:
        import (if system == "x86_64-darwin" then nixpkgs-legacy else nixpkgs) {
          inherit system;
        };

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
      # Updating Racket must not also update the numerical reference: the
      # newer pin has R glmnet 5.0, whose Cox fits differ from our solver.
      rEnvFor = system:
        let referencePkgs = import nixpkgs-legacy { inherit system; };
        in referencePkgs.rWrapper.override {
          packages = with referencePkgs.rPackages; [ glmnet jsonlite survival ];
        };

      # rkt-polars' prebuilt native library for each system it ships one for.
      # Its pre-install hook picks the candidate by OS family, which would give
      # an x86-64 library to aarch64 Linux (bkc39/rkt-polars#146), so the flake
      # picks it by system, and refuses a system with none.
      polarsCandidates = {
        x86_64-linux = "linux/libcompat.so";
        aarch64-darwin = "darwin/libcompat.dylib";
      };
      polarsCandidate = system:
        polarsCandidates.${system} or (throw
          "glmnet: rkt-polars ships no native library for ${system}, so glmnet/data/polars cannot be installed there (bkc39/rkt-polars#146)");

      # The Racket package depends on polars, so it, the default package and
      # the parity check exist only on the systems polars has a library for;
      # the native library builds on all four.
      hasPolars = system: polarsCandidates ? ${system};

      # Installs polars (rkt-polars) and its dependency closure offline into
      # $PLTUSERHOME, before glmnet. Its Racket dependencies are installed first:
      # setup must run on them, since it copies the tzdata package's zoneinfo
      # into the share directory, where gregor, which polars loads, looks for
      # it (the sandbox has no system zoneinfo). polars is installed from a
      # writable copy of its source with the system's prebuilt candidate
      # already in native-libs/, as this flake stages libglmnetcompat, and
      # without the candidates directory, so that its pre-install hook finds
      # the library staged and leaves it be: on macOS under Nix the hook's own
      # copy left native-libs/ empty (bkc39/rkt-polars#146). polarsCandidate is
      # forced first, so that a system with no library fails with its message
      # rather than with an error from evaluating the rkt-polars input there.
      installPolars = system: builtins.seq (polarsCandidate system) ''
        raco pkg install --batch --copy --no-docs --scope user \
          ${rkt-polars.packages.${system}.racket-deps}/*/
        cp -r ${rkt-polars}/polars "$TMPDIR/polars"
        chmod -R u+w "$TMPDIR/polars"
        cp ${rkt-polars}/polars/native-libs/candidates/${polarsCandidate system} \
          "$TMPDIR/polars/native-libs/"
        rm -rf "$TMPDIR/polars/native-libs/candidates"
        raco pkg install --batch --copy --no-docs --scope user \
          --name polars "$TMPDIR/polars"
      '';

      # Guide examples use the catalog datasets API. Stage its source and
      # adapter dependencies offline, reusing the Polars installed above.
      installDatasets = ''
        raco pkg install --batch --deps fail --no-setup --copy --scope user \
          --name al2-test-runner ${al2-test-runner-src}
        raco pkg install --batch --deps fail --no-setup --copy --scope user \
          --name data-frame ${data-frame-src}
        raco pkg install --batch --deps fail --no-setup --copy --scope user \
          --name datasets-core ${datasets-src}/datasets-core
        raco pkg install --batch --deps fail --no-setup --copy --scope user \
          --name datasets ${datasets-src}/datasets
      '';
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = pkgsFor system;

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

              ${installPolars system}
              ${installDatasets}

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
            runtimeInputs = [ (rEnvFor system) ];
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

            nativeBuildInputs = [ pkgs.racket (rEnvFor system) ];
            buildInputs = [ native ];

            buildPhase = ''
              runHook preBuild
              export PLTUSERHOME=$TMPDIR/racket-home
              export GLMNET_NATIVE_LIB_PATH=${native}
              mkdir -p $PLTUSERHOME ./glmnet/native-libs
              cp ${native}/lib/libglmnetcompat.* ./glmnet/native-libs/ 2>/dev/null || true
              ${installPolars system}
              ${installDatasets}
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
          inherit native copy-native-libs gen-goldens;
        } // nixpkgs.lib.optionalAttrs (hasPolars system) {
          default = racket;
          inherit racket parity;
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

      checks = forAllSystems (system:
        {
          inherit (self.packages.${system}) native;
        } // nixpkgs.lib.optionalAttrs (hasPolars system) {
          inherit (self.packages.${system}) racket parity;
        });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
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
            packages = [ pkgs.racket (rEnvFor system) ];
            shellHook = ''
              echo "glmnet R-parity shell."
              echo "  Regenerate goldens:  Rscript scripts/r-parity/gen-reference.R"
            '';
          };
        });
    };
}
