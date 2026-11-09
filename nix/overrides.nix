{ lib
, haskell
, ...
}:

self: _:
with lib;
with haskell.lib;

let
  # A default build, made strict: built from an sdist, so a file the cabal
  # file forgot to list fails here rather than on Hackage, and with warnings
  # as errors, which stays out of the cabal files so that a warning a later
  # GHC invents fails this build and not the build of whoever depends on this.
  markdownRenderPkg = name: buildStrictly (self.callPackage (../${name}) { });

  markdownRenderPackages = {
    markdown-render = markdownRenderPkg "markdown-render";
    # The generators and the test suite live here, so this is the package
    # whose checks run.
    markdown-render-gen = markdownRenderPkg "markdown-render-gen";
  };
in
{
  inherit markdownRenderPackages;
}
  // markdownRenderPackages
