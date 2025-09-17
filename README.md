# SPQR
A package for polynomial reduction and elimination theory over finite fields. It is based on the upcoming publication
- Vsevolod Chestnov and Giulio Crisanti, *SPQR: A Package for Multivariate Polynomial Reduction and Elimination over Finite Fields*,
  _to appear soon_
Please note currently this is an early release/beta.
## Setting up SPQR
### Dependencies
SPQR depends on _FiniteFlow_, which can be found [here](https://github.com/peraro/finiteflow).
Please note that Mathematica 13.1+ is required for SPQR to function correctly.
### Installation
To install, open a Mathematica window and run the command
```wolfram
ResourceFunction["GitHubInstall"]["giu989","SPQR"]
```
The package should now be loadable with
```wolfram
<<SPQR`
```
### Updating
To update to the latest version, open a Mathematica window and run the install command
```wolfram
ResourceFunction["GitHubInstall"]["giu989","SPQR"]
```
The package should now be updated to the latest available version.
### Documentation
Upon loading the package, documentation can be accessed via the "Open documentation" button (if running via a GUI). Documentation will also be provided with the accompanying manuscript upon release.
### Uninstallation
To uninstall, open a Mathematica window and run the command
```wolfram
PacletUninstall["SPQR"]
```