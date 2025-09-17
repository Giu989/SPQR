# SPQR
A package for polynomial reduction and elimination theory over finite fields. It is based on the upcoming publication
- Vsevolod Chestnov and Giulio Crisanti, *SPQR: A Package for Multivariate Polynomial Reduction and Elimination over Finite Fields*,
  _to appear soon_

Please note currently this is an early release/beta.
## Setting up SPQR
### Dependencies
SPQR depends on _FiniteFlow_, which can be found [here](https://github.com/peraro/finiteflow).
Mathematica 13.1+ is required for SPQR to function correctly.
### Automatic Installation
To install, open a Mathematica window and run the command
```wolfram
ResourceFunction["GitHubInstall"]["giu989","SPQR"];
```
The package should now be loadable with
```wolfram
<<SPQR`
```
### Installation from source
To install from source, clone the git repository to your directory of choice. Then open a Mathematica window and run the command
```wolfram
PacletInstall["/path-to-spqr/build/SPQR-x.x.x.paclet"];
``` 
where ``x-x-x`` is changed to the desired version.
### Updating
To update to the latest version, simply rerun the automatic install command above. The package should now be updated to the latest available version.
### Documentation
Upon loading the package, extensive documentation can be accessed via the "Open documentation" button (if running via a GUI). Furthermore, all publicly available functions have documentation pages that can be accessed directly with
```wolfram
? FunctionName
```
in an identical manner to standard built in functions. Documentation will also be provided in the accompanying manuscript upon release.
### Uninstallation
To uninstall, open a Mathematica window and run the command
```wolfram
PacletUninstall["SPQR"];
```