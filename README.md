# SPQR
Polynomial Rational Remainder and Reconstruction (Beta)
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
### Documentation
Upon loading the package, documentation can be accessed via the "Open documentation" button (if running via a GUI). Documentation will also be provided with the accompanying manuscript upon release.
### Uninstallation
To uninstall, open a Mathematica window and run the command
```wolfram
PacletUninstall["SPQR"]
```