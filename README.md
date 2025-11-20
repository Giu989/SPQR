# SP$`\mathbb{Q}`$R
A package for polynomial reduction and elimination theory over finite fields. It is based on the publication
- Vsevolod Chestnov and Giulio Crisanti, [Sampling Polynomial Rational Remainders with SPQR: A new Package for Polynomial Division and Elimination](https://arxiv.org/abs/2511.14875).

## Setting up SP$`\mathbb{Q}`$R
### Dependencies
SP$`\mathbb{Q}`$R depends on ``FiniteFlow``, which can be found [here](https://github.com/peraro/finiteflow).
Mathematica 13.1+ is required for SP$`\mathbb{Q}`$R to function correctly.
### Automatic Installation
To install, open a Mathematica window and run the command
```wolfram
ResourceFunction["GitHubInstall"]["giu989","SPQR"];
```
The package should now be loadable with
```wolfram
<<SPQR`
```
If required, the installation directory can be found with
```wolfram
"Location" /. PacletFind["SPQR"][[1]][[1]]
```
### Installation from source
To install from source, clone the git repository to your directory of choice. Then open a Mathematica window and run the command
```wolfram
PacletInstall["/path-to-spqr/build/SPQR-x.x.x.paclet"];
``` 
where ``x-x-x`` is changed to the desired version. The install location directory can be found as above.
### Updating
To update to the latest version, simply rerun the automatic install command above. The package should now be updated to the latest available version.
### Documentation
Upon loading the package, extensive documentation can be accessed via the "Open documentation" button (if running via a GUI). Furthermore, all publicly available functions have documentation pages that can be accessed in an identical manner to standard built in functions. Documentation will also be provided in the accompanying manuscript upon release.
### Uninstallation
To uninstall, open a Mathematica window and run the command
```wolfram
PacletUninstall["SPQR"];
```
