# Description

PHI is a FeynCalc add-on that facilitates calculations in Chiral Perturbation Theory (ChPT).
It was originally developed by Frederik Orellana during his PhD at the University of Bern.

PHI is currently developed and maintained by F. Orellana, V. Shtabovenko and R. Mertig.

# Status

The current version of PHI is for testing purposes only. It is not yet fully compatible with the most recent versions of FeynCalc. Some things might work, some won't.

# Requirements

PHI is designed as an add-on for FeynCalc. To use it you need at least FeynCalc 9.3 (development version) and Mathematica 8.

# Installation

## Automatic installation

* Install the latest development version of FeynCalc according to the [instructions](https://github.com/FeynCalc/feyncalc/wiki/Installation#dev_automatic_installation) in the wiki

* Run the following instruction in a Kernel or Notebook session of Mathematica

	```
	Import["https://raw.githubusercontent.com/FeynCalc/PHI/master/install.m"]
	InstallPHI[InstallPHIDevelopmentVersion -> True]
	```

If the above code fails with `URLFetch::invhttp: SSL connect error` (e.g. with Mathematica 9 on macOS), try


```
	ImportString[URLFetch["https://raw.githubusercontent.com/FeynCalc/PHI/master/install.m"]]
	InstallPHI[InstallPHIDevelopmentVersion -> True]
```

## Manual installation


* Create a directory _PHI_ inside

```
	FileNameJoin[{$UserBaseDirectory, "Applications", "FeynCalc", "AddOns"}]
```

and put the source code there.

# Usage

For usage information see [here](http://frederik.orellana.dk/wp-content/files/phi_examples_html).
Notice that not all code examples are going to work, most of them probably won't :(

# Acknowledgments

If you use PHI in your work, please cite

* F. Orellana, doctoral dissertation, University of Bern, 2003
* F. Orellana, R. Mertig and V. Shtabovenko, in preparation

# License

* The code of PHI is licensed under GNU GPLv3.
