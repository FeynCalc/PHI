(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: install															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  Installs PHI *)

(* ------------------------------------------------------------------------ *)

InstallPHI::nofc =
"Looks like you don't have FeynCalc installed. PHI cannot work without FeynCalc, so please \
install it first.";

InstallPHI::notcomp =
"Your Mathematica version is too old. PHI requires at least Mathematica 8. Installation aborted!";

InstallPHI::failed =
"Download of `1` failed. Installation aborted!";

AutoOverwritePHIDirectory::usage="AutoOverwritePHIDirectory is an option of InstallPHI. If \
set to True, the existing PHI directory will be deleted without any further notice. The default
value None means that the user will be asked by a dialog. False means that the directory will be overwritten.";

PHIDevelopmentVersionLink::usage="PHIDevelopmentVersionLink is an option of InstallPHI. It specifies the url \
to the main repository of PHI. This repository is used to install the development version of PHI.";

PHIStableVersionLink::usage="PHIStableVersionLink is an option of InstallPHI. It specifies the url \
to the latest stable release of PHI.";

InstallPHIDevelopmentVersion::usage="InstallPHIDevelopmentVersion is an option of InstallPHI. If \
set to True, the installer will download the latest development version of PHI from the git repository. \
Otherwise it will install the latest stable version.";

InstallPHITo::usage="InstallPHITo is an option of InstallPHI. It specifies, the full path \
to the directory where PHI will be installed.";

If[  $VersionNumber == 8,
(*To use FetchURL in MMA8 we need to load URLTools first *)
Needs["Utilities`URLTools`"];
];

If [Needs["FeynCalc`"]===$Failed,
	Message[InstallPHI::nofc];
	Abort[]
];

Options[InstallPHI]={
	AutoOverwritePHIDirectory->None,
	PHIDevelopmentVersionLink->"https://github.com/FeynCalc/PHI/archive/master.zip",
	(* For now there is no stable version *)
	PHIStableVersionLink->"https://github.com/FeynCalc/PHI/archive/master.zip",
	InstallPHIDevelopmentVersion->False,
	InstallPHITo->FileNameJoin[{$FeynCalcDirectory, "AddOns","PHI"}]
};


InstallPHI[OptionsPattern[]]:=
	Module[{	unzipDir, tmpzip, gitzip, packageName, packageDir,
				FCGetUrl, strOverwriteFCdit, zipDir},

	If[OptionValue[InstallPHIDevelopmentVersion],
		gitzip = OptionValue[PHIDevelopmentVersionLink];
		zipDir = "PHI-master",
		(* For now there is no stable version *)
		gitzip = OptionValue[PHIStableVersionLink];
		zipDir = "PHI-master"
	];

	packageName = "PHI";
	packageDir = OptionValue[InstallPHITo];

strOverwriteFCdit="Looks like PHI is already installed. Do you want to replace the content \
of " <> packageDir <> " with the downloaded version of PHI? If you are using any custom configuration \
files or add-ons that are located in that directory, please backup them in advance.";

	If[$VersionNumber < 8,
		Message[InstallPHI::notcomp];
		Abort[]
	];

	If[$VersionNumber == 8,
		(*To use FetchURL in MMA8 we need to load URLTools first *)
		FCGetUrl[x_]:= Utilities`URLTools`FetchURL[x],
		FCGetUrl[x_]:= URLSave[x,CreateTemporary[]]
	];


	(* If the package directory already exists, ask the user about overwriting *)
	If[ DirectoryQ[packageDir],

		If[ OptionValue[AutoOverwritePHIDirectory],

			Quiet@DeleteDirectory[packageDir, DeleteContents -> True],

			Null,
			If[ ChoiceDialog[strOverwriteFCdit,{"Yes, overwrite the " <> packageName <>" directory"->True,
				"No! I need to do a backup first."->False}],
				Quiet@DeleteDirectory[packageDir, DeleteContents -> True],
				Abort[]
			]
		]
	];

	(* Download PHI tarball	*)
	WriteString["stdout", "Downloading PHI from ", gitzip," ..."];
	tmpzip=FCGetUrl[gitzip];
	unzipDir= tmpzip<>".dir";
	WriteString["stdout", "done! \n"];

	(* Extract to the content	*)
	WriteString["stdout", "PHI zip file was saved to ", tmpzip,".\n"];
	WriteString["stdout", "Extracting PHI zip file to ", unzipDir, " ..."];
	ExtractArchive[tmpzip, unzipDir];
	WriteString["stdout", "done! \n"];

	(* Delete the downloaded file	*)
	Quiet@DeleteFile[tmpzip];

	(* Move the files to the final destination	*)
	WriteString["stdout", "Copying "<>packageName<>" to ", packageDir, " ..."];
	Print[FileNameJoin[{unzipDir,zipDir}]];
	CopyDirectory[FileNameJoin[{unzipDir,zipDir}],packageDir];
	WriteString["stdout", "done! \n"];
	(* Delete the extracted archive *)
	Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];

	WriteString["stdout", "done! \n"];

	WriteString["stdout","\nInstallation complete! To load PHI, restart Mathematica \
and evaluate \n\n $LoadAddOns={\"PHI\"}; \n\n before you load FeynCalc; \n"];

];
