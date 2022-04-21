(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PHI																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  Physics of Hadronic Interactions - Tools for ChPT				*)

(* ------------------------------------------------------------------------ *)


$PHIVersion::usage=
"$PHIVersion is the string that represents the version of PHI";

$PHIDirectory::usage=
"$PHIDirectory is the string that represents the full path to the PHI \
directory";

$Lagrangians::usage =
"$Lagrangians is a list of the lagrangians loaded (without heads \
Lagrangian).  Setting $Lagrangians to a list of lagrangians in PhiStart.m \
causes these lagrangians to be loaded at startup.";

$Configuration::usage =
"$Configuration is a string variable determining which configuration is loaded \
at startup or restart.  It can be set before loading FeynCalc, in PhiStart.m or with the \
configurations palette.  If the palette is used for restarting, the setting \
in PhiStart.m is overruled.  Possible values include \"ChPT2\" and \"QED\".  Default \
value : \"None\".";

$PaletteConfiguration::usage =
"$PaletteConfiguration is a string variable set when clicking on the \
configuration palette and overruling the setting of $Configuration.  Default \
value : \"None\".";

Begin["`Package`"]
End[]

Begin["`PHI`Private`"];

$PHIVersion="1.3";

$PHIDirectory =
	ToFileName[{$FeynCalcDirectory, "AddOns", "PHI"}];

Off[General::shdw];

If[ !ValueQ[Global`$Configuration],
	FeynCalc`$Configuration = "None",
	FeynCalc`$Configuration = Global`$Configuration
];
Remove[Global`$Configuration];

If[ !ValueQ[Global`$PaletteConfiguration],
	FeynCalc`$PaletteConfiguration = "None",
	FeynCalc`$PaletteConfiguration = Global`$PaletteConfiguration
];
Remove[Global`$PaletteConfiguration];

If[ !ValueQ[Global`$Lagrangians],
	FeynCalc`$Lagrangians = {},
	FeynCalc`$Lagrangians = Global`$Lagrangians
];
Remove[Global`$Lagrangians];

On[General::shdw];

tmp`phiFiles = Map[ToFileName[{$PHIDirectory,"Shared"}, (# <> ".m")] &,
	{"Objects", "Couplings", "Channels", "Utilities", "Renormalization", "Palettes"}];

(* Load the .m files *)
BeginPackage["FeynCalc`",{"JLink`"}];

FCDeclareHeader[FileNameJoin[{$PHIDirectory,"First.m"}]];
Get[FileNameJoin[{$PHIDirectory,"First.m"}]];

FCDeclareHeader/@tmp`phiFiles;
Get/@tmp`phiFiles;

Remove[tmp`phiFiles];

(* Loading of user definitions *)

FCPrint[2,"Loading Phi`PhiStart`"];

FCDeclareHeader[FileNameJoin[{$PHIDirectory,"PhiStart.m"}]];
Get[FileNameJoin[{$PHIDirectory,"PhiStart.m"}]];

(*Clean out doubles (strings and non-strings)  in $Lagrangians*)
$Lagrangians = Union[(ToExpression[#[[0]]] @@ #) & /@ $Lagrangians];


(* ************************************************************** *)

(* Update particles *)

FAUpdate;

(* ************************************************************** *)

(* FeynArts definitions are cleared to avoid error messages *)
(*
If[ NumberQ[FeynArts`$FeynArts],
	ClearAll[FeynArts`Greek, FeynArts`UCGreek],
	Remove[FeynArts`$FeynArts]
];*)



EndPackage[]



(* Print startup message *)
If[ $FeynCalcStartupMessages =!= False,
	Print[Style["PHI ", "Text", Bold], Style[$PHIVersion <> " loaded.", "Text"]];
	Print[ Style["Have a look at the supplied ","Text"],

	Style[DisplayForm@ButtonBox["examples.", BaseStyle -> "Hyperlink",	ButtonFunction :>
							SystemOpen[FileNameJoin[{$PHIDirectory, "Examples"}]],
							Evaluator -> Automatic, Method -> "Preemptive"], "Text"],
	Style[" If you use PHI in your research, please cite","Text"]];
	Print [Style[" \[Bullet] F. Orellana, doctoral dissertation, University of Bern, 2003","Text"]];
	Print [Style[" \[Bullet] F. Orellana, R. Mertig and V. Shtabovenko, in preparation","Text"]];
];

End[]


