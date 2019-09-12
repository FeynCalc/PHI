(* *************************************************************** *)
(*                                                                 *)
(*                      QED2                                       *)
(*                                                                 *)
(* *************************************************************** *)

(*
Author:              F.Orellana

Year:                1998

Mathematica Version: 3.0

Requirements:        FeynCalc > 3, PHI

Summary:             Lagrangian for PHI

Description:         Counterterm QED lagrangian
						for the electron.

						Taken from S .Weinberg (1995),
						"The Quantum Theory of Fields",
						Cambridge University Press

						Adapted to the usual space-
						time metric (1,-1,-1,-1)
*)


Begin["Phi`Objects`"];

(* --------------------------------------------------------------- *)

QED2::usage =
"QED2.m is the name of the file containing the definitions for
Lagrangian[QED[2]], which is the QED counterterm lagrangian.";

DM::usage =
"DM := CouplingConstant[QED[2],1] is one of the constants of the
counterterm QED lagrangian - the mass counterterm.";

Z2::usage =
"Z2 := CouplingConstant[QED[2],2] is one of the constants of the
counterterm QED lagrangian - the factor relating the bare to the
physical electron field.";

Z3::usage =
"Z3 := CouplingConstant[QED[2],3] is one of the constants of the
counterterm QED lagrangian - the factor relating the bare to the
physical photon field.";

(* --------------------------------------------------------------- *)

End[];

(* --------------------------------------------------------------- *)

(* Box definitions *)

CouplingConstant/:
MakeBoxes[
	CouplingConstant[
QED[2],1,st___RenormalizationState,
	  sc___RenormalizationScheme,qs___ExpansionState],
	TraditionalForm]:=
	SuperscriptBox[
	MakeBoxes[StyleForm["\[Delta]m",FontSlant->"Italic"]][[1]],
	RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
		  MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
		  MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant/:
MakeBoxes[
	CouplingConstant[
QED[2],2,st___RenormalizationState,
	  sc___RenormalizationScheme,qs___ExpansionState],
	TraditionalForm]:=
SubsuperscriptBox[MakeBoxes[StyleForm["Z",FontSlant->"Italic"]][[1]],
	MakeBoxes[TraditionalForm[2]],
	RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
		  MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
		  MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant/:
MakeBoxes[
	CouplingConstant[
QED[2],3,st___RenormalizationState,
	  sc___RenormalizationScheme,qs___ExpansionState],
	TraditionalForm]:=
SubsuperscriptBox[MakeBoxes[StyleForm["Z",FontSlant->"Italic"]][[1]],
	MakeBoxes[TraditionalForm[3]],
	RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},{
		  MakeBoxes[TraditionalForm[IndexBox[sc]]]},{
		  MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

(* --------------------------------------------------------------- *)

(* Abbreviations *)

DM = CouplingConstant[QED[2],1];
Z2 = CouplingConstant[QED[2],2];
Z3 = CouplingConstant[QED[2],3];

(* --------------------------------------------------------------- *)

Lagrangian[QED[2]] :=
	-1/4*(Z3[0]-1)*
	DOT[FieldStrengthTensor[LorentzIndex[\[Mu]],
	QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]],
	FieldStrengthTensor[LorentzIndex[\[Mu]],
	QuantumField[Particle[Photon],LorentzIndex[\[Nu]]]]]+

	(Z2[0]-1)*
	(DOT[
	DiracBar[QuantumField[Particle[Electron]]],
	DiracMatrix[LorentzIndex[\[Mu]]],
	(I*QuantumField[FCPartialD[LorentzIndex[\[Mu]]],Particle[Electron]]+
	CouplingConstant[QED[1]]*
	DOT[QuantumField[Particle[Photon],LorentzIndex[\[Mu]]],
	QuantumField[Particle[Electron]]])
	]-

	ParticleMass[Electron]*
	DOT[DiracBar[QuantumField[Particle[Electron]]],
	QuantumField[Particle[Electron]]])-

	Z2[0]*DM[0]*
	DOT[DiracBar[QuantumField[Particle[Electron]]],
	QuantumField[Particle[Electron]]];

(* --------------------------------------------------------------- *)

FieldsSet[QED[2]] :=
	{QuantumField[Particle[Electron,RenormalizationState[0]]],
	QuantumField[Particle[Photon,RenormalizationState[0]],LorentzIndex[\[Mu]]]};

$Lagrangians = Union[$Lagrangians,{QED[2]}];
