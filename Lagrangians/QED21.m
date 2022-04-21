(* *************************************************************** *)
(*                                                                 *)
(*                      QED21                                      *)
(*                                                                 *)
(* *************************************************************** *)

(*
Author:              F.Orellana

Year:                1998

Mathematica Version: 3.0

Requirements:        FeynCalc > 3, PHI

Summary:             Lagrangian for PHI

Description:         The standard QED lagrangian
						for three leptons.

						Taken from Bjoerken and Drell,
						"Relativistic Quantum Fields",
						McGraw-Hill 1965
*)

QED21::usage =
"QED21.m is the name of the file containing the definitions for
Lagrangian[QED2[1]], which is the standard QED lagrangian,
CouplingConstant[QED2[1]] is the bare
unit charge (the charge of the positron).";

(* --------------------------------------------------------------- *)

Begin["`Package`"]
End[]


Begin["`QED21`Private`"]

(* --------------------------------------------------------------- *)

Lagrangian["QED2"] :=
	Lagrangian[QED2[1]];

(* --------------------------------------------------------------- *)

Lagrangian[QED2[1]] :=
	-1/4*
	DOT[FieldStrengthTensor[LorentzIndex[Global`\[Mu]],
	QuantumField[Particle[Photon],LorentzIndex[Global`\[Nu]]]],
	FieldStrengthTensor[LorentzIndex[Global`\[Mu]],
	QuantumField[Particle[Photon],LorentzIndex[Global`\[Nu]]]]]+

	DOT[
	DiracBar[QuantumField[Particle[Lepton],SUNIndex[i]]],
	DiracMatrix[LorentzIndex[Global`\[Mu]]],
	(I*QuantumField[FCPartialD[LorentzIndex[Global`\[Mu]]],
	Particle[Lepton],SUNIndex[i]]+
	CouplingConstant[QED[1]]*
	DOT[QuantumField[Particle[Photon],LorentzIndex[Global`\[Mu]]],
	QuantumField[Particle[Lepton],SUNIndex[i]]])
	]-

	ParticleMass[Lepton,SUNIndex[i]]*
	DOT[DiracBar[QuantumField[Particle[Lepton],SUNIndex[i]]],
	QuantumField[Particle[Lepton],SUNIndex[i]]];

(* --------------------------------------------------------------- *)

FieldsSet[QED2[1]] :=
	{QuantumField[Particle[Lepton]],
	QuantumField[Particle[Photon],LorentzIndex[Global`\[Mu]]]};

$Lagrangians = Union[$Lagrangians,{QED2[1]}];

End[]
