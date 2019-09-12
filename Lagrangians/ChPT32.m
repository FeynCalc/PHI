(* *************************************************************** *)
(*                                                                 *)
(*                      ChPT32                                     *)
(*                                                                 *)
(* *************************************************************** *)

(*
Author:              F.Orellana

Year:                1997

Mathematica Version: 3.0

Requirements:        FeynCalc > 3, PHI

Summary:             Lagrangian for PHI

Description:         The simplest ChPT lagrangian.

						Taken from J.F. Donoghue, E. Golowich
						and B.R. Holstein (1992), "Dynamics of
						the Standard Model", Cambridge
*)

(* --------------------------------------------------------------- *)

ChPT32::usage =
"ChPT32.m is the name of the file containing the definitions for
Lagrangian[ChPT3[2]], which is the SU(3) lowest order ChPT
lagrangian.  To evaluate use ArgumentsSupply.";

(* --------------------------------------------------------------- *)

Begin["`Package`"]
End[]


Begin["`ChPT32`Private`"]

(* --------------------------------------------------------------- *)

Lagrangian[ChPT3[2]] :=
	1/4*DecayConstant[PhiMeson,RenormalizationState[0]]^2*

	(UTrace[ NM[CDr[MM,{\[Mu]}],Adjoint[CDr[MM,{\[Mu]}]]] ] +

	UTrace[ NM[UChiMatrix,Adjoint[MM]]+NM[MM,Adjoint[UChiMatrix]] ]);

(* --------------------------------------------------------------- *)

FieldsSet[ChPT3[2]] :=
	{IsoVector[
	QuantumField[Particle[PhiMeson,RenormalizationState[0]]]
	]};

$Lagrangians = Union[$Lagrangians,{ChPT3[2]}];

End[]
