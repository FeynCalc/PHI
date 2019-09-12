(* ************************************************************** *)
(*

This file is read before anything else.  Put here definitions
to be read before the subpackages.

E.g.:
When using ArgumentsSupply on objects that are defined in a model
file but are in context Phi`Objects`, it is
necessary to define these objects before Objects.m is read.

*)
(* ************************************************************** *)

(* Definitions used by the model PiN2 *)

Phi`Objects`CovariantNucleonFieldDerivative::usage =
"CovariantNucleonFieldDerivative is the nucleon covariant derivative of J. Gasser, \
M. E. Sainio and A. Svarc (1988), Nucl. Phys, B307, 779-853";

Phi`Objects`CNDr::usage =
"CNDr is the shorthand notation for CovariantNucleonFieldDerivative, which is the nucleon \
covariant derivative of J. Gasser, M. E. Sainio and A. Svarc (1988), Nucl. Phys, B307, 779-853";

Options[CovariantFieldDerivative] = {
	Explicit->True,
	DiagonalToU->True,
	SUNN->2,
	UDimension->Automatic
};
