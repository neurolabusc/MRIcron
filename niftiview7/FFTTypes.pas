{ Unit FFTTypes

  This unit provides the basic floating point type for units based
  on this mathematical library.

  TFloat is the basic floating point type and can be set to single
  (4 byte) or double (8 byte), or any other type. Please note that
  processor instructions are well optimised for single and double
  and less for other types.

  Copyright: Nils Haeck M.Sc. (email: n.haeck@simdesign.nl)
  For more information visit http://www.simdesign.nl
  Original date of publication: 10 Mar 2003

  ****************************************************************

  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at:
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
}
unit FFTTypes;

interface

type

  // TFloat is the basic library floating point type. Use single for single
  // precision (4 bytes) or double for more demanding, double precision (8 bytes)
  TFloat = single;

const

  // Single ranges from 1.5 x 10^–45 .. 3.4 x 10^38
  cMinFloat = 1.5E-45;
  cMaxFloat = 3.4E38;

  // Double ranges from 5.0 x 10^–324 .. 1.7 x 10^308
{  cMinFloat = 5.0E-324;
  cMaxFloat = 1.7E308; }

implementation

end.
