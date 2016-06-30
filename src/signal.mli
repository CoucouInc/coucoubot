(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Basic signal} *)

type 'a t
  (** Signal of type 'a *)

val create : unit -> 'a t
  (** New signal *)

val send : 'a t -> 'a -> unit Lwt.t
  (** Trigger the signal *)

type handler_response =
  | ContinueListening
  | StopListening

val on : 'a t -> ('a -> handler_response Lwt.t) -> unit
  (** Register a handler to the signal; the handler returns [ContinueListening]
      if it wants to continue being notified, [StopListening] otherwise *)

val on' : 'a t -> ('a -> 'b Lwt.t) -> unit

val once : 'a t -> ('a -> 'b Lwt.t) -> unit
  (** Register a handler to be called only once *)

val propagate : 'a t -> 'a t -> unit
  (** [propagate a b] propagates all values of [a] into [b]. Cycles
      are not detected. *)

(** {2 Combinators} *)

val map : 'a t -> ('a -> 'b) -> 'b t

val filter : 'a t -> ('a -> bool) -> 'a t

val filter_map : 'a t -> ('a -> 'b option) -> 'b t

val set_exn_handler : (exn -> unit) -> unit
  (** Set the handler that is called upon an exception in
      a Signal.  The default handler does nothing.
      If the handler raises an exception, it is not caught! *)
