(* snip_gc_report: allocation accounting for `snip measure`.
 *
 * `snip measure` links this module with every snippet binary it
 * builds (ocamlopt -O2 -o <bin> snip_gc_report.ml <snippet>.ml, into
 * _build/bin-measure/ so `snip test` binaries stay pristine). At
 * normal program exit the handler reports total allocation volume
 * on stderr:
 *
 *   SNIP_ALLOCATED_BYTES=<bytes>
 *
 * where <bytes> is Gc.allocated_bytes: total minor+major heap words
 * allocated since startup, converted to bytes by the runtime and
 * returned as a float (exact below 2^53). The harness parses the
 * marker from the SAME Pin run that yields pin_instructions, so one
 * instrumented run produces both signals.
 *
 * Stderr is deliberate: snippet stdout stays byte-identical for the
 * harness output comparison. at_exit runs on normal exit and on
 * explicit exit calls; crashes and timeouts fail the case before
 * any marker matters.
 *
 * Perturbation: module init plus one eprintf at exit shift Pin
 * instruction counts by a small deterministic constant, identical
 * for every snippet (report tool_version 3 marks the break).
 *)

let () =
  at_exit (fun () ->
      Printf.eprintf "SNIP_ALLOCATED_BYTES=%.0f\n%!" (Gc.allocated_bytes ()))
