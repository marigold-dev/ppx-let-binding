open Ppxlib

let make_let_extension (ext, substitution) =
  let expand ~ctxt expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    match expr with
    | [%expr
        let [%p? var] = [%e? let_expr] in
        [%e? body]] ->
      substitution loc var let_expr body (*  *)
    | _ ->
      Location.raise_errorf ~loc
        "Extension %%%s can only be used with a let, e.g. let%%%s a = b in ..."
        ext ext in
  let extension =
    Extension.V3.declare ext Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      expand in
  let rule = Ppxlib.Context_free.Rule.extension extension in
  Driver.register_transformation ~rules:[rule] ext

let () =
  List.iter make_let_extension
    [
      ( "await",
        fun loc var let_expr body ->
          [%expr Lwt.bind [%e let_expr] (fun [%p var] -> [%e body])] );
      ( "some",
        fun loc var let_expr body ->
          [%expr Option.bind [%e let_expr] (fun [%p var] -> [%e body])] );
      ( "default",
        fun loc _var let_expr body ->
          [%expr Option.value ~default:[%e let_expr] [%e body]] );
      ( "ok",
        fun loc var let_expr body ->
          [%expr Result.bind [%e let_expr] (fun [%p var] -> [%e body])] );
      ( "assert",
        fun loc _var let_expr body ->
          [%expr
            let message, b = [%e let_expr] in
            if b then [%e body] else Error message] );
      ( "await_ok",
        fun loc var let_expr body ->
          [%expr
            let err_to_exn, promise = [%e let_expr] in
            Lwt.bind promise (fun result ->
                match result with
                | Ok [%p var] -> [%e body]
                | Error err -> err_to_exn err |> Lwt.fail)] );
    ]
