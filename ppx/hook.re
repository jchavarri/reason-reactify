open Migrate_parsetree;
open OCaml_406.Ast;
open Ast_mapper;
open Asttypes;
open Parsetree;

let mapper =
  /* We override the expr mapper to catch hook.  */
  {
    ...default_mapper,
    expr: (mapper, outer_expr) =>
      switch (outer_expr) {
      | [%expr [%hook [%e? expr]]] =>
        /* Matches "hook"-annotated expressions. */
        switch (expr.pexp_desc) {
        | Pexp_let(Nonrecursive, valueBindings, body) =>
          /* This is a let%hook expression!  It's of the form
                  let%hook $p1 = $e1(...) and ... and $pn = $en(...); $e0
                and we want it to take the form
                  $e1(($p1) => ... $en(($pn) => ... $e0) ...);
             */
          let rec hookWrap = valueBindings' =>
            switch (valueBindings') {
            | [
                {
                  pvb_pat: hookPattern,
                  pvb_expr: hookExpr,
                  pvb_attributes: [],
                  pvb_loc: hookLoc,
                },
                ...valueBindings'',
              ] =>
              switch (hookExpr) {
              | {pexp_desc: Pexp_apply(fn, args), pexp_attributes, pexp_loc} =>
                /* Recurse and then wrap the resulting body. */
                let body' = hookWrap(valueBindings'');
                let contFunction =
                  [@metaloc expr.pexp_loc]
                  [%expr (([%p hookPattern]) => [%e body'])];
                mapper.expr(
                  mapper,
                  {
                    pexp_desc:
                      Pexp_apply(fn, args @ [(Nolabel, contFunction)]),
                    pexp_attributes,
                    pexp_loc,
                  },
                );
              | _ =>
                raise(
                  Location.Error(
                    Location.error(
                      ~loc=hookLoc,
                      "let%hook can only be used with hooks declared in reason-reactify: `useState`, `useReducer`, `useEffect`",
                    ),
                  ),
                )
              }

            | _ =>
              /* Nothing left to do.  Just return the body. */
              mapper.expr(mapper, body)
            };

          hookWrap(valueBindings);
        | _ => default_mapper.expr(mapper, outer_expr)
        }
      | _ => default_mapper.expr(mapper, outer_expr)
      },
  };

let () =
  Driver.register(~name="ppx-hook", Versions.ocaml_406, (_config, _cookies) =>
    mapper
  );