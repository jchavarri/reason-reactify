module State = Reactify.State;
module Object = State.Object;

open TestUtility;

test("object", () => {
    test("Object conversion works with ints", () => {
        let i = 1;
        let stateI = Object.to_object(i);
        let rehydratedI = Object.of_object(stateI);
        assert(i == rehydratedI);
    });

    test("Object conversion works with tuples", () => {
        let p = (1, "a");
        let stateP = Object.to_object(p);
        let rehydratedP = Object.of_object(stateP);
        assert(p == rehydratedP);
    });

    test("Object can be used to create list of different types", () => {
        let stateList: list(Object.t) = [];

        let stateList = [Object.to_object(1), ...stateList];
        let stateList = [Object.to_object(("a", "b")), ...stateList];

        let firstElement = List.nth(stateList, 0);
        assert(Object.of_object(firstElement) == ("a", "b"));

        let lastElement = List.nth(stateList, 1);
        assert(Object.of_object(lastElement) == 1);
    });
});

/* test("Make", () => { */
/*     module TestModule { */
/*         type t = int; */
/*     }; */

/*     module TestState = State.Make(TestModule); */

/*     test("held ref gets updated", () => { */
/*         let state = TestState.create(); */
/*         TestState.start(state, []); */

/*         let ctx = TestState.getCurrentContext(state); */
/*         TestState.finalize(state, 5); */

/*         switch (ctx^) { */
/*         | Some(i) => assert(i == 5) */
/*         | None => assert(false) */
/*         }; */
/*     }); */
/* }); */
