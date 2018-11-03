/*
 * Interface to bridge a React-style functional API
 * with a mutable back-end. Similiar in spirit
 * to the reconciler interface provided via 'react-conciler'.
 */
module type Reconciler = {
  /*
      Primitives is a variant type describing metadata needed
      to instantiate something in the mutable world.
   */
  type primitives;

  /*
      A node is a live instance representing a mutable object
   */
  type node;

  let appendChild: (node, node) => unit;

  let createInstance: primitives => node;

  let replaceChild: (node, node, node) => unit;

  let removeChild: (node, node) => unit;

  let updateInstance: (node, primitives) => unit;
};

module Make = (ReconcilerImpl: Reconciler) => {
  type element =
    | Primitive(ReconcilerImpl.primitives)
    | Component
  and renderedElement =
    | RenderedPrimitive(ReconcilerImpl.node)
  and elementWithChildren = (element, childComponents, list(effect))
  /*
     A component is our JSX primitive element - just an object
     with a render method.
     TODO: Can we clean this interface up and just make component
     a function of type unit => elementWithChildren ?
   */
  and component = {render: unit => elementWithChildren}
  and childComponents = list(component)
  /*
      An instance is a component that has been rendered.
      We store some additional context for it, like the state,
      effects that need to be run, and corresponding nodes.
   */
  and instance = {
    component,
    element,
    children: childComponents,
    node: option(ReconcilerImpl.node),
    rootNode: ReconcilerImpl.node,
    mutable childInstances,
    mutable effectInstances: Effects.effectInstances,
    state: State.HeterogenousMutableList.t,
  }
  and childInstances = list(instance)
  /*
     An effectInstance is an effect that was already instantiated -
     it's an effect we'll have to run when the element is unmounted
   */
  and effectInstance = unit => unit
  and effectInstances = list(effectInstance)
  /* An effect is a function sent to useEffect. We haven't run it yet, */
  /* But we will once the element is mounted */
  and effect = unit => effectInstance;

  /*
    A global, non-pure container to hold effects
    during the course of a render operation.
  */
  let __globalEffects = Effects.create();

  /*
    State management for reconciliation
  */
  module ComponentStateContext {
    type t = instance;
  };
  module ComponentState = State.Make(ComponentStateContext);
  let __globalState = ref(ComponentState.create([]));

  /* 
    Container API
  */
  type container = {
    rootInstance: ref(option(instance)),
    rootNode: ReconcilerImpl.node,
  };

  let createContainer = (rootNode: ReconcilerImpl.node) => {
    let ret: container = {rootNode, rootInstance: ref(None)};
    ret;
  };

  type componentFunction = unit => component;

  let component = (~children=[], c: componentFunction) => {
    let ret: component = {
      render: () => {
        Effects.resetEffects(__globalEffects);
        let children: list(component) = [c()];
        let effects = Effects.getEffects(__globalEffects);
        let renderResult: elementWithChildren = (
          Component,
          children,
          effects,
        );
        renderResult;
      },
    };
    ret;
  };

  let primitiveComponent = (prim, ~children) => {
    let comp: component = {render: () => (Primitive(prim), children, [])};
    comp;
  };

  let useEffect = (e: effect) => Effects.addEffect(__globalEffects, e);

  exception TodoException;

  let _getEffectsFromInstance = (instance: option(instance)) =>
    switch (instance) {
    | None => []
    | Some(i) => i.effectInstances
    };

  let _getCurrentStateFromInstance = (instance: option(instance)) =>
    switch (instance) {
    | None => []
    | Some(i) => i.state
    };

  /*
   * Instantiate turns a component function into a live instance,
   * and asks the reconciler to append it to the root node.
   */
  let rec instantiate =
          (
            rootNode,
            previousInstance: option(instance),
            component: component,
          ) => {
    /* Recycle any previous effect instances */
    let previousEffectInstances = _getEffectsFromInstance(previousInstance);
    Effects.runEffectInstances(previousEffectInstances);
    let printState = (state: State.HeterogenousMutableList.t) => {
        let printItem = (i: ref(State.Object.t)) => {
            let item = i^;
            let intItem: int = State.Object.of_object(item);
            print_endline("Item: " ++ string_of_int(intItem));
        };
        List.iter(printItem, state);
    };


    /* Set up state for the component */
    let previousState = _getCurrentStateFromInstance(previousInstance);
    print_endline ("** OLD STATE **");
    printState(previousState);
    let stateInstance = ref(previousInstance);
    print_endline("Previous state lengtH: " ++ string_of_int(List.length(previousState)));
    let state = ComponentState.create(previousState);
    let context = ComponentState.getCurrentContext(state);
    __globalState := state;
    let (element, children, effects) = component.render();
    let newState = ComponentState.getNewState(state);

    printState(newState);

    /* TODO: Should this be deferred until we actually mount the component? */
    let effectInstances = Effects.runEffects(effects);

    let primitiveInstance =
      switch (element) {
      | Primitive(p) => Some(ReconcilerImpl.createInstance(p))
      | _ => None
      };

    let nextRootPrimitiveInstance =
      switch (primitiveInstance) {
      | Some(i) => i
      | None => rootNode
      };

    let childInstances =
      List.map(instantiate(nextRootPrimitiveInstance, None), children);

    let appendIfInstance = ci =>
      switch (ci.node) {
      | Some(s) => ReconcilerImpl.appendChild(nextRootPrimitiveInstance, s)
      | _ => ()
      };

    List.iter(appendIfInstance, childInstances);

    let instance = {
      component,
      element,
      node: primitiveInstance,
      rootNode: nextRootPrimitiveInstance,
      children,
      childInstances,
      effectInstances,
      state: newState,
    };

    context := Some(instance);

    instance;
  };

  let rec getFirstNode = (node: instance) =>
    switch (node.node) {
    | Some(n) => Some(n)
    | None =>
      switch (node.childInstances) {
      | [] => None
      | [c] => getFirstNode(c)
      | _ => None
      }
    };

  let rec reconcile = (rootNode, instance, component) => {
    let newInstance = instantiate(rootNode, instance, component);

    let r =
      switch (instance) {
      | None =>
        switch (newInstance.node) {
        | Some(n) => ReconcilerImpl.appendChild(rootNode, n)
        | None => ()
        };

        newInstance;
      | Some(i) =>
        let ret =
          switch (newInstance.node, i.node) {
          | (Some(a), Some(b)) =>
            /* Only both replacing node if the primitives are different */
            switch (newInstance.element, i.element) {
            | (Primitive(oldPrim), Primitive(newPrim)) =>
              if (oldPrim != newPrim) {
                /* Check if the primitive type is the same - if it is, we can simply update the node */
                /* If not, we'll replace the node */
                if (Utility.areConstructorsEqual(oldPrim, newPrim)) {
                  switch (newInstance.element) {
                  | Primitive(o) =>
                    ReconcilerImpl.updateInstance(b, o);
                    i.childInstances = reconcileChildren(i, newInstance);
                    i;
                  | _ =>
                    print_endline(
                      "ERROR: We shouldn't hit this condition! If there is a node, there should be a related primitive element.",
                    );
                    newInstance;
                  };
                } else {
                  ReconcilerImpl.replaceChild(rootNode, a, b);
                  newInstance;
                };
              } else {
                /* The node itself is unchanged, so we'll just reconcile the children */
                i.childInstances = reconcileChildren(i, newInstance);
                i;
              }
            | _ =>
              print_endline(
                "ERROR: Should only be nodes if there are primitives!",
              );
              newInstance;
            }
          | (Some(a), None) =>
            /* If there was a non-primitive instance, we need to get the top-level node - */
            /* and then remove it */
            let currentNode = getFirstNode(i);
            switch (currentNode) {
            | Some(c) => ReconcilerImpl.removeChild(rootNode, c)
            | _ => ()
            };
            ReconcilerImpl.appendChild(rootNode, a);
            newInstance;
          | (None, Some(b)) =>
            ReconcilerImpl.removeChild(rootNode, b);
            newInstance;
          | (None, None) =>
            switch (getFirstNode(i), getFirstNode(newInstance)) {
            | (Some(a), Some(b)) => ReconcilerImpl.removeChild(rootNode, a)
            | _ => ()
            };
            newInstance;
          };

        ret;
      };
    r;
  }
  and reconcileChildren = (currentInstance: instance, newInstance: instance) => {
    let root = currentInstance.rootNode;
    let currentChildInstances = Array.of_list(currentInstance.childInstances);
    let newChildren = Array.of_list(newInstance.children);

    let newChildInstances = ref([]);

    for (i in 0 to Array.length(newChildren) - 1) {
      let childInstance =
        i >= Array.length(currentChildInstances) ?
          None : Some(currentChildInstances[i]);
      let childComponent = newChildren[i];
      let newChildInstance = reconcile(root, childInstance, childComponent);
      newChildInstances :=
        List.append(newChildInstances^, [newChildInstance]);
    };

    /* Clean up existing children */
    for (i in
         Array.length(newChildren) to
         Array.length(currentChildInstances) - 1) {
      switch (currentChildInstances[i].node) {
      | Some(n) => ReconcilerImpl.removeChild(root, n)
      | _ => ()
      };
    };

    newChildInstances^;
  };

  let useState = (v: 't) => {

    let state = __globalState^;
    let n = ComponentState.popOldState(state, v);

    let updateFunction = ComponentState.pushNewState(state, n);

    /* let updateFunction = (_n) => { (); }; */

    let currentContext = ComponentState.getCurrentContext(state);

    let setState = (context: ref(option(instance)), newVal: 't) => {
      print_endline("UPDATE: " ++ string_of_int(Obj.magic(newVal)));
      updateFunction(newVal);
      switch (context^) {
      | Some(i) =>
        let {rootNode, component} = i;
        reconcile(rootNode, Some(i), component);
        ();
      | _ => print_endline("WARNING: Skipping reconcile!")
      };
    };

    (n, setState(currentContext));
  };

  let updateContainer = (container, component) => {
    let {rootNode, rootInstance} = container;
    let prevInstance = rootInstance^;
    let nextInstance = reconcile(rootNode, prevInstance, component);
    rootInstance := Some(nextInstance);
  };
};

module State = State;
