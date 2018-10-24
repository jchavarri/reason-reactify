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
    | Component(component)
  and renderedElement =
    | RenderedPrimitive(ReconcilerImpl.node)
  and elementWithChildren = (element, childComponents)
  and component = {render: unit => elementWithChildren}
  and childComponents = list(component)
  and instance = {
    component,
    element,
    children: childComponents,
    node: option(ReconcilerImpl.node),
    rootNode: ReconcilerImpl.node,
    mutable childInstances,
  }
  and childInstances = list(instance);

  type container = {
    rootInstance: ref(option(instance)),
    rootNode: ReconcilerImpl.node,
  };

  let createContainer = (rootNode: ReconcilerImpl.node) => {
    let ret: container = {rootNode, rootInstance: ref(None)};
    ret;
  };

  let primitiveComponent = (prim, ~children) => {
    let comp: component = {render: () => (Primitive(prim), children)};
    comp;
  };

  let rec instantiate = (rootNode, component) => {
    let (element, children) = component.render();

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
      List.map(instantiate(nextRootPrimitiveInstance), children);

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
    };

    instance;
  };

  let rec reconcile = (rootNode, instance, component) => {
    let newInstance = instantiate(rootNode, component);

    let r =
      switch (instance) {
      | None =>
        switch (newInstance.node) {
        | Some(n) => ReconcilerImpl.appendChild(rootNode, n)
        | None => ()
        };

        newInstance;
      | Some(i) =>
        let newInstance = instantiate(rootNode, component);

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
                  print_endline("1!");
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
            ReconcilerImpl.appendChild(rootNode, a);
            newInstance;
          | (None, Some(b)) =>
            ReconcilerImpl.removeChild(rootNode, b);
            newInstance;
          | (None, None) => newInstance
          };

        ret;
      };
    r;
  }
  and reconcileChildren = (currentInstance: instance, newInstance: instance) => {
    print_endline("reconcile children");
    let root = currentInstance.rootNode;
    let currentChildInstances = Array.of_list(currentInstance.childInstances);
    let newChildren = Array.of_list(newInstance.children);

    let newChildInstances = ref([]);

    print_endline(
      "new children length: " ++ string_of_int(Array.length(newChildren)),
    );

    for (i in 0 to Array.length(newChildren) - 1) {
      print_endline("reconcile children - iteration: " ++ string_of_int(i));
      let childInstance =
        i >= Array.length(currentChildInstances) ?
          None : Some(currentChildInstances[i]);
      let childComponent = newChildren[i];
      let newChildInstance = reconcile(root, childInstance, childComponent);
      newChildInstances :=
        List.append(newChildInstances^, [newChildInstance]);
    };

    newChildInstances^;
  };

  let updateContainer = (container, component) => {
    let {rootNode, rootInstance} = container;
    let prevInstance = rootInstance^;
    let nextInstance = reconcile(rootNode, prevInstance, component);
    rootInstance := Some(nextInstance);
  };
};
