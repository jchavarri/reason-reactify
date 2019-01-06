  type t('slot, 'nextSlots) = ref(option(('slot, 'nextSlots)));
  let create = () => ref(None);
  let use = (~default, continuation, slots: t(_)) => {
    switch (slots^) {
    | None =>
      let slot = default;
      let nextSlots = create();
      slots := Some((slot, nextSlots));
      continuation(slot, nextSlots);
    | Some((slot, nextSlots)) => continuation(slot, nextSlots)
    };
  };