type t('slot, 'nextSlots);
let create: unit => t('slot, 'nextSlots);
let use:
  (
    ~default: 'slot,
    (('slot, t('slot2, 'nextSlots))) => 'c,
    t('slot, t('slot2, 'nextSlots))
  ) =>
  'c;