signature PONYO_TRAIT_COMPARABLE =
sig
    type t
    val compare : t * t -> order
end
