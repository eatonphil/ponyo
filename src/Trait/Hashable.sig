signature PONYO_TRAIT_HASHABLE =
sig
    include PONYO_TRAIT_COMPARABLE
    val hash : t -> Word64.word
    val unitialized : t
end
