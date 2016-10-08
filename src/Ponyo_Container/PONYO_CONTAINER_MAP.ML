(*
 *  PONYO_CONTAINER_MAP: This is a high-level interface that allows
 *  the underlying implementation to change over time as its
 *  optimized. The current implementation uses a binary search tree.
 *
 *  Ex:
 *      structure String = Ponyo_String;
 *      val config = String.Map.new
 *      val config = String.Map.insert (config, ("PRODUCTION", 1))
 *      val config = String.Map.insert (config, ("DEVELOPMENT", 2))
 *      val develLevel = String.Map.get (config, "DEVELOPMENT")
 *)
signature PONYO_CONTAINER_MAP = PONYO_CONTAINER_TREE_REDBLACK
