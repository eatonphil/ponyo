(*
 *  PONYO_CONTAINER_MAP: This is a high-level interface that allows
 *  the underlying implementation to change over time as its
 *  optimized. The current implementation uses a binary search tree.
 *
 *  Ex:
 *      StringMap = Ponyo_Container_Map (Ponyo_String);
 *      val config = StringMap.empty
 *      val config = StringMap.insert (config, ("PRODUCTION", 1))
 *      val config = StringMap.insert (config, ("DEVELOPMENT", 2))
 *      val develLevel = StringMap.get (config, "DEVELOPMENT")
 *)
signature PONYO_CONTAINER_MAP = PONYO_CONTAINER_TREE_BINARYSEARCH
