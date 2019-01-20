(*
 *  PONYO_CONTAINER_MAP: This is a high-level map that allows
 *  the underlying implementation to change over time as its
 *  optimized. The current implementation uses a red-black tree.
 *
 *  Ex:
 *      local
 *	    open Ponyo.String
 *      in
 *          val config = Map.new
 *          val config = Map.insert config "PRODUCTION" 1
 *          val config = Map.insert config "DEVELOPMENT" 2
 *          val develLevel = Map.get config "DEVELOPMENT"
 *	end
 *)
signature PONYO_CONTAINER_MAP = PONYO_CONTAINER_TREE_REDBLACK
