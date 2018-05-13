fun cleanPath (path: string, right: bool) : string =
    if right andalso String.isSuffix "/" path
        then cleanPath (String.substring (path, 0, String.size path - 1), true)
    else if not right andalso String.isPrefix "/" path
        then cleanPath (String.extract (path, 1, NONE), false)
    else path;

val PONYO_ROOT =
    case OS.Process.getEnv "PONYO_ROOT" of
        NONE => (print "PONYO_ROOT must be set. (Directory of source is a good default.)\n"; raise Fail "")
      | SOME root =>
    case String.isPrefix "~/" root of
        false => root
      | true =>
    case OS.Process.getEnv "HOME" of
        NONE => (print "Bad PONYO_ROOT: HOME undefined.\n"; raise Fail "")
      | SOME home =>
          cleanPath(home, true) ^ "/" ^ (cleanPath (String.extract (root, 2, NONE), false));

val oldUse = use;
fun use (path: string) = oldUse (PONYO_ROOT ^ "/" ^ path);
use "src/build.sml";
