structure Basis = struct
    structure String = String
    structure List = List
    structure Os = OS
    structure Posix = Posix
    structure Socket = Socket
end;

fun cleanPath (path: string, right: bool) : string =
    if right andalso Basis.String.isSuffix "/" path
        then cleanPath (Basis.String.substring (path, 0, String.size path - 1), true)
    else if not right andalso Basis.String.isPrefix "/" path
        then cleanPath (Basis.String.extract (path, 1, NONE), false)
    else path;

val PONYO_ROOT =
    case Basis.Os.Process.getEnv "PONYO_ROOT" of
        NONE => (print "PONYO_ROOT must be set. (Directory of source is a good default.)\n"; raise Fail "")
      | SOME root =>
    case Basis.String.isPrefix "~/" root of
        false => root
      | true =>
    case Basis.Os.Process.getEnv "HOME" of
        NONE => (print "Bad PONYO_ROOT: HOME undefined.\n"; raise Fail "")
      | SOME home =>
          cleanPath(home, true) ^ "/" ^ (cleanPath (Basis.String.extract (root, 2, NONE), false));

val ponyoLib = PONYO_ROOT ^ "/src";

PolyML.make (ponyoLib)
