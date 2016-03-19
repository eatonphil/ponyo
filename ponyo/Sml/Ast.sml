structure Ast =
struct
    datatype e =
        StringExp of Token.t
      | NumberExp of Token.t
      | ValueExp  of Token.t

    datatype ty =
        NoType
      | IdentType       of Token.t
      | RecordType      of Token.t
      | ProductType     of ty list
      | FunctionType    of ty * ty
      | ConstructedType of ty list * ty

    datatype t =
        Empty
      | Root            of t list
      | SignatureBody   of t list
      | Signature       of Token.t * t
      | StructureBody   of t list
      | Structure       of Token.t * t
      | Function        of Token.t * t * t
      | FunctionDec     of Token.t * t
      | Value           of Token.t * t * t
      | ValueDec        of Token.t * t
      | Expression      of e
      | Type            of ty

    fun insert (ast: t, child: t) : t =
        case ast of
            Root children          => Root (children @ [child])
          | SignatureBody children => SignatureBody (children @ [child])
          | StructureBody children => StructureBody (children @ [child])
          | _ => raise Fail "Unexpected parent type."

    fun tyToString (ty: ty) : string =
        case ty of
            IdentType ident => Token.toString ident
          | ProductType tys => "(" ^ (String.join (map tyToString tys, " * ")) ^ ")"
          | FunctionType (inTy, outTy) => Format.sprintf "% -> %" (map tyToString [inTy, outTy])
          | ConstructedType (tys, ty) => (
              case tys of
                  [ty] => tyToString ty
                | _    => String.join (map tyToString tys, ", ")
          ) ^ " " ^ (tyToString ty)
          | NoType => "unknown"
          | _ => " "

    fun print (ast: t) =
        case ast of
            Root children => (map print children; ())
          | Structure (name, body) => (Format.printf "Parsed structure: %\n" [Token.toString name]; print body)
          | StructureBody (children) => (map print children; ())
          | Signature (name, body) => (Format.printf "Parsed signature: %\n" [Token.toString name]; print body)
          | SignatureBody (children) => (map print children; ())
          | ValueDec (name, (Type ty)) => (Format.printf "Parsed value-dec: % of %\n"
                                          [Token.toString name, tyToString ty]; ())
          | Value (name, (Type ty), value) => (Format.printf "Parsed value: % of %\n"
                                               [Token.toString name, tyToString ty]; print value)
          | FunctionDec (name, (Type ty)) => (Format.printf "Parsed function-dec: % of %\n"
                                              [Token.toString name, tyToString ty]; ())
          | Function (name, (Type ty), value) => (Format.printf "Parsed function: % of %"
                                                  [Token.toString name, tyToString ty]; print value)
          | Expression (e) => (case e of
              StringExp e => Format.printf "Parsed literal: \"%\"\n" [Token.toString e]
            | NumberExp e => Format.printf "Parsed literal: %\n" [Token.toString e]
            | ValueExp e  => Format.printf "Parsed value reference: %\n" [Token.toString e])
          | _ => ()
end
