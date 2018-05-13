structure Ponyo_Sml_Parser =
struct
    local
        structure Format = Ponyo_Format
        structure Ast = Ponyo_Sml_Ast
        structure Token = Ponyo_Sml_Token
        structure Symbol =
        struct
            local open Token in
            val Colon      = Symbol ":"
            val Comma      = Symbol ","
            val Asterisk   = Ident "*"
            val Arrow      = Symbol "->"
            val LeftCurly  = Symbol "{"
            val RightCurly = Symbol "}"
            val LeftParen  = Symbol "("
            val RightParen = Symbol ")"
            val End        = Symbol "end"
            val Equal      = Symbol "="
            val Sig        = Symbol "sig"
            val Signature  = Symbol "signature"
            val Struct     = Symbol "struct"
            val Structure  = Symbol "structure"
            val Fun        = Symbol "fun"
            val Val        = Symbol "val"
            val Type       = Symbol "type"
            val Datatype   = Symbol "datatype"
            val Eqtype     = Symbol "eqtype"
            val Exception  = Symbol "exception"
            val Of         = Symbol "of"
            end
        end
    in

    type tokens = Token.t list

    structure TokenList = Ponyo_Container_List(Token)

    type reader = {
        tokens : tokens,
        store  : tokens
    }

    val debug = ref false


    fun filterComments (tokens: tokens) : tokens =
        foldl (fn (t, tokens) => case t of
            Token.Comment _ => tokens
          | t => t :: tokens) [] (List.rev tokens)

    infix 6 >>=;
    fun (reader, token) >>= f =
        case token of
            SOME token => f (reader, token)
          | NONE       => (reader, NONE)

    fun readToken (reader: reader) : reader * Token.t option =
        case (#store reader @ #tokens reader) of
            hd :: tl => ({tokens=tl, store=[]}, SOME hd)
          | []       => ({tokens=[], store=[]}, NONE)

    fun readSymbol (reader: reader, tokens: Token.t list) : reader * Token.t option =
        readToken (reader) >>= (fn (reader, readToken) =>
        if TokenList.contains (tokens, readToken) then (reader, SOME readToken)
        else ({tokens=(#tokens reader), store=readToken :: (#store reader)}, NONE))

    fun readIdent (reader: reader) : reader * Token.t option =
        readToken (reader) >>= (fn (reader, token) =>
        case token of
            Token.Ident ident => (reader, SOME token)
          | t                 => ({tokens=(#tokens reader), store=t :: (#store reader)}, NONE))

    fun parseSigLit (reader: reader) : reader * Ast.t option =
        readSymbol (reader, [Symbol.Sig]) >>= (fn (reader, _) =>
        parseBody (reader, Ast.SignatureBody [], true) >>= (fn (reader, body) =>
        readSymbol (reader, [Symbol.End]) >>= (fn (reader, _) =>
        (reader, SOME body))))

    and parseSigDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, [Symbol.Signature]) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, signatureName) =>
        readSymbol (reader, [Symbol.Equal]) >>= (fn (reader, _) =>
        case parseSigLit (reader) of 
            (reader, SOME sigLit) =>
              (reader, SOME (Ast.Signature (signatureName, sigLit)))
          | (reader, NONE) =>
        readIdent (reader) >>= (fn (reader, sigIdent) =>
        (reader, SOME (Ast.Signature (signatureName, Ast.Deferred sigIdent)))))))


    and parseStrLit (reader: reader) : reader * Ast.t option =
        readSymbol (reader, [Symbol.Struct]) >>= (fn (reader, _) =>
        parseBody (reader, Ast.StructureBody [], false) >>= (fn (reader, body) =>
        readSymbol (reader, [Symbol.End]) >>= (fn (reader, _) =>
        (reader, SOME body))))

    and parseStrDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, [Symbol.Structure]) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, structureName) =>
        readSymbol (reader, [Symbol.Equal]) >>= (fn (reader, _) =>
        case parseStrLit (reader) of
            (reader, SOME strLit) =>
              (reader, SOME (Ast.Structure (structureName, strLit)))
          | (reader, NONE) =>
        readIdent (reader) >>= (fn (reader, strIdent) =>
        (reader, SOME (Ast.Structure (structureName, Ast.Deferred strIdent)))))))

    and parseExp (reader: reader) : reader * Ast.t option =
        readToken (reader) >>= (fn (reader as {tokens, store}, token) =>
        case token of
            Token.String s => (reader, SOME (Ast.Expression (Ast.StringExp token)))
          | Token.Number n => (reader, SOME (Ast.Expression (Ast.NumberExp token)))
          | Token.Ident i  => (reader, SOME (Ast.Expression (Ast.ValueExp token)))
          | _ => ({tokens=tokens, store=token :: store}, NONE))

    and parseType (reader: reader) : reader * Ast.t option =
        let
            fun parseList (reader: reader) : reader * Ast.ty list =
                case parseType (reader) of
                    (reader, SOME (Ast.Type ty)) =>
                    (case readSymbol (reader, [Symbol.Comma]) of
                        (reader, NONE) => (reader, [ty])
                      | (reader, _) =>
                    case parseList (reader) of
                        (reader, []) => (reader, [ty])
                      | (reader, tys) => (reader, ty :: tys))
                  | _ => (reader, [])

            fun parseBasic (reader: reader) : reader * Ast.ty =
                case readToken (reader) of
                    (reader, SOME (Token.Symbol "(")) =>
                        let
                            val (reader, tys) = parseList (reader)
                            val ty = case tys of
                                []   => Ast.NoType
                              | [ty] => ty
                              | tys  => Ast.ConstructedType (tys, Ast.NoType)
                        in
                            case readSymbol (reader, [Symbol.RightParen]) of
                                (reader, SOME _) => (reader, ty)
                              | _ => raise Fail "Expected closing paren."
                        end
                    | (reader, SOME (Token.Symbol "{")) =>
                        let
                        in
                            (reader, Ast.NoType)
                        end
                    | (reader, SOME (token as (Token.Ident ident))) =>
                        (reader, Ast.IdentType token)
                    | (reader as {tokens, store}, SOME token) =>
                        ({tokens=tokens, store=token :: store}, Ast.NoType)
                    | (reader, NONE) => raise Fail "Invalid eof during type declaration."

            and parseProduct (reader: reader): reader * Ast.ty list =
                let val (reader, basicType) = parseBasic (reader) in
                    case readSymbol (reader, [Symbol.Asterisk]) of
                        (reader, NONE) => (reader, [basicType])
                      | (reader, _) =>
                            let val (reader, types) = parseProduct (reader) in
                                (reader, basicType :: types)
                            end
                end

            and parseTuple (reader: reader) : reader * Ast.t option =
                case parseProduct (reader) of
                    (reader, [ast]) => (reader, SOME (Ast.Type ast))
                  | (reader, asts)  => (reader, SOME (Ast.Type (Ast.ProductType asts)))

            fun parseNextType (reader: reader, lastType: Ast.t) : reader * Ast.t option =
                let
                    val lastType : Ast.ty = case lastType of
                        Ast.Type lt => lt
                      | _ => raise Fail "Expected type ast."

                    fun wrapConstructed (tys) =
                        case parseType (reader) of
                            (reader, NONE) => (reader, SOME (Ast.Type lastType))
                          | (reader, SOME (Ast.Type Ast.NoType)) =>
                              (reader, SOME (Ast.Type lastType))
                          | (reader, SOME (Ast.Type nextType)) =>
                              (reader, SOME (Ast.Type (Ast.ConstructedType (tys, nextType))))
                          | (reader, _) =>
                              raise Fail "Expected next type."
                in
                    case lastType of
                        Ast.ConstructedType (tys, _) => wrapConstructed (tys)
                      | Ast.IdentType (_) => wrapConstructed ([lastType])
                      | Ast.NoType => (reader, NONE)
                      | _ => (reader, (SOME (Ast.Type lastType)))
                end
        in
            parseTuple (reader) >>= (fn (reader, inputAst) =>
            parseNextType (reader, inputAst) >>= (fn (reader, inputAst) =>
            let val inputAst : Ast.ty = case inputAst of
                Ast.Type ty => ty
              | _ => raise Fail "Expected type ast."
            in
                case readSymbol (reader, [Symbol.Arrow]) of
                    (reader, NONE) => (reader, SOME (Ast.Type inputAst))
                  | (reader, SOME _) =>
                    let
                        val (reader, outputAst) = parseType (reader)
                    in
                        case outputAst of
                            SOME (Ast.Type outputAst) =>
                                (reader, SOME (Ast.Type
                                               (Ast.FunctionType (inputAst, outputAst))))
                          | _ => (raise Fail "Expected type after ->."; (reader, NONE))
                    end
            end))
        end

    and parseOptType (reader: reader) : reader * Ast.t option =
        case readSymbol (reader, [Symbol.Colon]) of
            (reader, NONE) => (reader, SOME (Ast.Type (Ast.NoType)))
          | (reader, _)    => parseType (reader)

    and parseOptExp (reader: reader) : reader * Ast.t option =
        readSymbol (reader, [Symbol.Equal]) >>= (fn (reader, _) =>
        parseExp (reader))

    and parseFunDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, [Symbol.Fun]) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, functionName) =>
        parseOptType (reader) >>= (fn (reader, typeAst) =>
        case parseOptExp (reader) of
            (reader, NONE) =>
              (reader, SOME (Ast.FunctionDec (functionName, typeAst)))
          | (reader, SOME expAst) =>
              (reader, SOME (Ast.Function (functionName, typeAst, expAst))))))

    and parseValDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, [Symbol.Val]) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, valueName) =>
        parseOptType (reader) >>= (fn (reader, typeAst) =>
        case parseOptExp (reader) of
            (reader, NONE) =>
              (reader, SOME (Ast.ValueDec (valueName, typeAst)))
          | (reader, SOME expAst) =>
              (reader, SOME (Ast.Value (valueName, typeAst, expAst))))))

    and parseTypeDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, [Symbol.Type]) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, typeName) =>
        case readSymbol (reader, [Symbol.Equal]) of
            (reader, NONE) =>
              (reader, SOME (Ast.TypeDec (typeName, Ast.Type Ast.NoType)))
          | (reader, SOME _) =>
        parseType (reader) >>= (fn (reader, ast) =>
        (reader, SOME (Ast.TypeDec (typeName, ast))))))

    and parseEqtypeDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, [Symbol.Eqtype]) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, typeName) =>
        (reader, SOME (Ast.EqtypeDec typeName))))

    and parseExcDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, [Symbol.Exception]) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, excName) =>
        case readSymbol (reader, [Symbol.Of]) of
            (reader, NONE) =>
              (reader, SOME (Ast.ExcDec (excName, Ast.Type Ast.NoType)))
          | (reader, SOME _) =>
        parseType (reader) >>= (fn (reader, typeAst) =>
        (reader, SOME (Ast.ExcDec (excName, typeAst))))))

    and parseBody (reader: reader, ast: Ast.t, isSig: bool) : reader * Ast.t option =
        let
            val parsers = [
                ("signature", parseSigDec),
                ("structure", parseStrDec),
                ("function", parseFunDec),
                ("value", parseValDec),
                ("type", parseTypeDec),
                ("eqtype", parseEqtypeDec),
                ("exception", parseExcDec)
            ]

            fun doParse ((parseType, parse), (reader, ast)) : reader * Ast.t option =
                case parse (reader) of
                    (reader, SOME child) => (
                        if !debug then Format.printf "Parsed %\n" [parseType] else ();
                        parseBody (reader, Ast.insert (valOf ast, child), isSig)
                    )
                  | (reader, NONE) => (reader, ast)

        in
            foldl doParse (reader, SOME ast) parsers
        end

    fun parse (tokens: tokens) : Ast.t =
        let
            val (reader, ast) =
                parseBody ({tokens=(filterComments tokens), store=[]}, Ast.Root [], false)

            fun illegalMessage (token) : string =
                Format.sprintf "Invalid parser construct: %" [Token.toString token]
        in
            case readToken (reader) of
                (_, SOME token) => raise Fail (illegalMessage token)
              | (_, NONE)        =>
            case (#store reader) of
                []     => valOf (ast)
              | tokens => raise Fail "Invalid parser construct 2"
        end

    end
end
