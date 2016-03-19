structure Parser =
struct
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
        end
    end

    type tokens = Token.t list

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

    fun readSymbol (reader: reader, token: Token.t) : reader * Token.t option =
        readToken (reader) >>= (fn (reader, readToken) =>
        if token = readToken then (reader, SOME token)
        else ({tokens=(#tokens reader), store=readToken :: (#store reader)}, NONE))

    fun readIdent (reader: reader) : reader * Token.t option =
        readToken (reader) >>= (fn (reader, token) =>
        case token of
            Token.Ident ident => (reader, SOME token)
          | t                 => ({tokens=(#tokens reader), store=t :: (#store reader)}, NONE))

    fun parseSigLit (reader: reader) : reader * Ast.t option =
        readSymbol (reader, Symbol.Sig) >>= (fn (reader, _) =>
        parseBody (reader, Ast.SignatureBody [], true) >>= (fn (reader, body) =>
        readSymbol (reader, Symbol.End) >>= (fn (reader, _) =>
        (reader, SOME body))))

    and parseSigDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, Symbol.Signature) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, signatureName) =>
        readSymbol (reader, Symbol.Equal) >>= (fn (reader, _) =>
        parseSigLit (reader) >>= (fn (reader, sigAst) =>
        (reader, SOME (Ast.Signature (signatureName, sigAst)))))))

    and parseStrLit (reader: reader) : reader * Ast.t option =
        readSymbol (reader, Symbol.Struct) >>= (fn (reader, _) =>
        let
            val (reader, body) =
                case parseBody (reader, Ast.StructureBody [], false) of
                    (reader, NONE)     => (reader, SOME (Ast.StructureBody []))
                  | (reader, SOME ast) => (reader, SOME ast)
        in
            readSymbol (reader, Symbol.End) >>= (fn (reader, _) =>
            (reader, body))
        end)

    and parseStrDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, Symbol.Structure) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, structureName) =>
        readSymbol (reader, Symbol.Equal) >>= (fn (reader, _) =>
        parseStrLit (reader) >>= (fn (reader, ast) =>
        (reader, SOME (Ast.Structure (structureName, ast)))))))

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
                    (case readSymbol (reader, Symbol.Comma) of
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
                            case readSymbol (reader, Symbol.RightParen) of
                                (reader, NONE) => raise Fail "Expected closing paren."
                              | (reader, _)    => (reader, ty)
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
                    case readSymbol (reader, Symbol.Asterisk) of
                        (reader, NONE) => (reader, [basicType])
                      | (reader, _)   =>
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
                case readSymbol (reader, Symbol.Arrow) of
                    (reader, NONE) => (reader, SOME (Ast.Type inputAst))
                  | (reader, SOME _) =>
                    let
                        val (reader, outputAst) = parseType (reader)
                    in
                        case outputAst of
                            SOME (Ast.Type outputAst) =>
                            (reader, SOME (Ast.Type (Ast.FunctionType (inputAst, outputAst))))
                            | _ => (raise Fail "Expected type after ->."; (reader, NONE))
                    end
            end))
        end

    and parseOptType (reader: reader) : reader * Ast.t option =
        case readSymbol (reader, Symbol.Colon) of
            (reader, NONE) => (reader, SOME (Ast.Type (Ast.NoType)))
          | (reader, _)    => parseType (reader)

    and parseOptExp (reader: reader) : reader * Ast.t option =
        readSymbol (reader, Symbol.Equal) >>= (fn (reader, _) =>
        parseExp (reader))

    and parseFunDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, Symbol.Fun) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, functionName) =>
        parseOptType (reader) >>= (fn (reader, typeAst) =>
        case parseOptExp (reader) of
            (reader, NONE) =>
              (reader, SOME (Ast.FunctionDec (functionName, typeAst)))
          | (reader, SOME expAst) =>
              (reader, SOME (Ast.Function (functionName, typeAst, expAst))))))

    and parseValDec (reader: reader) : reader * Ast.t option =
        readSymbol (reader, Symbol.Val) >>= (fn (reader, _) =>
        readIdent (reader) >>= (fn (reader, valueName) =>
        parseOptType (reader) >>= (fn (reader, typeAst) =>
        case parseOptExp (reader) of
            (reader, NONE) =>
              (reader, SOME (Ast.ValueDec (valueName, typeAst)))
          | (reader, SOME expAst) =>
              (reader, SOME (Ast.Value (valueName, typeAst, expAst))))))

    and parseBody (reader: reader, ast: Ast.t, isSig: bool) : reader * Ast.t option =
        let
            val parsers = [
                ("signature", parseSigDec),
                ("structure", parseStrDec),
                ("function", parseFunDec),
                ("value", parseValDec)(*,
                ("type", parseTypeDec),
                ("exception", parseExcDec)*)
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
