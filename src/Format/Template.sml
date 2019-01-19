local
    structure String = Ponyo_String

    datatype astElement =
         String of string
       | Variable of string
       | Loop of { body: ast, control: string }

    and ast = AST of astElement list

    structure Control =
    struct
        fun parse (c: string) = Variable c
    end

    structure Template =
    struct
        type t = ast

        type state = {
            inControl: bool,
            controlStartChar: bool,
            controlEndChar: bool,
            current: string,
            result: ast,
            error: string
        }

        fun handleControlStart ({
            result = AST result,
            current = current,
            controlStartChar = sc,
            ...
        }: state) : state = {
            controlEndChar = false,
            controlStartChar = not sc, (* case: {{{ *)
            inControl = sc,
            current = "",
            result = if current = "" then AST result else AST (String current :: result),
            error = ""
        }

        fun handleControlEnd ({
            result = AST result,
            current = current,
            controlEndChar = ec,
            ...
        }: state) : state = {
            controlEndChar = not ec, (* case: }}} *)
            controlStartChar = false,
            inControl = ec,
            result = if current = "" then AST result else AST (Control.parse current :: result),
            current = "",
            error = ""
        }

        fun handleGeneric ({
            result = result,
            current = current,
            ...
        }: state) (c: char) : state = {
            controlStartChar = false,
            controlEndChar = false,
            result = result,
            current = current ^ (String.fromChar c),
            inControl = false,
            error = ""
        }
    
        fun iterator (state: state ref) : char -> unit =
            let
                fun inner (c: char) : state =
                    case c of
                        #"{" => handleControlStart (!state)
                      | #"}" => handleControlEnd (!state)
                      | _ => handleGeneric (!state) (c)
            in
                fn (c: char) => state := inner c
            end

        fun parse (template: string) : (t * string) =
            let
                val state = ref {
                    controlStartChar = false,
                    controlEndChar = false,
                    inControl = false,
                    current = "",
                    result = AST [],
                    error = ""
                }

                val _ = String.iterate template (iterator state)

                val { error = error, result = AST result, current = current, ... } = !state
                val AST result = if current = "" then AST result else AST (String current :: result)
            in
                (AST (List.rev result), error)
            end
    end

    structure Generator =
    struct
        type t = ast
        type context = string String.Dict.t

        fun generateVariable (v: string) (c: context) : string =
            case String.Dict.get c v of
                SOME r => r
              | _ => ""

        fun generateElement (String s) (context: context) : string = s
           | generateElement (Variable v) (context: context) = generateVariable v context
           | generateElement (_ : astElement) (_: context) = ""

        fun generate (AST l: t) (context: context) : string =
            String.join (map (fn e => (generateElement e context)) l) ""
    end
in
    structure Ponyo_Format_Template : PONYO_FORMAT_TEMPLATE =
    struct
        type t = ast
    
        val generate = Generator.generate
        val parse = Template.parse
    end
end