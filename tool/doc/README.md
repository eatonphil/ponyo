# mldoc

This is a very simple parser that pulls commented functions
out of an SML source and converts it into HTML. Only functions
that are preceded by a comment indicating a relationship to the
function are captured.

Comments must be of the following format:
    OPEN_COMMENT WS DASH IDENT COLON WS DESC WS OPT_EX END_COMMENT

OPT_EX is an optional area for examples that will get quoted and
displayed as source code in the HTML. OPT_EX is defined:
    "Ex" COLON LF EXAMPLE (LF EXAMPLE)*

Multiple lines are concatenated together with one space. WS is defined
in ponyo/String/StringExport.sml. WS preceding a line in DESC is
removed. and the IDENT is the identifier of the function following.

## Example

Here is an example file, MyStructure.sml:

```sml
structure MyStructure =
struct
    ...

    (* -readAll: Reads all data and splits on newlines.
	*  Any consecutive newlines are ignored.
	*
	*  Ex:
	*      readAll ("foo\n\nbar") = ["foo", "bar"]
	*)
    fun readAll (source: string) : string list =
	...

    ...
end
```

This script returns the following basic HTML for this file:

```html
<h1>MyStructure</h1>
<div class="values">
    <div class="fun">
	<p class="fun-sig">val <strong>readAll</strong> : (source: string) :
	string list</p>
	<p class="fun-desc">Reads all data and splits on newlines. Any
	consecutive newlines are ignored.</p>
	<pre class="fun-ex"><code>readAll ("foo\n\nbar") = ["foo",
	"bar"]</code></pre>
    </div>
<div>
```