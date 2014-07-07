
structure Json =
struct
local
  structure P = PolyML
in
  structure Ty = JsonTy

  val comma = P.PrettyString ","
  val bracket_open  = P.PrettyString "["
  val bracket_close = P.PrettyString "]"
  val brace_open    = P.PrettyString "{"
  val brace_close   = P.PrettyString "}"

  fun pretty {indent,consistent} depth printArgTypes value =
  	if depth <= 0 then P.PrettyString "..."
	else
	  let val pretty' = pretty {indent=indent,consistent=consistent}
	  in
		case value
		  of Ty.JInteger  x => P.PrettyString (LargeInt.toString x)
		   | Ty.JString   s => P.PrettyString ("\"" ^ s ^ "\"")
		   | Ty.JArray   xs =>
			let
			  val pxs = map (pretty' (depth-1) printArgTypes) xs
			  fun split xs = foldr (fn (x,[])=> [x]
									 | (x,xs)=>
									[x, comma, P.PrettyBreak(1,0) ]
									@ xs
									) [] xs
			in
			  P.PrettyBlock (indent, consistent, []
								 , [bracket_open , P.PrettyBreak(1, 0)]
								 @ (split pxs)
								 @ [P.PrettyBreak(1,~indent), bracket_close])
			end
		   | Ty.JObject obj =>
			let
			  val pxs = map (fn (k,v)=>
							 P.PrettyBlock (indent, false, [],
								[ P.PrettyString ("\"" ^ k ^ "\"")
								, P.PrettyString ":"
								, pretty' (depth-1) printArgTypes v
								])) obj
			  fun split xs = foldr (fn (x,[])=> [x]
									 | (x,xs)=>
									[x, comma, P.PrettyBreak(1,0) ]
									@ xs
									) [] xs
			in
			  P.PrettyBlock (indent, consistent, []
								 , [brace_open , P.PrettyBreak(1,0)]
								 @ (split pxs)
								 @ [P.PrettyBreak(1,~indent), brace_close])
			end
		   | Ty.JNull       => P.PrettyString "null"
		   | Ty.JBool     b => P.PrettyString (Bool.toString b)
	  end

  val j = Ty.JArray [Ty.JBool true
  					, Ty.JBool false
					, Ty.JNull
					, Ty.JInteger 31412341234
					, Ty.JObject [
						("foo", Ty.JString "foos foosss..."),
						("bar", Ty.JString "bars barsssszzzz.."),
						("bazz", Ty.JArray [
									Ty.JString "bazzs"
									, Ty.JNull
									, Ty.JInteger 141421356
									])
						]
					, Ty.JString "aaaaaaabbbbbbdddeeeeeee"
					]
end (* local *)
end
