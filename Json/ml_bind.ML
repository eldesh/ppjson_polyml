
structure Json =
struct
local
  structure P = PolyML
in
  structure Ty = JsonTy

  val ` = P.PrettyString

  fun pretty {indent,consistent} depth printArgTypes value =
    if depth <= 0 then `"..."
	else
	  let
        (* printer of nested elements *)
        val pretty' = pretty {indent=indent, consistent=consistent} (depth-1) printArgTypes
        val BR = P.PrettyBreak
	  in
		case value
		  of Ty.JInteger  x => `(LargeInt.toString x)
		   | Ty.JString   s => `("\"" ^ s ^ "\"")
		   | Ty.JNull       => `"null"
		   | Ty.JBool     b => `(Bool.toString b)
		   | Ty.JArray   [] => `"[]"
		   | Ty.JObject  [] => `"{}"
		   | Ty.JArray (x::xs) =>
			let
              (* x::xs => x_0,<space> x_1,<space> x_2,<space> ... x_(N-1),<space> x_N *)
              val comma_separated_elems =
                    rev (foldl (fn (p,ps)=> pretty' p::BR(1,0):: `","::ps) [pretty' x] xs)
			in
			  P.PrettyBlock (indent, true, []
								 , [`"[", BR(1, 0)]
								 @ comma_separated_elems 
								 @ [BR(1,~indent), `"]"])
			end
		   | Ty.JObject (x::xs) =>
			let
              fun pretty_field (k,v) =
                    P.PrettyBlock (indent, false, []
                                    , [ `("\"" ^ k ^ "\""), BR(1,0), `":", BR(1,0), pretty' v ])

              (* (f,x)::xs =>
               *     f0:x0,<space> f1:x1,<space> f2:x2,<space> ... f(N-1):x(N-1),<space> fN:xN
               *)
              val comma_separated_fields =
                    rev (foldl (fn (p,ps)=> pretty_field p::BR(1,0):: `","::ps) [pretty_field x] xs)
			in
			  P.PrettyBlock (indent, consistent, []
								 , [`"{", BR(1,0)]
								 @ comma_separated_fields
								 @ [BR(1,~indent), `"}"])
			end
	  end

  (* test object *)
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
val () = PolyML.addPrettyPrinter (Json.pretty {indent=2,consistent=true});

