
structure JsonTy =
struct
  datatype t = JInteger of LargeInt.int
             | JString of String.string
             | JArray of t list
             | JObject of (String.string * t) list
             | JNull
             | JBool of Bool.bool

end

