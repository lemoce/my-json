module Data.Json ( JSON (..)
                 , jnull
                 , jbool
                 , jnumber
                 , jstring
                 , jobject
                 , jarray
                 , decode )
where

import           Data.List (intercalate)
import           Prelude   hiding (null)

data JValue = JString String
            | JBool Bool
            | JNumber Double
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

type JSON = JValue

jnull = JNull

jbool = JBool

jnumber = JNumber

jstring = JString

jobject = JObject

jarray = JArray

decode :: JSON -> String
decode (JNull) = "null"
decode (JBool True) = "true"
decode (JBool False) = "false"
decode (JNumber n) = show n
decode (JString s) = show s

decode (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)

        renderPair (k, v) = show k ++ ": " ++ decode v

decode (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map decode vs)
