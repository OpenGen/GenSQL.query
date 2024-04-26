(ns gensql.query.permissive.parser
  "Functions for parsing GenSQL queries into a parse tree. See
  `gensql.query.parser.tree` for functions that operate on parse trees."
  #?(:clj (:require [gensql.query.io :as io])
     :cljs (:require-macros [gensql.query.io :as io]))
  (:require [instaparse.core :as insta]
            [instaparse.combinators :as combinators]))

(def bnf
  (str (io/inline-file "gensql/query/base.bnf")
       (io/inline-file "gensql/query/permissive.bnf")))

(def parse
  "An instaparse parser for GenSQL queries. The grammar is inlined at macro
  expansion time so that it can be used in the ClojureScript context where we
  don't have access to file resources."
  (insta/parser bnf))

(def non-terminals (set (keys (combinators/ebnf bnf))))

(def unparse-transformations (zipmap non-terminals (repeat str)))

(defn unparse
  "Returns a string that when parsed by `parse` will yield the provided parse tree."
  [node]
  (insta/transform unparse-transformations node))
