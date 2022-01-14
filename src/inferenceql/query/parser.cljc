(ns inferenceql.query.parser
  "Functions for parsing IQL SQL queries into a parse tree. See
  `inferenceql.query.parser.tree` for functions that operate on parse trees."
  #?(:clj (:require [inferenceql.query.io :as io])
     :cljs (:require-macros [inferenceql.query.io :as io]))
  (:require [instaparse.core :as insta]
            [instaparse.combinators :as combinators]))

(def bnf (io/inline-file "inferenceql/query/grammar.bnf"))

(def parse
  "An instaparse parser for IQL SQL queries. The grammar is inlined at macro
  expansion time so that it can be used in the ClojureScript context where we
  don't have access to file resources."
  (insta/parser bnf))

(def non-terminals (set (keys (combinators/ebnf bnf))))

(def unparse-transformations (zipmap non-terminals (repeat str)))

(defn unparse
  "Returns a string that when parsed by `parse` will yield the provided parse tree."
  [node]
  (insta/transform unparse-transformations node))

(comment

  (require '[inferenceql.query.parser] :reload)

  (parse "mutual information of var x with var y under model" :start :mutual-information-expr)
;; => [:mutual-information-expr
;;     "mutual"
;;     [:ws " "]
;;     "information"
;;     [:ws " "]
;;     "of"
;;     [:ws " "]
;;     [:variable-list [:variable "var" [:ws " "] [:simple-symbol "x"]]]
;;     [:ws " "]
;;     "with"
;;     [:ws " "]
;;     [:variable-list [:variable "var" [:ws " "] [:simple-symbol "y"]]]
;;     [:ws " "]
;;     "under"
;;     [:ws " "]
;;     [:model-expr [:simple-symbol "model"]]]

  ,)
