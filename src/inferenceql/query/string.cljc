(ns inferenceql.query.string
  (:refer-clojure :exclude [munge])
  (:require [clojure.set :as set]
            [clojure.string :as string]))

(defn upper-case?
  "Returns true if s is all upper-case. Otherwise returns false."
  [s]
  (= s (string/upper-case s)))

(defn lower-case?
  "Returns true if s is all lower-case. Otherwise returns false."
  [s]
  (= s (string/lower-case s)))

(defn match-case
  "If s2 is all upper-case or lower-case changes s1 to match."
  [s1 s2]
  (cond (upper-case? s2)
        (string/upper-case s1)

        (lower-case? s2)
        (string/lower-case s1)

        :else
        s1))

(def ^:private munge-map
  "Chars to munge.
   Based off clj compiler, but that's concerned with Java compatibility,
   whereas we're concerned with clj/sci compatibility."
  {" " "_SPACE_"
   "$" "_DOLLAR_"
   ;; "-" "_"
   ;;"." "_DOT_"
   ":" "_COLON_"
   ;;"+" "_PLUS_"
   ;;">" "_GT_"
   ;;"<" "_LT_"
   "=" "_EQ_"
   "~" "_TILDE_"
   "!" "_BANG_"
   "@" "_CIRCA_"
   "#" "_SHARP_"
   "'" "_SINGLEQUOTE_"
   "\"" "_DOUBLEQUOTE_"
   "%" "_PERCENT_"
   "^" "_CARET_"
   "&" "_AMPERSAND_"
   "*" "_STAR_"
   "|" "_BAR_"
   "(" "_LPAREN_"
   ")" "_RPAREN_"
   "{" "_LBRACE_"
   "}" "_RBRACE_"
   "[" "_LBRACK_"
   "]" "_RBRACK_"
   "/" "_SLASH_"
   "\\" "_BSLASH_"
   ;;"?" "_QMARK_"
   })

(def ^:private demunge-map
  "Extra chars to demunge"
  (set/map-invert munge-map))

#?(:cljs
   (defn js-regex-escape
     "Escapes special chars in a string for literal matching in a regex.

     Based off TC39 proposal for RegExp.escape."
     [s]
     (string/replace s #"[\\^$*+?.()|\[\]{}]" "\\$&")))

(defn ^:private key-matching-regex
  "Generates a regex that matches every key in a map.

   \\Q and \\E are used to quote special chars for literal matches in Java
   regexes."
  [m]
  (re-pattern
    #?(:clj (str
              "\\Q"
              (string/join "\\E|\\Q" (keys m))
              "\\E")
       :cljs (string/join "" (mapv js-regex-escape (keys m))))))

(def ^:private munge-regex (key-matching-regex munge-map))
(def ^:private demunge-regex (key-matching-regex demunge-map))

(defn ^:private munge*
  "Munging/demunging helper fn"
  [x re m]
  (cond
    (string? x)
    (string/replace x re m)

    (or (symbol? x)
        (keyword? x))
    ((if (symbol? x) symbol keyword)
     (munge* (namespace x) re m)
     (munge* (name x) re m))

    (nil? x)
    nil

    :else (throw (ex-info "Unsupported type" {:type (type x)}))))

(defn munge
  "Munges input, replacing invalid clj chars with tokens. Works on strings,
   keywords, and symbols. Preserves namespaces. Returns the same class
   as the input.

   For supporting delimited identifiers like `foo bar`."
  [x]
  (munge* x munge-regex munge-map))

(defn demunge
  "Inverts munging. Returns the same class as the input."
  [x]
  (munge* x demunge-regex demunge-map))

(defn safe-symbol
  "Returns a munged symbol version of the input."
  ([x]
   (-> x munge symbol))
  ([ns x]
   (symbol (munge ns) (munge x))))

(defn safe-keyword
  "Returns a munged keyword version of the input."
  ([x]
   (-> x munge keyword))
  ([ns x]
   (keyword (munge ns) (munge x))))
