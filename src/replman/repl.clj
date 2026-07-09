;; Based on code from Nubank Morse
;; Copyright (c) Nu North America, Inc. All rights reserved.
;; Modifications Copyright Filipe Andrade

(ns replman.repl
  (:require [clojure.core.async :as async]
            [clojure.main :as main]
            [clojure.core.server :as server]
            [dev.nu.morse :as morse]))

(def ^:private echan (var-get #'dev.nu.morse/echan))

(defn morse-ui
  []
  (morse/ui :proc server/prepl :mode :in-proc))

(defn eval-special-forms!
  [val]
  (cond
    (= val :repl/morse-ui)
    (morse-ui)))

(defn repl []
  (apply require main/repl-requires)
  (println "Clojure" (clojure-version))
  (printf "%s=> " (ns-name *ns*))
  (flush)
  (let [ch (async/chan)
        o *out*
        ex? (every-pred :cause :via)
        cb (fn [{:keys [tag val form ms ns] :as m}]
             (when-not (= tag :tap)
               (async/offer! echan (assoc m :rebl/source "REPL")))
             (eval-special-forms! val)
             (binding [*out* o]
               (case tag
                 :err (do (print val) (flush))
                 :out (do (print val) (flush))
                 :tap nil
                 :ret
                 (do
                   (if (:exception m)
                     (binding [*out* *err*]
                       (print (-> val main/ex-triage main/ex-str))
                       (flush))
                     (prn val))
                   (printf "%s=> " ns)
                   (flush)))))]
    (server/prepl *in* cb)))


(defn repl+ui
  [_]
  (morse-ui)
  (repl))

(comment
  (in-ns 'replman.repl)
  (#'dev.nu.morse/launch-in-proc)
  (#'replman.repl/repl))
