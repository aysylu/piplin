(ns piplin.modules
  (:use [piplin sim types protocols])
  (:use [clojure.string :only [join]])
  (:use [slingshot.slingshot :only [throw+]])
  (:refer-clojure :exclude [replace cast])
  (:use [clojure.string :only [join replace split]])
  (:require [piplin.connect :as conn]))

(defn connect-impl
  "This connects a register to an expr"
  [reg expr]
  (let [port-type (:port-type (value reg))]
    (when-not (or (#{:register :subport} port-type)
                  (= :array-get (-> reg value :op)))
      (throw+ (error "Must be :register or :subport or the whole thing must be an array location (not displayed yet TODO), was"
                     port-type)))
    {:type (or port-type :register)
     :args {:reg reg 
            :expr (cast (typeof reg) expr)}}))

(comment
  All exprs must list their subexprs that should be checked
  as an :args map. This map's keys are used by the synth/sim
  phases to do something specific, but the values are implied
  to be exprs that should be walked and checked.
  )

(let [module-name-cache (atom {})]
 (defn gen-module-name
  "Generates a unique module name out of the given basename
  and arguments. Tries to make the name human-readable."
   [name args]
   (let [x (let [k (apply vector name args)]
             (if-let [id (get @module-name-cache k)]
               id 
               (let [id (str name "__" (join "_" args))]
                 (swap! module-name-cache assoc k id)
                 id)))]
     x)))

(defn make-port*
  "Takes a keyword name, an owning module token, and
  the port's type and returns the port."
  [name piplin-type port-type]
  (alter-value (mkast piplin-type :port []
                      #(throw+
                         (error "no longer using sim-fn")))
               merge
               {:port name
                :port-type port-type}))

(defn module*
  "Takes the module's name, input, output, feedback,
  submodules, and connections. The name is optional, and the
  other 5 arguments are keyword args: :inputs :outputs
  :feedback :modules :connections. The inputs are a map from keywords,
  which are the inputs' names, to their types The outputs and
  feedbacks are the same, but the values of the map are the
  initial values of those state elements. The connections
  are a list of functions which, when evaluated,
  will call connect to populate all the necessary connections."
  [& args]
  (let [mod-name (first args)
        ;`str?` holds whether the module has a given name
        sym? (symbol? mod-name)
        args (if sym? (rest args) args)
        mod-name (symbol (str *ns*)
                         (if sym?
                           (name mod-name)
                           ;The module is called moduleNN, where
                           ;NN is hopefully the line that it was declared on.
                           (str "module"
                                (-> (RuntimeException.)
                                  .getStackTrace
                                  ^java.lang.StackTraceElement (aget 2)
                                  .getLineNumber
                                  ))))
        
        ;parses keyword arguments, defaulting them to {}
        {:keys [inputs outputs feedback modules connections]}
        (->> (apply hash-map args)
          (merge-with #(or %2 %1)
                      {:inputs {} :outputs {} :feedbock {}
                       :modules {} :connections []})) 
        
        ;We define port objects for each input and state
        ;element. These are given to the functions to make
        ;the deferred ASTs that will be used in
        ;simulation/synthesis.
       
        ;Start by merging outputs and feedback and making
        ;their ports
        register-ports (->> (concat outputs feedback)
                ;Next, we convert all the outputs and feedback to
                ;their types.
                (map (fn [[k v]] [k (typeof v) :register]))
                ;We finally make each one a port
                (map #(apply make-port* %)))

        ;Then, we make the input ports
        input-ports 
                (map #(make-port* (key %) (val %) :input) inputs)

        ;Next we'll make the final list of ports.
        ports (concat input-ports register-ports) 

        ;TODO: reject duplicated keywords between inputs and
        ;outputs/feedback

        ;To make the connections, we must define a root connect
        ;function that stores to an atom. Then, we can invoke
        ;all of the thunks to populate the connection. Afterwards,
        ;we can do error checking
        body (atom [])
        _ (binding [conn/connect (fn [reg expr]
                              ;TODO: could pass up to parent
                              ;on unmatched tokens (but I think
                              ;I'm removing tokens from ports)
                              (swap! body conj
                                     (connect-impl reg expr)))]
            (doseq [f connections] (f)))]
    {:type :module
     :token mod-name
     :inputs inputs
     :outputs outputs
     :feedback feedback
     :modules modules
     :ports ports
     :body @body}
    ))

(defmacro module
  "TODO: must check for repeated declarations"
  [module-name config & body]
  (let [has-name? (not (vector? module-name))
        body (if has-name? body (cons config body))
        config (if has-name? config module-name)
        module-name (if has-name? module-name nil)
        ;First, we extract the 4 sections
        {:keys [inputs outputs feedback modules]}
        (into {:inputs [] :outputs []
               :feedback [] :modules []}
              (apply hash-map config))

        ;Next, we construct a map from symbols to
        ;their types
        reg-types (map (fn [[k v]]
                         [k `(typeof ~v) :register])
                       (partition 2
                         (concat outputs feedback)))
        sym->type (concat reg-types
                          (map (fn [[k v]] [k v :input])
                               (partition 2 inputs)))
        
        ;Now we can create the port declarations
        port-decls (mapcat (fn [[k v t]]
                          `(~k (make-port* ~(keyword k) ~v ~t)))
                        sym->type)
        
        ;Finally, we need to make the arguments compatible with
        ;module*. To do this, we must keywordize all the symbol
        ;names, and put them into maps
        keywordize (fn [macro-format]
                     (into {} (map (fn [[k v]]
                                     [(keyword k) v])
                                   (partition 2 macro-format))))
        inputs (keywordize inputs)  
        outputs (keywordize outputs)  
        feedback (keywordize feedback)  
        modules (keywordize modules)
        module-decls (mapcat (fn [[k v]]
                            [(symbol (name k)) v]) modules)

        ;first, we flatten the body and filter for symbols that start
        ;with a module's name
        module-names (map (comp #(str % \$) name) (keys modules))
        symbols (->> (flatten body)
                  (filter (fn [node]
                            (symbol? node))) 
                  (mapcat (fn [node]
                            (let [name (name node)]
                              (when (some #(.startsWith name %1) module-names)
                                (let [[x y] (split name #"\$")]
                                  [node `(->>
                                           ~(symbol x)
                                           :ports
                                           (some #(and (= ~(keyword y)
                                                          (:port (value %)))
                                                       (subport ~(symbol x)
                                                                ~(keyword x)
                                                                ~(keyword y)))))])))))
                  distinct)]
    `(let [~@port-decls
           ~@module-decls
           ~@symbols]
       (module* ~@(cond
                    (symbol? module-name) `('~module-name)
                    (not (nil? module-name)) `(~module-name)  
                    :else nil) 
                :inputs ~inputs
                :outputs ~outputs
                :feedback ~feedback
                :modules ~modules
                :connections [(fn [] ~@body)]))))

(defmacro defmodule
  "Same as module, but conveniently defs it at the same time"
  [name params & args]
  `(defn ~name ~params (module (symbol (gen-module-name ~(str name) ~params)) ~@args)))

(declare make-input-map)

(def ^{:dynamic true
       :doc "A map from inputs to functions
            which, when evaluated, return their
            value given the state of the sim
            (currently passed as a dynamic var)."}
  *input-fns*)

(def ^{:dynamic true
       :doc "A vector of the state elements needed
            by the input fns."} *input-fn-ports* [])

(def ^:dynamic *module-path* [])
(defn walk-modules
  "Takes a map-zipper of the ast and applies
  the function combine as a reduce across the values
  given by (visit module) for every module."
  [ast visit combine]
  (let [x (visit ast)
        input-map (make-input-map ast)]
    (if (seq (:modules ast))
      (reduce combine x
              (map (fn [[name mod]]
                     (let [[input-fns input-ports] 
                            (input-map name)]
                       (binding [*module-path* (conj *module-path* name)
                                 *input-fns* input-fns
                                 *input-fn-ports* input-ports]
                         (walk-modules mod visit combine))))
                   (:modules ast)))
      x)))

(defn walk-connects
  "Takes a map-zipper of the ast and applies
  the function combine as a reduce operation
  across the values given by (visit connect)
  for every connection in every module"
  [ast visit combine]
  (walk-modules
   ast 
    (fn [ast]
      (doall ;note: force eagerness here for *module-path* correctness
        (map visit
             (:body ast))))
    combine))

;TODO: refactor walk-registers to use walk-connects
(defn walk-registers
  "Takes a map-zipper of the ast and applies
  the function combine as a reduce operation
  across the values given by (visit connect)
  for every connection in every module"
  [ast visit combine]
  (walk-modules
   ast 
    (fn [ast]
      (doall ;note: force eagerness here for *module-path* correctness
        (map visit
             (filter #(= (:type %)
                         :register)
                     (:body ast)))))
    combine))

(defn walk-expr
  [expr visit combine]
  (let [x (visit expr)]
    (if-let [args (:args (value expr))]
      (let [subexprs (vals args)]
        (reduce combine x
                (map #(walk-expr % visit combine)
                     subexprs)))
      x)))

(defn- map-keys
  "Takes a map and a function, and
  returns a new map with that function
  applied to all of the keys."
  [m f]
  (->> (mapcat (fn map-keys-helper [[k v]]
                 [k (f v)]) m)
    (apply hash-map)))

(defn subport
  {:post [#(every? (comp not nil?) (vals %))]}
  [submodule submodule-name submodule-port]
  (let [port-type (->> submodule
                    ((juxt (comp #(map-keys % typeof)
                                 :outputs)
                           :inputs))
                    (apply merge)
                    submodule-port)]
    {:type port-type
     :port-type :subport
     :module submodule-name
     :op :port
     :port submodule-port}))

(comment
  How to get the combinational function for ports.

  We find all ports and build the arglist.
  The arglist is going to get used to fill in a map that'll
  be set into a binding when the function is executed.
  As the function is built, all termini will be const or
  ports. The ports' access fn will read the value from
  the binding.
  
  This is done by make-connection and make-sim-fn.)

(def ^:dynamic *sim-state*)

(defn make-sim-fn
  "Takes a map-zipper of the ast of an expr
  and walks along the expr. It returns a function
  that computes the expr
  and takes no args (needed ports come via binding).
  The function collects its args into a map which it
  binds before invoking the function so that the
  ports can get their values at the bottom."
  [expr]
  (let [[my-sim-fn my-args]
        (if (pipinst? expr)
          [#(identity expr) []] 
          (-> expr 
            meta
            :sim-factory))]
    (let [args (:args (value expr))
          arg-fns (map #(make-sim-fn (val %)) args)
          arg-map (zipmap (keys args)
                          arg-fns)
          fn-vec (map #(get arg-map %) my-args)]
      (if (= (:op (value expr)) 
             :port) 
        (condp = (:port-type (value expr))
          :register
          (let [path (conj *module-path* (:port (value expr)))]
            (fn []
              (get *sim-state* path)))
          :input 
          (let [f (get *input-fns* (:port (value expr)))]
            f)
          :subport
          (let [path (conj *module-path* (:module expr) (:port expr))]
            (fn []
              (get *sim-state* path))))
        (fn []
          (apply my-sim-fn (map #(%) fn-vec)))))))

(defn get-required-state
  "Takes an expression and returns a list
  of the needed state elements to compute the expression."
  [expr]
  (walk-expr expr 
             #(when-let [port (:port (value %))]
                (condp = (:port-type (value %))
                  :register 
                  [(conj *module-path* port)]
                  :subport
                  [(conj *module-path* (:module %) port)]
                  nil))
             concat))

(defn make-input-map
  "Takes a module and returns a map from names of
  its submodules to maps from names of the submodules'
  inputs to their simfns."
  [module]
  (merge
    (apply hash-map
           (interleave
             (keys (:modules module))
             (repeat [{} []])))
    (->> (:body module)
      (filter #(= (:type %)
                  :subport))
      (map (fn make-input-pair
             [ast]
             (let [{{:keys [reg expr]} :args} ast
                   {:keys [port module]} reg]
               {module [{port (make-sim-fn expr)} (get-required-state expr)]})))
      (apply merge-with
             (fn [[x deps1] [y deps2]]
               [(merge-with
                  #(throw+ (error "duplicate connection" %1 %2)) 
                  x y) (concat deps1 deps2)])))))

(defn make-connection
  "Takes a connection to a feedback or output, compiles
  the expr and returns a simulator function via every-cycle."
  [connection]
  (let [reg (get-in (value connection) [:args :reg])
        expr (get-in (value connection) [:args :expr])
        index-expr (let [reg (value reg)]
                     (when (= (:op reg) :array-get)
                       (get-in reg [:args :i])))
        sim-fn (make-sim-fn expr)
        index-fn (when index-expr (make-sim-fn index-expr))
        reg (if-not index-expr reg
              (-> reg value :args :array))
        reg-state (conj *module-path* (:port (value reg))) 
        ports (get-required-state expr)
        ports (concat ports *input-fn-ports*)]
    (every-cycle
      (fn [& vals]
        (binding [*sim-state* (zipmap ports vals)]
          (let [ans (sim-fn)]
            (if-not index-expr
              ans
              (assoc (get *sim-state* reg-state)
                     (index-fn)
                     ans)))))
      ports
      reg-state)))

(defn- get-qual-state
  "This function takes a module and returns a
  map whose keys are [token port] pairs (token
  is a gensym unique to the module instance and
  port is a keyword) and whose values are the
  initial values of the registers of the module
  (from the :feedback and :outputs sections).
  This state can be used by the simulation
  engine."
  [module]
  (let [token (:token module)
        regs (merge (:outputs module)
                    (:feedback module))]
    (apply hash-map (mapcat (fn [[k v]]
                              [(conj *module-path* k) v])
                            regs))))

(defn get-all-registers
  "Takes a module and returns a seq of all
  registers in the hierarchy. The registers
  are described as vectors of the form
  `(conj *module-path* reg)`."
  [root-module]
  (walk-registers root-module
                 #(identity
                    (conj *module-path* 
                          (let [reg (value (get-in % [:args :reg]))]
                            (-> (if (= (:op reg) :array-get)
                                  (get-in reg [:args :array])
                                  reg)
                              value 
                              :port))))
                 concat))

(defn make-sim
  "Takes an elaborated hierarchy of modules and returns a
  pair of [state fns] that can be simulated with
  exec-sim. See exec-sim for details."
  [root]
  (let [initial-state (walk-modules root get-qual-state
                                    merge)
        connections (walk-registers root make-connection
                                   concat)
        connections (->> connections
                      (apply concat)
                      (apply hash-map))]
    [initial-state connections]))

(defn trace-module
  "Compiles and sims a module and returns the trace."
  [mod cycles]
  (let [[state fns] (make-sim mod)
        [fns trace] (apply trace-keys fns
                           (get-all-registers mod))
        _ (exec-sim state fns cycles)]
    @trace))

(comment
  First make nested modules connect and information hiding
  work properly. Next, add a semantic check to verify
  that everything was connected. This should involve
  several semantic checks and useful errors.

  Next, I must write more functions, like inc, dec, slice,
  bits, concat, and the bits type. I'll also need to write
; if/mux and case.

  At this point we can either try for toVerilog or
  implement structs or vectors, including pattern
  matching aka destructuring. )
