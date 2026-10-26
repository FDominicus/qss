;;;; cases.sexp -- what the words of vocabulary.sexp do, with data
;;;;
;;;; Every case runs in Common Lisp and Emacs Lisp, each in memory and in
;;;; SQLite (the tables written to a fresh database), and in Nushell (0.116.1)
;;;; when nu is installed. All runs must give the expected result. The same file lives in
;;;; qss-query/spec/ and qss/spec/; change both.
;;;;
;;;; (:case NAME :from TABLE :stages (STAGE ...) EXPECTED [:nushell NOTE])
;;;;   TABLE     a key of :tables
;;;;   STAGE     a word with arguments, as in a pipe; (table :cities) is the
;;;;             table :cities, in memory or in the same database
;;;;   EXPECTED  :rows (plist ...)   rows, in this order, columns in this order
;;;;             :values (v ...)     after get
;;;;             :value v            after count, columns, or first/last without n
;;;;             :error t            the pipe signals an error
;;;;   NOTE      (:differs "why"): Nushell gives another result (as noted);
;;;;             (:skip "why"): not run in Nushell (a word Nushell does not have)
;;;; Values compare by value for numbers (10 = 10.0), else with EQUAL.

(:cases 1

 :tables
 ((:people
   ((:nr 1 :name "Anna"  :city "Bruchsal"    :age 34  :score 2.5)
    (:nr 2 :name "bert"  :city nil           :age 41  :score nil)
    (:nr 3 :name "Carla" :city "Heidelsheim" :age 29  :score 7)
    (:nr 4 :name nil     :city "Bruchsal"    :age 41  :score 10.0)
    (:nr 5 :name "Ärger" :city "Karlsruhe"   :age nil :score 3)))
  (:cities
   ((:city "Bruchsal"    :zip "76646" :name "Stadt Bruchsal")
    (:city "Heidelsheim" :zip "76646" :name "Heidelsheim (Bruchsal)")
    (:city "Karlsruhe"   :zip "76131" :name "Stadt Karlsruhe")
    (:city "Mannheim"    :zip "68159" :name "Stadt Mannheim")))
  (:tags
   ((:place nil        :tag "none")
    (:place "Bruchsal" :tag "home"))))

 ;; ------------------------------------------------------------------
 ;; where, filter: conditions and NULL

 (:case "all rows" :from :people :stages ()
  :rows ((:nr 1 :name "Anna"  :city "Bruchsal"    :age 34  :score 2.5)
         (:nr 2 :name "bert"  :city nil           :age 41  :score nil)
         (:nr 3 :name "Carla" :city "Heidelsheim" :age 29  :score 7)
         (:nr 4 :name nil     :city "Bruchsal"    :age 41  :score 10.0)
         (:nr 5 :name "Ärger" :city "Karlsruhe"   :age nil :score 3)))
 (:case "where =" :from :people
  :stages ((where (= :city "Bruchsal")) (sort-by :nr) (select :nr))
  :rows ((:nr 1) (:nr 4)))
 (:case "filter is where" :from :people
  :stages ((filter (= :city "Bruchsal")) (sort-by :nr) (select :nr))
  :rows ((:nr 1) (:nr 4)))
 (:case "not: NULL is not equal to a value" :from :people
  :stages ((where (not (= :city "Bruchsal"))) (sort-by :nr) (get :nr))
  :values (2 3 5))
 (:case "= nil finds NULL" :from :people
  :stages ((where (= :city nil)) (get :nr))
  :values (2))
 (:case "/= nil finds the others" :from :people
  :stages ((where (/= :city nil)) (sort-by :nr) (get :nr))
  :values (1 3 4 5))
 (:case "in: NULL is in no list without nil" :from :people
  :stages ((where (not (in :city '("Bruchsal")))) (sort-by :nr) (get :nr))
  :values (2 3 5))
 (:case "in: a list with nil finds NULL" :from :people
  :stages ((where (in :city '(nil "Karlsruhe"))) (sort-by :nr) (get :nr))
  :values (2 5))
 (:case "is-null" :from :people
  :stages ((where (is-null :city)) (get :nr))
  :values (2))
 (:case "not-null" :from :people
  :stages ((where (not-null :age)) (sort-by :nr) (get :nr))
  :values (1 2 3 4))
 (:case "in" :from :people
  :stages ((where (in :nr '(2 4 9))) (sort-by :nr) (get :nr))
  :values (2 4))
 (:case "or with unknown" :from :people
  :stages ((where (or (> :age 40) (< :score 3))) (sort-by :nr) (get :nr))
  :values (1 2 4)
  :nushell (:differs "Nushell: null > x gives nothing, and or refuses it (an error)"))
 (:case "and with unknown" :from :people
  :stages ((where (and (> :age 30) (> :score 1))) (sort-by :nr) (get :nr))
  :values (1 4)
  :nushell (:differs "Nushell: null > x gives nothing, and and refuses it (an error)"))
 (:case "numbers by value" :from :people
  :stages ((where (= :score 10)) (get :nr))
  :values (4))
 (:case "column with column" :from :people
  :stages ((where (< :score :nr)) (get :nr))
  :values (5))
 (:case "like folds ASCII" :from :people
  :stages ((where (like :name "B%")) (get :nr))
  :values (2)
  :nushell (:skip "like in Nushell is a regular expression"))
 (:case "like does not fold umlauts" :from :people
  :stages ((where (like :name "ä%")) (get :nr))
  :values ()
  :nushell (:skip "like in Nushell is a regular expression"))
 (:case "like with an umlaut" :from :people
  :stages ((where (like :name "Ä%")) (get :nr))
  :values (5)
  :nushell (:skip "like in Nushell is a regular expression"))
 (:case "like _" :from :people
  :stages ((where (like :name "_nna")) (get :nr))
  :values (1)
  :nushell (:skip "like in Nushell is a regular expression"))

 ;; ------------------------------------------------------------------
 ;; select, reject, get

 (:case "select in its order" :from :people
  :stages ((sort-by :nr) (first 1) (select :name :nr))
  :rows ((:name "Anna" :nr 1)))
 (:case "select unknown column" :from :people
  :stages ((select :nope))
  :error t)
 (:case "reject" :from :people
  :stages ((sort-by :nr) (first 2) (reject :name :city :score))
  :rows ((:nr 1 :age 34) (:nr 2 :age 41)))
 (:case "get" :from :people
  :stages ((sort-by :nr) (where (< :nr 3)) (get :name))
  :values ("Anna" "bert"))

 ;; ------------------------------------------------------------------
 ;; sort-by: NULL last both ways; several columns

 (:case "sort-by, NULL last" :from :people
  :stages ((sort-by :age :nr) (get :nr))
  :values (3 1 2 4 5))
 (:case "sort-by :desc reverses the order: NULL first" :from :people
  :stages ((sort-by :age :desc :nr) (get :nr))
  :values (5 4 2 1 3))
 (:case "sort-by text, by code points" :from :people
  :stages ((sort-by :name) (get :nr))
  :values (1 3 2 5 4))
 (:case "sort-by text :desc" :from :people
  :stages ((sort-by :name :desc) (get :nr))
  :values (4 5 2 3 1))
 (:case "sort-by :desc reverses ties too" :from :people
  :stages ((sort-by :city :desc) (get :nr))
  :values (2 5 3 4 1))
 (:case "sort-by is stable: an earlier sort breaks ties" :from :people
  :stages ((sort-by :nr :desc) (sort-by :age) (get :nr))
  :values (3 1 4 2 5))
 (:case "sort-by direction without column" :from :people
  :stages ((sort-by :desc))
  :error t
  :nushell (:skip "an error before anything runs"))

 ;; ------------------------------------------------------------------
 ;; first, last

 (:case "first n" :from :people
  :stages ((sort-by :nr) (first 2) (get :nr))
  :values (1 2))
 (:case "first alone is the row itself" :from :people
  :stages ((sort-by :nr) (first) (get :nr))
  :value 1)
 (:case "last alone is the row itself" :from :people
  :stages ((sort-by :nr) (last))
  :value (:nr 5 :name "Ärger" :city "Karlsruhe" :age nil :score 3))
 (:case "first of no rows is nil" :from :people
  :stages ((where (= :nr 99)) (first))
  :value nil)
 (:case "head: 10 by default" :from :people
  :stages ((sort-by :nr) (head) (get :nr))
  :values (1 2 3 4 5)
  :nushell (:skip "head is from clsh; Nushell says first"))
 (:case "head n" :from :people
  :stages ((sort-by :nr) (head 2) (get :nr))
  :values (1 2)
  :nushell (:skip "head is from clsh; Nushell says first"))
 (:case "last n" :from :people
  :stages ((sort-by :nr) (last 2) (get :nr))
  :values (4 5))
 (:case "where after last" :from :people
  :stages ((sort-by :nr) (last 3) (where (> :age 30)) (get :nr))
  :values (4))
 (:case "sort-by after first" :from :people
  :stages ((sort-by :nr) (first 3) (sort-by :age :desc) (get :nr))
  :values (2 1 3))

 ;; ------------------------------------------------------------------
 ;; count, columns

 (:case "count" :from :people
  :stages ((where (= :city "Bruchsal")) (count))
  :value 2)
 (:case "count all" :from :people :stages ((count)) :value 5)
 (:case "length is count" :from :people
  :stages ((where (= :city "Bruchsal")) (length))
  :value 2)
 (:case "columns" :from :people
  :stages ((select :nr :name) (columns))
  :value (:nr :name))
 (:case "columns of the table" :from :people :stages ((columns))
  :value (:nr :name :city :age :score))

 ;; ------------------------------------------------------------------
 ;; group-by as in Nushell (group-by --to-table): the key and the rows of
 ;; each group, in the order the keys first appear

 (:case "group-by: the keys, NULL too, as they first appear" :from :people
  :stages ((group-by :city) (get :city))
  :values ("Bruchsal" nil "Heidelsheim" "Karlsruhe"))
 (:case "group-by: the columns" :from :people
  :stages ((group-by :city) (columns))
  :value (:city :items))
 (:case "group-by: the rows of a group" :from :people
  :stages ((group-by :city) (first) (get :items))
  :value ((:nr 1 :name "Anna" :city "Bruchsal" :age 34 :score 2.5)
          (:nr 4 :name nil :city "Bruchsal" :age 41 :score 10.0)))
 (:case "group-by keeps the type of the key" :from :people
  :stages ((group-by :age) (get :age))
  :values (34 41 29 nil)
  :nushell (:differs "Nushell turns the keys into text: \"34\""))

 ;; group-sum, summarize: one row per group with :count and aggregates, as
 ;; in SQL; groups sorted by their columns, NULL last

 (:case "group-sum: count per group" :from :people
  :stages ((group-sum :city) (get :count))
  :values (2 1 1 1)
  :nushell (:skip "group-sum is not in Nushell"))
 (:case "group-sum: the group columns" :from :people
  :stages ((group-sum :city) (get :city))
  :values ("Bruchsal" "Heidelsheim" "Karlsruhe" nil)
  :nushell (:skip "group-sum is not in Nushell"))
 (:case "group-sum: sum leaves NULL out" :from :people
  :stages ((group-sum :city (sum :score)) (get :sum-score))
  :values (12.5 7 3 nil)
  :nushell (:skip "group-sum is not in Nushell"))
 (:case "group-sum: aggregate named with :as" :from :people
  :stages ((group-sum :city (max :age :as :oldest)) (get :oldest))
  :values (41 29 nil 41)
  :nushell (:skip "group-sum is not in Nushell"))
 (:case "group-sum: where on the groups" :from :people
  :stages ((group-sum :city) (where (> :count 1)) (get :city))
  :values ("Bruchsal")
  :nushell (:skip "group-sum is not in Nushell"))
 (:case "group-sum: two columns" :from :people
  :stages ((group-sum :city :age) (get :count))
  :values (1 1 1 1 1)
  :nushell (:skip "group-sum is not in Nushell"))
 (:case "summarize" :from :people
  :stages ((summarize (avg :age)) (get :avg-age))
  :values (36.25)
  :nushell (:skip "Nushell has no summarize"))
 (:case "summarize: count of a column counts values" :from :people
  :stages ((summarize (count :age)) (get :count-age))
  :values (4)
  :nushell (:skip "Nushell has no summarize"))
 (:case "summarize: count of rows" :from :people
  :stages ((summarize (count :age)) (get :count))
  :values (5)
  :nushell (:skip "Nushell has no summarize"))
 (:case "summarize over no rows" :from :people
  :stages ((where (= :nr 99)) (summarize (sum :score)) (get :count))
  :values (0)
  :nushell (:skip "Nushell has no summarize"))
 (:case "group-sum without a column" :from :people
  :stages ((group-sum (sum :score)))
  :error t
  :nushell (:skip "group-sum is not in Nushell"))

 ;; ------------------------------------------------------------------
 ;; join

 (:case "join on all common columns" :from :people
  :stages ((join (table :cities)) (count))
  :value 0
  :nushell (:skip "join in Nushell needs a column"))
 (:case "join :on a column" :from :people
  :stages ((join (table :cities) :on :city) (sort-by :nr) (select :nr :zip))
  :rows ((:nr 1 :zip "76646") (:nr 3 :zip "76646") (:nr 4 :zip "76646") (:nr 5 :zip "76131")))
 (:case "join :on (left right)" :from :people
  :stages ((join (table :cities) :on (:city :city)) (sort-by :nr) (select :nr :zip))
  :rows ((:nr 1 :zip "76646") (:nr 3 :zip "76646") (:nr 4 :zip "76646") (:nr 5 :zip "76131")))
 (:case "join :kind :left" :from :people
  :stages ((join (table :cities) :on :city :kind :left) (sort-by :nr) (select :nr :zip))
  :rows ((:nr 1 :zip "76646") (:nr 2 :zip nil) (:nr 3 :zip "76646") (:nr 4 :zip "76646")
         (:nr 5 :zip "76131")))
 (:case "left join: rows without partner" :from :people
  :stages ((join (table :cities) :on :city :kind :left) (where (is-null :zip)) (get :nr))
  :values (2))
 (:case "join: a name both tables have gets _ on the right" :from :people
  :stages ((join (table :cities) :on :city) (sort-by :nr) (select :nr :name :name_))
  :rows ((:nr 1 :name "Anna" :name_ "Stadt Bruchsal")
         (:nr 3 :name "Carla" :name_ "Heidelsheim (Bruchsal)")
         (:nr 4 :name nil :name_ "Stadt Bruchsal")
         (:nr 5 :name "Ärger" :name_ "Stadt Karlsruhe")))
 (:case "join: the columns" :from :people
  :stages ((join (table :cities) :on :city) (columns))
  :value (:nr :name :city :age :score :zip :name_))
 (:case "join: NULL matches NULL" :from :people
  :stages ((join (table :tags) :on (:city :place)) (sort-by :nr) (get :nr))
  :values (1 2 4))
 (:case "join: the right join column stays" :from :people
  :stages ((join (table :tags) :on (:city :place)) (columns))
  :value (:nr :name :city :age :score :place :tag))
 (:case "join :suffix renames the right join column too" :from :people
  :stages ((join (table :tags) :on (:city :place) :suffix "-t") (columns))
  :value (:nr :name :city :age :score :place-t :tag-t))
 (:case "join :suffix" :from :people
  :stages ((join (table :cities) :on :city :suffix "-c") (sort-by :nr)
           (select :nr :name :name-c :zip-c))
  :rows ((:nr 1 :name "Anna" :name-c "Stadt Bruchsal" :zip-c "76646")
         (:nr 3 :name "Carla" :name-c "Heidelsheim (Bruchsal)" :zip-c "76646")
         (:nr 4 :name nil :name-c "Stadt Bruchsal" :zip-c "76646")
         (:nr 5 :name "Ärger" :name-c "Stadt Karlsruhe" :zip-c "76131")))
 (:case "join :prefix" :from :people
  :stages ((join (table :cities) :on :city :prefix "c-") (where (= :nr 1)) (select :nr :c-name :c-zip))
  :rows ((:nr 1 :c-name "Stadt Bruchsal" :c-zip "76646"))))
