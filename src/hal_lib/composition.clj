(ns hal-lib.composition)

(def valid-link-keys #{:href :templated :type :deprecation :name :profile :title :hreflang})

(defn normalize-to-keyword
  [link-name]
  (-> link-name
      (clojure.string/replace " " "-")
      keyword))

(defn create-link
  [link-information link-name]
  {(normalize-to-keyword link-name)
   link-information})

(defn normalize-link-information
  [link-information]
  (if (map? link-information)
    (select-keys link-information valid-link-keys)
    {:href link-information}))

(defn valid-link?
  [link link-name]
  (let [link-keyword (normalize-to-keyword link-name)]
    (and (contains? link link-keyword)
         (contains? (link link-keyword) :href)
         (not (empty? ((link link-keyword) :href))))))

(defn confirm-link-root
  [structure]
  (if (not (contains? structure :_links))
    (conj structure {:_links {}})
    structure))

(defn confirm-embedded-root
  [structure]
  (if (not (contains? structure :_embedded))
    (conj structure {:_embedded {}})
    structure))

(defn confirm-curie-root
  [structure]
  (if (not (contains? (structure :_links) :curies))
    (update-in structure [:_links] conj {:curies []})
    structure))

(defn add-link-to-root
  [structure link]
  (update-in structure [:_links] conj link))

(defn add-embedded-to-root
  [structure embedded-data]
  (update-in structure [:_embedded] conj embedded-data))

(defn add-curie-to-root
  [structure curie-data]
  (update-in structure [:_links :curies] conj curie-data))

(defn add-link
  [link-name link-information structure]
  (let [link (-> link-information
                 (normalize-link-information)
                 (create-link link-name))]
    (if (valid-link? link link-name)
      (-> structure
          (confirm-link-root)
          (add-link-to-root link))
      structure)))

(defn add-self-link
  [link-information structure]
  (add-link "self" link-information structure))

(defn add-next-link
  [link-information structure]
  (add-link "next" link-information structure))

(defn add-previous-link
  [link-information structure]
  (add-link "previous" link-information structure))

(defn add-link-collection
  [link-name collection])

(defn add-embedded
  [embedded-data structure]
  (-> structure
      (confirm-embedded-root)
      (add-embedded-to-root embedded-data)))

(defn add-single-curie
  [curie-data structure]
  (-> structure
      (confirm-link-root)
      (confirm-curie-root)
      (add-curie-to-root curie-data)))

(defn add-curie
  [curie-data structure]
  (if (or (vector? curie-data) (seq? curie-data))
    (loop [[curie & rest] curie-data
           structure' (add-single-curie curie structure)]
      (if (empty? rest)
        structure'
        (recur rest (add-single-curie (first rest) structure'))))
    (add-single-curie curie-data structure)) )
