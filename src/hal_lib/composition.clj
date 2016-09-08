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

(defn add-link-to-root
  [structure link]
  (update-in structure [:_links] conj link))

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
  [embedded-data structure])

(defn add-curie
  [curie-data structure])
