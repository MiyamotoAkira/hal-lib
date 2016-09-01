(ns haltest.core-test
  (:require [clojure.test :refer :all]
            [haltest.core :refer :all]))

(deftest with-link-to-self
  (testing "Check that we can add a link to self"
    (let [answer (add-self-link "a-ref" {})]
      (is (contains? ((answer :_links) :self) :href)))))

(deftest with-link-to-self-as-map
  (testing "Check that we can add a link to self"
    (let [answer (add-self-link {:href "a-ref"} {})]
      (is (contains? ((answer :_links) :self) :href)))))

(deftest link-map-without-href
  (testing ""
    (let [answer (add-link "self" {:something "another"} {})]
      (is (empty? (answer :_links))))))

(deftest link-with-two-keys
  (testing "We want to check that two valid keys are added into the link"
    (let [answer (add-link "self" {:href "a-ref" :templated "true"} {})]
      (is (contains? ((answer :_links) :self) :href))
      (is (contains? ((answer :_links) :self) :templated)))))

(deftest link-map-with-unneeded-keys
  (testing "Check that a link with unneeded keys doesn't add them to the result"
    (let [answer (add-link "self" {:href "a-ref" :something "another"} {})]
      (is (contains? ((answer :_links) :self) :href))
      (is (not (contains? ((answer :_links) :self) :something))))))

(deftest with-next
  (testing "Check that we can add a link to next"
    (let [answer (add-next-link "an-href" {})]
      (is (contains? ((answer :_links) :next) :href)))))

(deftest with-previous
  (testing "Check that we can add a link to next"
    (let [answer (add-previous-link "an-href" {})]
      (is (contains? ((answer :_links) :previous) :href)))))

(deftest with-random
  (testing "Check that we can add a link to next"
    (let [answer (add-link "item" "an-href" {})]
      (is (contains? ((answer :_links) :item) :href)))))

(deftest with-collection
  (testing "Check that we can add a link to next"
    (let [answer (add-link-collection "items" ["an-href" "another-href" "a-third"] {})]
      (is (contains? (answer :_links) :items))
      (is (vector? ((answer :_links) :items)))
      (is (contains? (((answer :_links) :items) 0) :href)))))

(deftest multiple-links-same-type
  (testing "Check that if you try to add the same link relation it overwrites it"
    (let [second-ref "another ref"
          answer (add-self-link "a-ref" {})
          answer (add-self-link second-ref answer)]
      (is (= (((answer :_links) :self) :href) second-ref)))))

(deftest multiple-links-different-type
  (testing "Check that multiple link additions of different relations are all added"
        (let [answer (add-self-link "a-ref" {})
              answer (add-next-link "another ref" answer)]
          (is (contains? (answer :_links) :self))
          (is (contains? (answer :_links) :next)))))

(deftest with-embedded
  (testing "Check that we add an embedded section"
    (let [answer (add-embedded {:data "data"} {})]
      (is (contains? answer :_embedded))
      (is (contains? (answer :_embedded) :data))
      )))

(deftest multiple-embedded
  (testing "Check that I can add a collection of embedded objects"
    (let [answer (add-embedded {:data "data" :data2 "data2"} {})]
      (is (contains? (answer :_embedded) :data))
      (is (contains? (answer :_embedded) :data2)))))

(deftest add-embedded-after-another
  (testing "Check that we ca add one embedded object after we have already adde done"
    (let [answer (add-embedded {:data "data"} {})
          answer (add-embedded {:data2 "data2"} answer)]
      (is (contains? (answer :_embedded) :data))
      (is (contains? (answer :_embedded) :data2)))))

(defn get-curies
  [structure]
  ((structure :_links) :curies))

(deftest with-curie
  (testing "We can add a curie to our system"
    (let [answer (add-curie {:name "name" :href "a-ref" :templated true} {})]
      (is (some #(= (% :name) "name") (get-curies answer))))))

(deftest with-multiple-curies
  (testing "We can add a curie to our system"
    (let [answer (add-curie [{:name "name" :href "a-ref" :templated true} {:name "another" :href "a-ref" :templated true}] {})]
      (is (some #(= (% :name) "name") (get-curies answer)))
      (is (some #(= (% :name) "another") (get-curies answer))))))

(deftest add-curie-after-another
  (testing "We can add a curie to our system"
    (let [answer (add-curie {:name "name" :href "a-ref" :templated true} {})
          answer (add-curie {:name "another" :href "a-ref" :templated true} answer)]
      (is (some #(= (% :name) "name") (get-curies answer)))
      (is (some #(= (% :name) "another") (get-curies answer))))))

;; (deftest with-links-and-curies
;;   (testing "adding a link with a curie"
;;     (let [answer (add-curie {:name "name" :href "a-ref/{rel}" :templated true} {})
;;           answer (add-link "someref" {:curie "name" :href "myref"} answer)]
;;       (is (= (((answer :_links) :someref) :href))))))
(deftest link-with-curie-but-not-curie)
