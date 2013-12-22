(ns tailrecursion.boot.gitignore-test
  (:require 
    [clojure.test :refer :all]
    [clojure.java.io :refer [copy file]]
    [tailrecursion.boot.gitignore :as gi]))

(def fix-dir "test/fixtures/")
(->> (doto (file fix-dir ".gitignore") (.deleteOnExit)) (copy (file fix-dir "gitignore")))
(def matches? (gi/make-gitignore-matcher fix-dir))
(defn fixf [path]
  (file (str fix-dir path)))

(deftest test-beginning-slash
  (testing "a beginning slash only matches paths at the root"
    (is (matches? (fixf "boot")))
    (is (not (matches? (fixf "src/boot"))))))

(deftest test-no-beginning-slash
  (testing "no beginning slash matches everywhere"
    (is (matches? (fixf "ignore.txt")))
    (is (matches? (fixf "src/way/down/here/ignore.txt"))))

  (testing "no beginning slash with internal slashes still matches everywhere"
    (is (matches? (fixf ".boot/tmp")))
    (is (matches? (fixf "src/.boot/tmp")))))

(deftest test-ending-slash
  (testing "ending slashes only match directories"
    (is (matches? (fixf "tmpdir")))    
    (is (not (matches? (fixf "test/tmpdir"))))))

(deftest test-wildcard
  (testing "wildcards match"
    (is (matches? (fixf "test.bak")))
    (is (matches? (fixf "hola.mundo.bak")))
    (is (not (matches? (fixf "hola.bak2"))))))

(deftest test-dir-wildcard
  (testing "directory wildcards match 0 or more dirs"
    (is (matches? (fixf "src/what.what")))
    (is (matches? (fixf "src/this/is/the/end/what.what")))
    (is (not (matches? (fixf "what.what"))))))

(deftest test-negation
  (testing "negation prevents matching"
    (is (not (matches? (fixf "src/negated/what.what"))))))

