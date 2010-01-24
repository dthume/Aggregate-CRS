(ns com.dthume.acrs
  (:import
   [java.io File]
   [java.net URI]
   [org.ccil.cowan.tagsoup.jaxp SAXFactoryImpl])
  (:require
   [clojure.xml :as xml]
   [clojure.zip :as zip]
   [clojure.contrib.duck-streams :as ds]
   [clojure.contrib.seq-utils :as su]
   [clojure.contrib.zip-filter.xml :as zfx]
   [clojure.http.client :as http]))

(defn resolve-uri
  "Resolve uri relative to base"
  [base uri]
  (.. (URI. base) (resolve uri) (toString)))

(defn startparse-tagsoup
  "Like clojure.xml/startparse, but uses the TagSoup parser"
  [s ch]
  (.. (SAXFactoryImpl.) (newSAXParser) (parse s ch)))

(defn retrieve-page
  "Retrieve a single page of links to individual CRS'"
  [url]
  (let [page (zip/xml-zip (xml/parse url startparse-tagsoup))]
    (hash-map
     :next-page
     (zfx/xml1-> page
                 :body
                 :div [(zfx/attr= :id "content")]
                 :a [(zfx/text= "Next Page")]
                 (zfx/attr :href))
     :crs
     (zfx/xml-> page
                :body
                :div [(zfx/attr= :id "content")]
                :table :tr :td
                :ul :li :a
                (zfx/attr :href)))))

(defn retrieve-page-list
  "Retrieve a list of URIs for all CRS'"
  [base]
  (loop [uri base
         crs []]
    (let [page (retrieve-page uri)]
      (if-let [next-page (:next-page page)]
        (recur (resolve-uri uri next-page)
               (concat crs (:crs page)))
        (map #(resolve-uri base %) crs)))))

(defn retrieve-proj4js-string
  "Retrieve the proj4js string for a given CRS"
  [uri]
  (let [page (zip/xml-zip (xml/parse uri startparse-tagsoup))
        proj4js-uri (zfx/xml1-> page
                        :body
                        :div [(zfx/attr= :id "content")]
                        :div
                        :ul :li
                        :a [(zfx/text= "Proj4js format")]
                        (zfx/attr :href))]
    (try
     (-> uri
         (resolve-uri proj4js-uri)
         http/request
         :body-seq
         first)
     (catch Exception e (str "Failed to retrieve uri: " uri)))))

(defn valid-proj4j-crs-string
  "Logical true if s is a valid proj4js string, false otherwise"
  [s]
  (not (or (.startsWith s "Failed to retrieve uri:")
           (.endsWith s "] = \"\";"))))

(defn build-proj4js-file-string
  "Combine elements of coll into a single string, suitable for output
to a js file"
  [coll]
  (reduce #(.. %1 (append %2) (append "\n"))
          (StringBuilder.)
          coll))

(defn extract-crs-group
  "Extract the CRS group name (ESRI, EPSG etc.)"
  [s]
  (second (re-matches #"^Proj4js\.defs\[\"(.*?):.*$" s)))

(defn write-proj4j-files
  "Write the CRS' in m to base-dir"
  [base-dir m]
  (let [file-size 1000]
    (doseq [[k v] m]
      (doseq [[i lines] (su/indexed (su/partition-all file-size v))]
        (ds/spit (File. base-dir (str k "-" i ".js"))
                 (build-proj4js-file-string lines))))))

(defn retrieve-proj4js-data
  [base-url output-dir]
  (->> base-url
       retrieve-page-list
       (pmap retrieve-proj4js-string)
       (filter valid-proj4j-crs-string)
       (su/group-by extract-crs-group)
       (write-proj4j-files output-dir)))

;(add-classpath "file:/d:/dthws/acrs/lib/clojure-http-client-1.0.0-20091216.162623-1.jar")
;(add-classpath "file:/d:/tag-soup/1.2/tagsoup-1.2.jar")

;(def *proj4js-strs* (retrieve-proj4js-data))
;(def *valid-crs* (filter #(not (.startsWith % "Failed to retrieve uri:")) *proj4js-strs*))
;(def *valid-proj4js-crs* (filter #(not (.endsWith % "] = \"\";")) *valid-crs*))
;(count *proj4js-strs*)
;(count *valid-crs*)
;(count *valid-proj4js-crs*)
;(ds/spit "d:/proj4js-crs.js" (build-proj4js-file-string (sort *valid-proj4js-crs*)))
;(def *gm* (su/group-by extract-crs-group *valid-proj4js-crs*))
;(write-proj4j-files "d:/proj4js-groups/" *gm*)

;(def *base-url* "http://spatialreference.org/ref/")
;(def *output-dir* "D:/proj4js-groups")
;(time (retrieve-proj4js-data *base-url* *output-dir*))
