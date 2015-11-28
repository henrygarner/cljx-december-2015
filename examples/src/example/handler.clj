(ns example.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.util.response :refer [response]]))

(defroutes app-routes
  (GET "/echo" [& params]
    (response "hello"))
  (route/not-found "Not Found"))

(def app
  app-routes)
