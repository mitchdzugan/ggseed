(ns globals)

(def client-id 13)

(def auth-url
  (str "http://start.gg/api/-/rest/oauth/authorize?response_type=code"
       "&client_id=" client-id "&scope=user.identity%20tournament.manager"
       "&redirect_uri=https%3A%2F%2Fgg-seed-upload.herokuapp.com%2Foauth"))

(def redirect-uri "https://gg-seed-upload.herokuapp.com/oauth")

(def scope "user.identity tournament.manager")
