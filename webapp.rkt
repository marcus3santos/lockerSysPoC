#lang racket

(require web-server/servlet)
(provide/contract (start (request? . -> . response?)))

(require "model.rkt")

; start: request -> doesn't return
; Consumes a request and produces a page that displays
; all of the web content.
(define (start request)
  (render-admin-page
   (initialize-db!
    (build-path (current-directory-for-user)
                "htdocs/the-db-data.db"))
   request))


; render-blog-page: request -> doesn't return
; Produces an HTML page of the content of the BLOG.
(define (render-admin-page a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Lockers Management System")
                  (link ((rel "stylesheet")
                         (href "/w3.css")
                         (type "text/css"))))
            (body
             (div ((class="w3-container"))
                  (h1 "Lockers Management System")
                  (h2 "Select an option:")
                  (table ((class "w3-table")   )
                         (tr 
                          (td (a ((href  ,(embed/url render-insert-locker-page)) (class "w3-button w3-green"))
                                 "Insert new locker in database") ))
                         (tr
                          (td (a ((href ,(embed/url render-present-lockers-page)) (class "w3-button w3-green"))
                                 "List lockers currently in database") ))
                         (tr
                          (td (a ((href ,(embed/url render-insert-lock-page)) (class "w3-button w3-green"))
                                 "Insert new lock in database") ))
                         (tr
                          (td (a ((href ,(embed/url render-present-locks-page)) (class "w3-button w3-green"))
                                 "List locks currently in database") ))
                         (tr
                          (td (a ((href ,(embed/url render-indicate-locker-problem-page)) (class "w3-button w3-green"))
                                 "Indicate locker as broken/inaccessible/occupied") ))
                         (tr
                          (td (a ((href ,(embed/url render-list-locker-problem-page)) (class "w3-button w3-green"))
                                 "List lockers with problem") ))
                         (tr
                          (td (a ((href ,(embed/url render-lock-assign-page)) (class "w3-button w3-green"))
                                 "Assign lock to locker") ))
                         (tr
                          (td (a ((href ,(embed/url render-signout-locker-page)) (class "w3-button w3-green"))
                                 "Sign out locker") ""))
                         (td (a ((href ,(embed/url render-release-locker-page)) (class "w3-button w3-green"))
                                 "Release locker") "")))))))

  (define (render-insert-locker-page request)
    (render-insert-page a-db 'locker request))

  (define (render-insert-lock-page request)
    (render-insert-page a-db 'lock request))

  (define (render-signout-locker-page request)
    (render-locker-signout a-db request))
  
  (define (render-lock-assign-page request)
    (render-lock-assign a-db request))

  (define (render-present-lockers-page request)
    (render-present-page a-db 'locker request))

  (define (render-present-locks-page request)
    (render-present-page a-db 'lock request))

  (define (render-list-locker-problem-page request)
    (render-list-problem-page a-db request))

  (define (render-indicate-locker-problem-page request)
    (render-indicate-locker-problem a-db request))

  (define (render-release-locker-page request)
    (render-release-locker a-db request))

  
  (send/suspend/dispatch response-generator))


(define (render-insert-page a-db kind request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title ,(string-append "Insert " (kind->string kind)))
                  (link ((rel "stylesheet")
                       (href "/w3.css")
                       (type "text/css"))))
            (body
             (h1 ,(string-append "Insert " (kind->string kind) "Page"))
             (form ((action
                     ,(embed/url (if (eq? kind 'locker)
                                     insert-locker-handler
                                     insert-lock-handler))))
                   (p ,(string-append "Provide " (if (eq? kind 'lock) "lock " "locker ") "number."))
                   (input ((name "number")))
                   (p ,(string-append "Provide " (if (eq? kind 'locker)
                                                     "locker location."
                                                     "lock combination.")))
                   (input ((name ,(if (eq? kind 'locker) "location" "combination"))))
                   (input ((type "submit") (value "Submit"))))
             (p (a ((href ,(embed/url back-handler)) (class "w3-button w3-green"))
                "Back to the Menu"))))))
  
  (define (insert-locker-handler request)
    (insert-locker a-db (request-bindings request) request)
    (render-insert-page a-db 'locker (redirect/get)))
  
  (define (insert-lock-handler request)
    (insert-lock a-db (request-bindings request) request)
    (render-insert-page a-db 'lock (redirect/get)))

  (define (back-handler request)
    (render-admin-page a-db request))

  (send/suspend/dispatch response-generator))



(define (render-release-locker a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Release locker")
                  (link ((rel "stylesheet")
                       (href "/w3.css")
                       (type "text/css"))))
            (body
             (h1 "Release locker")
             (form ((action
                     ,(embed/url release-locker-handler)))
                   (p "Provide the locker number, then click 'Submit'.")
                   (input ((name "locker-number")))
                   (input ((type "submit") (value "Submit")))
                   (p (a ((href ,(embed/url back-handler)) (class "w3-button w3-green"))
                         "Back to the Menu")))))))
  (define (release-locker-handler request)
    (let* ((bindings (request-bindings request))
           (number (extract-binding/single 'locker-number bindings)))
      (release-locker a-db number request))
    (render-release-locker a-db (redirect/get)))

  (define (back-handler request)
    (render-admin-page a-db request))

  (send/suspend/dispatch response-generator))

(define (release-locker a-db locker-n request)
  (if (in-db? 'locker locker-n (db-lockers a-db))
      (let ((a-locker (my-get 'locker locker-n (db-lockers a-db))))
        (cond [(and (not (string=? "" (owner-email (locker-owner a-locker))))
                    (not (string=? "broken/inaccessible/occupied" (locker-note a-locker))))
               (locker-insert-owner! a-db a-locker (owner "" "" "" "" "" ""))
               (render-admin-page a-db request)]
              [else (render-error-page a-db "Locker already is either not occupied, or has issues!" request)]))
      (render-error-page a-db "Locker not in the database!" request))) 

(define (render-locker-signout a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker signout")
                  (link ((rel "stylesheet")
                       (href "/w3.css")
                       (type "text/css"))))
            (body
             (h1 "Sign out locker")
             (form ((action
                     ,(embed/url locker-signout-handler)))
                   (p "Provide the locker number.")
                   (input ((name "locker-number")))
                   (p "Provide locker owner email, then click 'Submit'.")
                   (input ((name "owner-email")))
                   (input ((type "submit") (value "Submit")))
                   (p (a ((href ,(embed/url back-handler)) (class "w3-button w3-green"))
                         "Back to the Menu")))))))
  
  (define (locker-signout-handler request)
    (let* ((bindings (request-bindings request))
           (number (extract-binding/single 'locker-number bindings))
           (owner-email (extract-binding/single 'owner-email bindings)))
      (signout-locker a-db number owner-email request))
    (render-locker-signout a-db (redirect/get)))

  (define (back-handler request)
    (render-admin-page a-db request))

  (send/suspend/dispatch response-generator))

(define (signout-locker a-db locker-n email request)
  (if (in-db? 'locker locker-n (db-lockers a-db))
      (let ((a-locker (my-get 'locker locker-n (db-lockers a-db))))
        (cond [(and (string=? "" (owner-email (locker-owner a-locker)))
                    (string=? (locker-note a-locker) "")
                    (not (string=? (lock-number (locker-lock a-locker)) "-1")))
               (locker-insert-owner! a-db a-locker (owner email "" "" "" "" ""))
               (render-admin-page a-db request)]
              [else (render-error-page a-db "Locker already occupied, has issues, or has no lock assigned to it!" request)]))
      (render-error-page a-db "Locker not in the database!" request)))
      
(define (render-lock-assign a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Lock assign")
                  (link ((rel "stylesheet")
                       (href "/w3.css")
                       (type "text/css"))))
            (body
             (h1 "Assign lock to locker")
             (form ((action
                     ,(embed/url lock-assign-handler)))
                   (p "Provide the lock number.")
                   (input ((name "lock-number")))
                   (p "Provide the locker number, then click 'Submit'.")
                   (input ((name "locker-number")))
                   (input ((type "submit") (value "Submit")))
                   (p (a ((href ,(embed/url back-handler)) (class "w3-button w3-green"))
                         "Back to the Menu")))))))
  
  (define (lock-assign-handler request)
    (let ((bindings (request-bindings request)))
      (assign-lock2locker a-db (extract-binding/single 'locker-number bindings)
                          (extract-binding/single 'lock-number bindings) request))
    (render-lock-assign a-db (redirect/get)))

  (define (back-handler request)
    (render-admin-page a-db request))

  (send/suspend/dispatch response-generator))


(define (assign-lock2locker a-db locker-n lock-n request)
  (cond [(and (in-db? 'locker locker-n (db-lockers a-db))
              (in-db? 'lock lock-n (db-locks a-db)))
         (assign-lock2locker! a-db (my-get 'locker locker-n (db-lockers a-db))
                              (my-get 'lock lock-n (db-locks a-db)))
         (render-admin-page a-db request)]
        [else (render-error-page a-db "Locker/lock number not in database!" request)]))
      
  
(define (render-list-problem-page a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker problem")
                  (link ((rel "stylesheet")
                       (href "/w3.css")
                       (type "text/css"))))
            (body
             (h1 "Lockers with problem")
             (table ((class "w3-table"))
              (tr (th "Number") (th "Location") (th "Lock") (th "Owner") (th "Status"))
             ,(render-list 'locker 'not-ok a-db))
             (p (a ((href ,(embed/url back-handler)) (class "w3-button w3-green"))
                "Back to the Menu"))))))

  (define (back-handler request)
    (render-admin-page a-db request))

  (send/suspend/dispatch response-generator))

  
(define (render-indicate-locker-problem a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Indicate")
                  (link ((rel "stylesheet")
                       (href "/w3.css")
                       (type "text/css"))))
            (body
             (h1 "Problem with locker")
             (p "If your locker is either broken, inaccessible, or occupied, then please provide us the locker number and click on 'Submit'.")
             (form ((action
                     ,(embed/url indicate-locker-problem-handler)))
                     (input ((name "number")))
                     (input ((type "submit") (value "Submit")))))
            (p (a ((href ,(embed/url back-handler)) (class "w3-button w3-green"))
                  "Back to the Menu")))))

  (define (indicate-locker-problem-handler request)
    (indicate-locker-problem a-db (request-bindings request) request)
    (render-indicate-locker-problem (redirect/get)))

  (define (back-handler request)
    (render-admin-page a-db request))

  (send/suspend/dispatch response-generator))


(define (indicate-locker-problem a-db bindings request)
  (let* ((l-number (extract-binding/single 'number bindings))
         (l-list (db-lockers a-db)))
    (cond [(in-db? 'locker l-number l-list)
           (locker-insert-note! a-db (my-get 'locker l-number (db-lockers a-db)) "broken/inaccessible/occupied")
           (render-admin-page a-db request)]
          [else (render-error-page a-db "Locker number not in database!" request)])))
                           

(define (render-present-page a-db kind request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title ,(kind->string kind))
                  (link ((rel "stylesheet")
                       (href "/w3.css")
                       (type "text/css"))))
            (body
             (h1 ,(string-append (if (eq? kind 'locker) "Lockers " "Locks ") "currently in database"))
             (table ((class "w3-table"))
              ,(cond [(eq? kind 'locker)
                     `(tr (th "Number") (th "Location") (th "Lock") (th "Owner") (th "Status"))]
                    [else
                     `(tr (th "Number") (th "Combination"))])
               ,(render-list kind 'ok a-db))
             (p (a ((href ,(embed/url back-handler)) (class "w3-button w3-green"))
                "Back to the Menu"))))))

  (define (back-handler request)
    (render-admin-page a-db request))

  (send/suspend/dispatch response-generator))

(define (render-list kind flag a-db)
  (cond [(eq? flag 'ok)
         `(div ((class "test"))
               ,@(map (if (eq? kind 'locker) render-locker render-lock)
                    (if (eq? kind 'locker) (db-lockers a-db) (db-locks a-db))))]
        [else
         `(div ((class "test"))
               ,@(map render-locker (get-defective-lockers (db-lockers a-db))))]))
      

(define (get-defective-lockers l-list)
  (cond ((null? l-list) '())
        ((string=? "broken/inaccessible/occupied" (locker-note (car l-list)))
         (cons (car l-list) (get-defective-lockers (cdr l-list))))
        (else (get-defective-lockers (cdr l-list)))))
  
(define (render-locker a-locker)
  `(tr
    (td ,(locker-number a-locker))
    (td ,(locker-location a-locker))
    (td ,(if (string=? (lock-number (locker-lock a-locker)) "-1")
              "None"
              (lock-number (locker-lock a-locker))))
    (td ,(if (string=? (owner-email (locker-owner a-locker)) "")
              "None"
              (owner-email (locker-owner a-locker))))
    (td , (if (string=? (locker-note a-locker) "broken/inaccessible/occupied")
              "broken/inaccessible/occupied"
              "OK"))))

(define (render-lock a-lock)
  `(tr 
    (td ,(lock-number a-lock))
    (td ,(lock-combination a-lock))))

(define (can-parse-info? kind bindings)
  (or (and (eq? kind 'locker)
           (exists-binding? 'number bindings)
           (exists-binding? 'location bindings))
      (and (eq? kind 'lock)
           (exists-binding? 'number bindings)
           (exists-binding? 'combination bindings))))




;; For inserting data in databse

(define (insert-locker a-db bindings request)
  (if (can-parse-info? 'locker bindings)
      (insert-locker! (locker (extract-binding/single 'number bindings)
                              (lock "-1" "" "" "")
                              (extract-binding/single 'location bindings)
                              (owner "" "" "" "" "" "")
                              "")
                      request
                      a-db)
      (render-error-page a-db "Incorrect/Incomplete information!" request)))


(define (insert-locker! a-locker request a-db)
  (cond [(in-db? 'locker (locker-number a-locker) (db-lockers a-db))
         (render-error-page a-db "Locker number already in database!" request)]
        [else (insert-new-locker/lock! a-db 'locker a-locker)
              (render-admin-page a-db request)]))

(define (insert-lock! a-lock request a-db)
  (cond [(in-db? 'lock (lock-number a-lock) (db-locks a-db))
         (render-error-page a-db "Lock number already in database!" request)]
        [else (insert-new-locker/lock! a-db 'lock a-lock) 
              (render-admin-page a-db request)]))

(define (insert-lock a-db  bindings request)
  (if (can-parse-info? 'lock bindings)
      (insert-lock! (lock (extract-binding/single 'number bindings)
                          (extract-binding/single 'combination bindings)
                           ""
                           "")
                    request
                    a-db)
      (render-error-page a-db "Incorrect/Incomplete information!" request)))


;; tools web page handling


(define (render-error-page a-db error-msg request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Error page")
                  (link ((rel "stylesheet")
                       (href "/w3.css")
                       (type "text/css"))))
            (body
             (h1 "ERROR")
             (p ,error-msg)
             (p (a ((href ,(embed/url back-handler)) (class "w3-button w3-green"))
                "Back to the Menu"))))))

  (define (back-handler request)
    (render-admin-page a-db request))

  (send/suspend/dispatch response-generator))

(require web-server/servlet-env)

(serve/servlet start
               #:port 8080
               #:listen-ip #f
               #:servlet-path "/main"
               #:extra-files-paths
               (list
                (build-path (current-directory-for-user) "htdocs")))
